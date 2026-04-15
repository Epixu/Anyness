///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Heap-Reference.hpp"


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Interfaces a heap. Adds a member that points to the heap memory.       
   /// The heap is allowed to move on reallocation.                           
   ///   @tparam ID heap provider ID. Must have a matching type component ID. 
   ///   @tparam INITIAL_SIZE the initial size (in elements). Used in hashed  
   ///      containers in order to control hash table size. If 0, the heap    
   ///      will use reflected type properties only.                          
   ///   @tparam GROWTH_FACTOR growth factor on reallocation. Used in hashed  
   ///      containers in order to control hash table growth on reallocation. 
   ///      If 0, the heap will grow according to reflected type properties.  
   ///   @tparam ENTRIES optional extensions that include more data into      
   ///      the heap allocation. Each ID must correspond to a matching type   
   ///      component ID. Each entry also allows for pointer customization,   
   ///      including support for packed pointers.                            
   template<Cid ID, uint INITIAL_SIZE, uint GROWTH_FACTOR, CT::HeapEntry...ENTRIES>
   struct HeapMovable : HeapReference<ID, ENTRIES...> {
      //static constexpr Cid  Id = ID;
      //static constexpr Cid  HeapProvider = ID;
      //static constexpr int  ComponentPrecedence = -2000;
      //static constexpr bool HeapCanBeNull = true;
      static constexpr uint InitialSize = INITIAL_SIZE;
      static constexpr uint GrowthFactor = GROWTH_FACTOR;

   protected:
      LglsComIterationOperators(friend);
      LglsComReserveEmergent(friend);
      LglsComInsertion(friend);
      LglsComMerging(friend);
      LglsComEmplacement(friend);
      LglsComConversion(friend);
      LglsComOwnershipEmergent(friend);

      template<CT::Container C>
      using Count = typename Deref<C>::CountType;

      template<CT::Container C>
      static constexpr auto CountMax = ::std::numeric_limits<Count<C>>::max();

      using Base = HeapReference<ID, ENTRIES...>;
      using typename Base::Request;
      
      /// Default-initialize the heap pointer                                 
      constexpr void ConstructDefault(this auto& self) noexcept {
         self.SetHeapInner(nullptr);
      }
      
      /// Transfer from any kind of container, respecting intents             
      ///   @param intent the intent and container to transfer from           
      template<CT::Container C, CT::Intent I> requires CT::Container<I>
      void ConstructFrom(this C& self, I&& intent) {
         using IT = Deint<I>;
         IT from = LglsFwd(intent.what);

         if constexpr (CT::Copied<I> or CT::Cloned<I>) {
            // Do a copy or clone.                                      
            // When copying, we're cloning just the first layer, so we  
            // guarantee that data is no longer static and constant at  
            // the first level of indirection.                          
            auto type = from.template GetType<ID>().GetDecvq();
            self.template SetType<ID>(type);
            auto count = from.template GetCount<ID>();
            if (0 == count) {
               self.template SetAllocationInner<ID>(nullptr);
               if_available(self.template SetReservedInner<ID>(0)); //TODO redundant?
               if_available(self.template SetHashTableInner<ID>(nullptr));
               self.template ResetCount<ID>();
               return;
            }

            // Pick a preferably typed block to optimize                
            if constexpr (CT::TypeErased<IT>) {
               // A runtime check is required before allocating         
               if constexpr (CT::Copied<I>) {
                  LglsAssert(type.GetReferConstructor(),
                     "Can't refer-construct elements"
                     " - no refer-constructor was reflected for type ",
                     type
                  );
               }
               else {
                  LglsAssert(type.GetCloneConstructor(),
                     "Can't clone-construct elements"
                     " - no clone-constructor was reflected for type ",
                     type
                  );
               }
            }
            else if constexpr (CT::Copied<I>) {
               static_assert(CT::ReferConstructible<TypeOf<IT>>,
                  "Contained type is not refer-constructible");
            }
            else {
               static_assert(CT::CloneConstructible<TypeOf<IT>>,
                  "Contained type is not clone-constructible");
            }

            // Allocate new memory and set count, so that handle        
            // iteration is valid                                       
            if constexpr (CT::Contiguous<C>)
               self.AllocateFresh(self.RequestHeap(count));
            else
               self.AllocateFresh(self.RequestHeap(from.template GetReserved<ID>()));

            if_available(self.template SetCountInner<ID>(count));
            auto dst = self.template GetHandle<void, ID>();
            try {
               from.Apply([&dst,&self,&from](auto const& src) {
                  (void) self; (void) from;
                  if constexpr (CT::Supported<decltype(src)>) {
                     if constexpr (CT::Copied<I>)
                        dst.ForceMutable().EmplaceWithIntent(Refer(src));
                     else
                        dst.ForceMutable().EmplaceWithIntent(Clone(src));

                     if constexpr (not CT::Contiguous<C>) {
                        // Copy hash table entry as well                
                        const auto idx = dst - self.template GetHandle<void, ID>();
                        self.template GetHashTableInner<ID>()[idx] = from.template GetHashTableInner<ID>()[idx];
                     }
                  }
                  ++dst;
               });
            }
            catch (...) {
               // Partial success                                       
               auto n = dst - self.template GetHandle<void, ID>();
               if constexpr (not requires { self.template SetCountInner<ID>(1); }) {
                  // Partial success is not allowed - we have to        
                  // destroy everything we initialized                  
                  while (n) {
                     dst.DestroyElement();
                     --dst;
                     --n;
                  }
               }
               self.template PartialSuccess<ID>(n);
               throw;
            }
                     
            // Full success                                             
            if constexpr (requires { from.template GetHashInner<ID>(); }) {
                    if_available(self.template SetHashInner<ID>(from.template GetHashInner<ID>()))
               else if_available(self.template ResetHash<ID>())
            }
         }
         else {
            // Move/Refer/Abandon/Disown other                          
            static_assert(I::IsShallow());
            self.template SetType<ID>(from.template GetType<ID>());
            self.template SetHeapInner<ID>(from.template GetHeapInner<ID>());

            if constexpr (I::IsKept()) {
               if constexpr (I::IsMoved()) {
                  // Move                                               
                  if constexpr (CT::AutoOwned<I>) {
                     from.template SetHeapInner<ID>(nullptr);
                     if_available(from.template ResetState<ID>());
                     if_available(from.template ResetType<ID>());
                  }
               }
               else static_assert(CT::Referred<I>);
            }
         }
      }

      /// Free this container and absorb from any other, respecting intents   
      ///   @param intent the intent and container to assign from             
      template<class C, CT::Intent I> requires CT::Container<I>
      auto AssignFrom(this C& self, I&& intent) -> C& {
         using IT = Deint<I>;
         IT from = LglsFwd(intent.what);

         if constexpr (requires { &self == &from; }) {
            // Make sure 'lhs' and 'rhs' are different instances,       
            // otherwise we lose rhs if we free lhs                     
            if (&self == &from)
               return self;
         }

         // Never modify containers if type-incompatible                
         if constexpr (CT::TypeErased<IT> or CT::TypeErased<C>) {
            auto t1 = self.GetType();
            auto t2 = from.GetType();
            if (t1 and t2) {
               LglsAssert(t1.IsSame(t2), "Type mismatch", ": ",
                  t1, " is not same as ", t2);
            }
         }
         else static_assert(Same<TypeOf<C>, TypeOf<IT>>, "Type mismatch");

         // Free old data and absorb the new container                  
         self.Free();
         self.ResetCount();
         self.Absorb(LglsFwd(intent));
         return self;
      }
      
      /// Allocate a fresh allocation                                         
      ///   @attention changes allocation, heap pointer and reserve count only
      ///   @param request request to fulfill                                 
      template<Cid SID = ID, CT::Container C>
      auto AllocateFresh(this C& self, const Request& request) -> Allocation* {
         static_assert(SID == ID or ((SID == ENTRIES::Id) or ...));
         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            auto al = Allocator::Allocate(self.template GetType<SID>(), request.mTotalBytes);
         #else
            auto al = Allocator::Allocate(self.template GetAlignment<SID>(), request.mTotalBytes);
         #endif
         LglsAssert(al, "Out of memory");
         
         self.template SetHeapInner<SID>(static_cast<void*>(al->GetBlockStart() + request.mHeaderBytes));
         self.template SetAllocationInner<SID>(al);
         if_available(self.template SetReservedInner<SID>(request.mReserved));
         self.ConstructHeapDefault();
         return al;
      }

      /// Allocate a number of elements, relying on the type of the container 
      ///   @attention assumes container is typed                             
      ///   @param elements number of elements to allocate                    
      template<Cid SID = ID, CT::Container C>
      void AllocateMore(this C& self, Count<C> elements) {
         static_assert(SID == ID or ((SID == ENTRIES::Id) or ...));
         if constexpr (InitialSize and GrowthFactor) {
            // We override allocation size with predefined parameters,  
            // if such are defined                                      
            if (elements <= InitialSize)
               elements = InitialSize;
            else {
               Count<C> growth = InitialSize;
               while (elements > InitialSize + growth)
                  growth *= GrowthFactor;
               elements = InitialSize + growth;
               //TODO when pagefile size is reached, start growing linearly by pagefile-sized intervals. this way we minimize cache misses in huge hash tables
            }
         }

         LglsAssumeDev(elements > self.template GetCount<SID>(), "Bad element count");
         if constexpr (CT::ContainsOne<C>)
            LglsAssumeDev(elements == 1, "Container allows only one allocated element");
         const auto al = DecvqAllCast(self.template GetAllocation<SID>());
         const auto request = self.template RequestHeap<SID>(elements);

         if (not al) {
            //                                                          
            // Allocate a fresh set of elements                         
            self.template AllocateFresh<SID>(request);
            return;
         }

         LglsAssumeDev(al->GetUses() == 1,
            "Can't reuse memory of a heap used from multiple places. "
            "Container should've branched-out prior to AllocateMore. "
         );

         if constexpr (CT::ContainsMany<C>) {
            if (self.template GetReserved<SID>() >= elements) {
               // Required memory is already available                  
               return;
            }

            //                                                          
            // Reallocate                                               
            C previous {Abandon {self}};
            #if LANGULUS_FEATURE(MANAGED_MEMORY)
               auto reallocated = Allocator::Reallocate(self.template GetType<SID>(), request.mTotalBytes, al);
            #else
               auto reallocated = Allocator::Reallocate(request.mTotalBytes, al);
            #endif
            LglsAssert(reallocated, "Out of memory");

            self.template SetAllocationInner<SID>(reallocated);

            if (reallocated != al) {
               self.template SetHeapInner<SID>(static_cast<void*>(reallocated->GetBlockStart() + request.mHeaderBytes));

               if (previous.GetCount()) {
                  // Memory moved, and we should move all elements with 
                  // it. We're moving to new memory, so no reverse is   
                  // required.                                          
                  auto to = self.GetHandle();
                  try {
                     previous.Apply([&to,&self,&previous](auto&& from) {
                        (void) self; (void) previous;
                        if constexpr (CT::Supported<decltype(from)>) {
                           to.ForceMutable().EmplaceWithIntent(Abandon(from));

                           if constexpr (not CT::Contiguous<C>) {
                              // Copy hash table entry as well          
                              const auto idx = to - self.GetHandle();
                              self.GetHashTableInner()[idx] = previous.GetHashTableInner()[idx];
                           }
                        }
                        ++to;
                     });
                  }
                  catch (...) {
                     self.template SetCountInner<SID>(to - self.GetHandle());
                     throw;
                  }
               }
            }
            else {
               // Memory didn't move, but reserved count changed so all 
               // HeapRequests which are PerElement need to be moved    
               // around.                                               
               if_available(self.template RemapHeapRequests<SID>(request.mReserved));
               previous.template SetAllocationInner<SID>(nullptr);
            }

            if_available(self.template SetReservedInner<SID>(request.mReserved));
         }
      }

      /// Shrink the block, depending on currently reserved	elements.         
      /// Initialized elements on the back will be destroyed.                 
      /// When MANAGED_MEMORY is enabled we have a strong guarantee that      
      /// allocations never move when shrinking.                              
      ///   @param desiredReserve number of elements to reserve               
      template<Cid SID = ID, CT::Container C>
      void AllocateLess(this C& self, const Count<C> desiredReserve) {
         static_assert(SID == ID or ((SID == ENTRIES::Id) or ...));
         static_assert(CT::ContainsMany<C>,
            "This makes sense to be called only by containers that support many elements");

         const auto al = DecvqAllCast(self.template GetAllocation<SID>());
         if (not al) {
            //                                                          
            // We have to branch out                                    
            const C backup{Abandon{self}};
            self.template AllocateFresh<SID>(self.template RequestHeap<SID>(desiredReserve));

            // Reinsert only the relevant items                         
            auto to = self.GetHandle();
            try {
               backup.Apply([&to](auto& from) {
                  to.EmplaceWithIntent(Refer(from));
                  ++to;
               });
            }
            catch (...) {
               self.template SetCountInner<SID>(to - self.GetHandle());
               throw;
            }

            if constexpr (CT::Contiguous<C>) {
               self.template SetCountInner<SID>(backup.template GetCount<SID>() < desiredReserve
                  ? backup.template GetCount<SID>()
                  : desiredReserve
               );
            }
            else self.template SetCountInner<SID>(backup.template GetCount<SID>());
            return;
         }

         LglsAssumeDev(desiredReserve <= self.template GetReserved<SID>(),
            "Can't shrink allocation using more elements");
         LglsAssumeDev(al->GetUses() == 1,
            "Can't reuse memory of a block used from multiple places");

         if (self.template GetCount<SID>() > desiredReserve) {
            auto temp = self.SelectInner(desiredReserve);
            temp.template DestroyAllElements<SID>();
            if_available(self.template SetCountInner<SID>(desiredReserve));
         }

         const auto request = self.template RequestHeap<SID>(desiredReserve);
         if (request.mTotalBytes == al->GetSize())
            return;

         // Memory doesn't move, but reserved count changed so all      
         // HeapRequests which are PerElement need to be moved around.  
         if_available(self.template RemapHeapRequests<SID>(request.mReserved));

         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            self.template SetAllocationInner<SID>(
               Allocator::Reallocate(self.template GetType<SID>(), request.mTotalBytes, al)
            );
         #else
            self.template SetAllocationInner<SID>(
               Allocator::Reallocate(request.mTotalBytes, al)
            );
         #endif

         if_available(self.template SetReservedInner<SID>(request.mReserved));
      }

      /// Remap footer requests onto the new reserve                          
      ///   @param newReserved the newly reserved number of elements          
      template<Cid SID = ID, CT::Container C> requires (C::CountHeapFooterRequests() > 0)
      void RemapHeapRequests(this C& self, const Count<C> newReserved) {
         static_assert(SID == ID or ((SID == ENTRIES::Id) or ...));
         const auto size     = self.template GetStride<SID>();
         const auto reserved = self.template GetReserved<SID>();
         const auto indirect = self.template GetIndirections<SID>();

         size_t from[C::CountHeapFooterRequests() + 1];
         size_t to  [C::CountHeapFooterRequests() + 1];
         size_t idx = 1;
         from[0] = to[0] = 0;
         
         C::ComponentList::ForEach([&]<class COM>{
            if constexpr (requires { typename COM::HeapRequest; }) {
               if constexpr (COM::Id == SID) {
                  using R = typename COM::HeapRequest;
                  if constexpr (requires { R::AllocatedPerIndirection; }) {
                     if constexpr (requires { R::Type::AllocatedPerElement; }) {
                        const size_t shift = sizeof(typename R::Type::Type) * indirect;
                        from[idx] = from[idx-1] + shift * reserved;
                        to  [idx] = to  [idx-1] + shift * newReserved;
                     }
                     else {
                        const size_t shift = sizeof(typename R::Type) * indirect;
                        from[idx] = from[idx-1] + shift;
                        to  [idx] = to  [idx-1] + shift;
                     }
                  
                     ++idx;
                  }
                  else if constexpr (requires { R::AllocatedPerElement; }) {
                     size_t shift;
                     if constexpr (requires { R::Type::AllocatedPerIndirection; })
                        shift = sizeof(typename R::Type::Type) * indirect;
                     else
                        shift = sizeof(typename R::Type);
                  
                     from[idx] = from[idx-1] + shift * reserved;
                     to  [idx] = to  [idx-1] + shift * newReserved;

                     ++idx;
                  }
               }
            }            
         });

         const auto footer = self.template GetAllocation<SID>()->GetBlockStart()
                           + self.template GetHeapHeaderSize<SID>();
         const auto to_footer = footer + newReserved * size;
         const auto from_footer = footer + reserved * size;

         --idx;

         if (newReserved > reserved) {
            // When newReserved is larger than reserved, stuff has to   
            // move left to right, so it must be done in reverse so that
            // we don't destroy any data. The newly formed gaps need    
            // to be filled with zeroes.                                
            while (idx) {
               --idx;
               const auto range = from[idx + 1] - from[idx];
               memmove(to_footer + to[idx], from_footer + from[idx], range);
               memset (to_footer + to[idx] + range, 0, to[idx + 1] - to[idx] - range);
            }
         }
         else {
            // When newReserved is smaller than reserved, stuff has to  
            // move right to left. No gaps will be formed.              
            for (size_t i = 0; i < idx; ++i) {
               memmove(to_footer + to[i], from_footer + from[i], from[i + 1] - from[i]);
            }
         }
      }

      /// Invoked to remedy the situation when element constructors throw     
      ///   @param n the number of elements that were actually initialized    
      template<Cid SID = ID, CT::Container C>
      void PartialSuccess(this C& self, Count<C> n) {
         static_assert(SID == ID or ((SID == ENTRIES::Id) or ...));
         if constexpr (requires { self.template SetCountInner<SID>(1); }) {
            // Partial success is supported                             
            self.template SetCountInner<SID>(n);
            self.template ResetHash<SID>(); // If n == 0, hash is 1; 0 otherwise      

            if constexpr (not CT::Contiguous<C>) {
               // Partial success involving maps/sets might result in   
               // gaps in the hash table that need to be accounted for. 
               self.template ShiftEntries<SID>();
            }
         }
         else {
            // Partial success is not allowed - we have to deallocate   
            // and make sure CountStatic reports as empty.              
            (void) n;
            Allocator::Deallocate(DecvqAllCast(self.template GetAllocationInner<SID>()));
            self.template SetAllocationInner<SID>(nullptr);
            if_available(self.template SetReservedInner<SID>(0));
            if_available(self.template SetHashTableInner<SID>(nullptr));
            self.template ResetCount<SID>();
         }
      }

      /// Branch out the current container by doing a shallow copy.           
      /// Happens when you try to modify a container with strong ownership    
      /// from somewhere else (when GetUses() > 1)                            
      ///   @param newReserve usually branching is accompanied by a resize,   
      ///      so specify it here                                             
      template<CT::Container C>
      void BranchOut(this C& self, Count<C> newReserve) {
         if (self.GetUses() > 1) {
            // We have to branch out                                    
            const C backup {Abandon{self}};
            self.AllocateFresh(self.RequestHeap(newReserve));

            // Reinsert the old items                                   
            auto to = self.GetHandle();
            try {
               backup.Apply([&to](auto const& from) {
                  if constexpr (CT::Supported<decltype(from)>)
                     to.ForceMutable().EmplaceWithIntent(Refer(from));
                  ++to;
               });
            }
            catch (...) {
               self.SetCountInner(to - self.GetHandle());
               throw;
            }

            self.SetCountInner(backup.GetCount());
         }
         else self.AllocateMore(newReserve);
      }
   };
}
