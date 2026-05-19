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
   /// Refers back to this particular component instance through the deduced  
   /// 'this'. Just for convenience. It is #undef-ed at the end of this file. 
   #define ThisCom self.HeapMovable<INITIAL_SIZE, GROWTH_FACTOR, ENTRY0, ENTRYN...>

   ///                                                                        
   /// Interfaces a heap. Adds a member that points to the heap memory.       
   /// The heap is allowed to move on reallocation.                           
   ///   @tparam INITIAL_SIZE the initial size (in elements). Used in hashed  
   ///      containers in order to control hash table size. If 0, the heap    
   ///      will use reflected type properties only.                          
   ///   @tparam GROWTH_FACTOR growth factor on reallocation. Used in hashed  
   ///      containers in order to control hash table growth on reallocation. 
   ///      If 0, the heap will grow according to reflected type properties.  
   ///   @tparam ENTRY0 the first entry                                       
   ///   @tparam ENTRYN optional extensions that include more data into       
   ///      the heap allocation. Each ID must correspond to a matching type   
   ///      component ID. Each entry also allows for pointer customization,   
   ///      including support for packed pointers.                            
   template<uint INITIAL_SIZE, uint GROWTH_FACTOR, CT::HeapEntry ENTRY0, CT::HeapEntry...ENTRYN>
   struct HeapMovable : HeapReference<ENTRY0, ENTRYN...> {
      using Id = typename HeapReference<ENTRY0, ENTRYN...>::Id;
      using HeapProvider = Id;

      static constexpr uint InitialSize  = INITIAL_SIZE;
      static constexpr uint GrowthFactor = GROWTH_FACTOR;
      template<Cid SID>
      static constexpr bool Relevant = Id::template Contains<SID>;

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

      using Base = HeapReference<ENTRY0, ENTRYN...>;
      using typename Base::Request;
      
      /// Default-initialize the heap pointer                                 
      constexpr void ConstructDefault(this auto& self) noexcept {
         ThisCom::SetHeapInner(nullptr);
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
            //self.template SetType<Id::First>(type);
            size_t count, reserve;

            // Verify that all dimensions are copiable/clonable, and    
            // make sure that 'count' and 'reserve' are consistent      
            // across all dimensions.                                   
            Id::ForEach([&]<Cid D> {
               #if LANGULUS(SAFE)
                  count = from.template GetCount<D>();
                  LglsAssert(count == from.template GetCount<Id::First>(),
                     "Inconsistent count across dimensions");

                  reserve = from.template GetReserved<D>();
                  LglsAssert(reserve == from.template GetReserved<Id::First>(),
                     "Inconsistent reserve across dimensions");
               #endif

               if constexpr (CT::TypeErased<IT>) {
                  auto type = self.template GetType<D>();
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
               else {
                  if constexpr (CT::Copied<I>) {
                     static_assert(CT::ReferConstructible<TypeOf<C, D>>,
                        "Contained type is not refer-constructible");
                  }
                  else {
                     static_assert(CT::CloneConstructible<TypeOf<C, D>>,
                        "Contained type is not clone-constructible");
                  }
               }
            });

            if (0 == count) {
               self.template ResetAllocationInner<Id::First>();
               return;
            }

            #if not LANGULUS(SAFE)
               count = from.template GetCount<Id::First>();
               reserve = from.template GetReserved<Id::First>();
            #endif

            // Allocate new memory and set count, so that handle        
            // iteration is valid                                       
            if constexpr (CT::Contiguous<C>)
               ThisCom::AllocateFresh(ThisCom::RequestHeap(count));
            else
               ThisCom::AllocateFresh(ThisCom::RequestHeap(reserve));

            if_available(self.template SetCountInner<Id::First>(count));
            auto dst = self.GetHandle().ForceMutable();
            try {
               from.template Apply<false>([&dst,&self,&from](auto const& src) {
                  (void) self; (void) from;

                  if constexpr (CT::Supported<decltype(src)>) {
                     Id::ForEach([&dst,&src]<Cid D>{
                        if constexpr (CT::Copied<I>)
                           dst.template EmplaceWithIntent<D>(Refer(src));
                        else
                           dst.template EmplaceWithIntent<D>(Clone(src));
                     });

                     if constexpr (not CT::Contiguous<C>) {
                        // Copy hash table entry as well                
                        const auto idx = dst - self.GetHandle();
                        self.template GetHashTableInner<Id::First>()[idx]
                            = from.template GetHashTableInner<Id::First>()[idx];
                     }
                  }
                  ++dst;
               });
            }
            catch (...) {
               // Partial success                                       
               auto n = dst - self.GetHandle();
               if constexpr (not requires { self.template SetCountInner<Id::First>(1); }) {
                  // Partial success is not allowed - we have to        
                  // destroy everything we initialized                  
                  while (n) {
                     Id::ForEach([&dst]<Cid D>{
                        dst.template DestroyElement<true, D>();
                     });
                     --dst;
                     --n;
                  }
               }
               ThisCom::PartialSuccess(n);
               throw;
            }
                     
            // Full success                                             
            if constexpr (requires { from.template GetHashInner<Id::First>(); }) {
                    if_available(self.template SetHashInner<Id::First>(from.template GetHashInner<Id::First>()))
               else if_available(self.template ResetHash<Id::First>())
            }
         }
         else {
            // Move/Refer/Abandon/Disown other                          
            //static_assert(I::IsShallow());
            //self.template SetType<Id::First>(from.template GetType<Id::First>());
            ThisCom::SetHeapInner(from.template GetRaw<Id::First>());

            if constexpr (I::IsKept()) {
               if constexpr (I::IsMoved()) {
                  // Move                                               
                  if constexpr (CT::StronglyOwned<I>) {
                     from.template SetHeapInner<Id::First>(nullptr);
                     if_available(from.ResetState());
                     if_available(from.template ResetType<Id::First>());
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
            // otherwise we lose rhs if we free lhs, and we have to     
            // free lhs in order to overwrite it with rhs.              
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
      template<Cid SID = Id::First, CT::Container C> requires Relevant<SID>
      void AllocateFresh(this C& self, const Request& request) {
         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            auto al = Allocator::Allocate(self.template GetType<SID>(), request.mTotalBytes);
         #else
            auto al = Allocator::Allocate(self.template GetAlignment<SID>(), request.mTotalBytes);
         #endif
         LglsAssert(al, "Out of memory");
         
         ThisCom::SetHeapInner(static_cast<void*>(al->GetBlockStart() + request.mHeaderBytes));
         self.template SetAllocationInner<SID>(al);
         if_available(self.template SetReservedInner<SID>(request.mReserved));
         if_available(self.template ConstructHeapRequest<SID>());
      }

      /// Allocate a number of elements, relying on the type of the container 
      ///   @attention assumes container is typed                             
      ///   @param elements number of elements to allocate                    
      template<Cid SID = Id::First, CT::Container C> requires Relevant<SID>
      void AllocateMore(this C& self, Count<C> elements) {
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
         const auto request = ThisCom::RequestHeap(elements);

         if (not al) {
            //                                                          
            // Allocate a fresh set of elements                         
            ThisCom::AllocateFresh(request);
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
               ThisCom::SetHeapInner(static_cast<void*>(reallocated->GetBlockStart() + request.mHeaderBytes));

               if (previous.GetCount()) {
                  // Memory moved, and we should move all elements with 
                  // it. We're moving to new memory, so no reverse is   
                  // required.                                          
                  auto to = self.GetHandle().ForceMutable();
                  try {
                     previous.template Apply<false>([&to,&self,&previous](auto&& from) {
                        (void) self; (void) previous;

                        if constexpr (CT::Supported<decltype(from)>) {
                           Id::ForEach([&]<Cid D>{
                              to.template EmplaceWithIntent<D>(Abandon(from));
                           });

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
      template<Cid SID = Id::First, CT::Container C> requires Relevant<SID>
      void AllocateLess(this C& self, const Count<C> desiredReserve) {
         static_assert(CT::ContainsMany<C>,
            "This makes sense to be called only by containers that support many elements");

         const auto al = DecvqAllCast(self.template GetAllocation<SID>());
         if (not al) {
            //                                                          
            // We have to branch out                                    
            const C backup{Abandon{self}};
            ThisCom::AllocateFresh(ThisCom::RequestHeap(desiredReserve));

            // Reinsert only the relevant items                         
            auto to = self.GetHandle();
            try {
               backup.Apply([&to](auto& from) {
                  Id::ForEach([&]<Cid D>{
                     to.template EmplaceWithIntent<D>(Refer(from)); //TODO won't work for maps/sets
                  });
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
            temp.template DestroyAllElements<true, SID>();
            if_available(self.template SetCountInner<SID>(desiredReserve));
         }

         const auto request = ThisCom::RequestHeap(desiredReserve);
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
      ///   @attention works on one dimension at a time!                      
      template<Cid SID = Id::First, CT::Container C>
      requires (C::template CountHeapFooterRequests<SID>() > 0 and Relevant<SID>)
      void RemapHeapRequests(this C& self, const Count<C> newReserved) {
         const auto reserved = self.template GetReserved<SID>();
         [[maybe_unused]]
         const auto indirect = self.template GetIndirections<SID>();

         size_t from[C::template CountHeapFooterRequests<SID>() + 1];
         size_t to  [C::template CountHeapFooterRequests<SID>() + 1];
         size_t idx = 1;
         from[0] = to[0] = 0;
         
         C::ComponentList::ForEach([&]<class COM> {
            if constexpr (requires { typename COM::HeapRequest; }) {
               using R = typename COM::HeapRequest;
               if constexpr (IsRequestModifier<R>
               and COM::Id::template Contains<SID>) {
                  size_t shift = sizeof(TypeOf<R>);
                  if constexpr (R::AllocatedPerIndirection)
                     shift *= indirect;

                  if constexpr (R::AllocatedPerElement) {
                     from[idx] = from[idx-1] + shift * reserved;
                     to  [idx] = to  [idx-1] + shift * newReserved;
                  }
                  else {
                     from[idx] = from[idx-1] + shift;
                     to  [idx] = to  [idx-1] + shift;
                  }

                  ++idx;
               }
            }            
         });

         // Calculate the new destination                               
         auto to_footer = self.template GetRawAs<uint8_t, Id::First>();
         Id::ForEachConstOr([&]<Cid i>{
            if constexpr (CT::TypeErased<C>) {
               const auto T = self.template GetType<i>();
               LglsAssumeDev(T, "Requesting allocation size for an untyped container");
               to_footer = Align(to_footer, T.GetAlignment());
               to_footer += newReserved * T.GetSize();
            }
            else {
               using T = TypeOf<C, i>;
               to_footer = Align(to_footer, alignof(T));
               to_footer += newReserved * sizeof(T);
            }

            if constexpr (i == SID)
               return true;
            else {
               to_footer += self.template DefineHeapFooter<i>(newReserved);
               return No {};
            }
         });

         const auto size = self.template GetStride<SID>();
         const auto from_footer = self.template GetRawAs<uint8_t, SID>() + reserved * size;

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
      template<Cid SID = Id::First, CT::Container C> requires Relevant<SID>
      void PartialSuccess(this C& self, Count<C> n) {
         if constexpr (requires { self.template SetCountInner<SID>(1); }) {
            // Partial success is supported                             
            self.template SetCountInner<SID>(n);
            self.template ResetHash<SID>();

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
            self.template ResetAllocationInner<SID>();
         }
      }

      /// Branch out the current container by doing a shallow copy.           
      /// Happens when you try to modify a container with strong ownership    
      /// from somewhere else (when GetUses() > 1)                            
      ///   @param newReserve usually branching is accompanied by a resize,   
      ///      so specify it here                                             
      template<Cid SID = Id::First, CT::Container C> requires Relevant<SID>
      void BranchOut(this C& self, Count<C> newReserve) {
         if (self.template GetUses<SID>() > 1) {
            // We have to branch out                                    
            const C backup {Abandon{self}};
            ThisCom::AllocateFresh(ThisCom::RequestHeap(newReserve));

            // Reinsert the old items                                   
            auto to = self.GetHandle().ForceMutable();
            try {
               backup.template Apply<false>([&to](auto const& from) {
                  if constexpr (CT::Supported<decltype(from)>) {
                     Id::ForEach([&]<Cid D>{
                        to.template EmplaceWithIntent<D>(Refer(from));
                     });
                  }
                  ++to;
               });
            }
            catch (...) {
               self.template SetCountInner<SID>(to - self.GetHandle()); //TODO apply to all shared counts?
               throw;
            }

            self.template SetCountInner<SID>(backup.template GetCount<SID>()); //TODO apply to all shared counts? also, isn't this redundant? self already has the count?
         }
         else ThisCom::AllocateMore(newReserve);
      }
   };

   #undef ThisCom
}
