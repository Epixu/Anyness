///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Heap-Reference.hpp"
#include <Langulus/Utils/Iterate-Handles.hpp>


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Interfaces a heap. Adds a member that points to the heap memory.       
   /// The heap is allowed to move on reallocation.                           
   ///   @tparam ID multiple heaps are supported                              
   template<unsigned ID>
   struct HeapMovable : HeapReference<ID> {
      static constexpr bool HeapCanBeNull = true;

   protected:
      template<unsigned, class>     friend struct ReserveEmergent;
      template<unsigned>            friend struct IterationOperators;
      template<unsigned, class AS>  friend struct Insertion;
      template<unsigned>            friend struct Emplacement;

      template<CT::Container C>
      using Count = typename Deref<C>::CountType;
      template<CT::Container C>
      static constexpr auto CountMax = ::std::numeric_limits<Count<C>>::max();
      template<CT::Container C>
      using Pick = Tmut<C, typename Deref<C>::PickMut, typename Deref<C>::Pick>;
      template<CT::Container C>
      using Deep = typename Deref<C>::DeepType;

      using typename HeapReference<ID>::Request;
      
      /// Default-initialize the heap pointer                                 
      constexpr void ConstructDefault(this auto& self) noexcept {
         self.SetHeapInner(nullptr);
      }
      
      /// Transfer from any kind of container, respecting intents             
      ///   @param intent the intent and container to transfer from           
      template<CT::Container C, CT::Intent I> requires CT::Container<I>
      void ConstructFrom(this C& self, I&& intent) {
         using IT = Decay<I>;
         decltype(auto) from = FWD(intent.what);

         if constexpr (CT::Copied<I> or CT::Cloned<I>) {
            // Do a copy or clone.                                      
            // When copying, we're cloning just the first layer, so we  
            // guarantee that data is no longer static and constant at  
            // the first level of indirection.                          
            auto type = from.GetType().GetDecvq();
            self.SetType(type);
            auto count = from.GetCount();
            if (0 == count) {
               self.SetHeapInner(nullptr);
               self.SetAllocationInner(nullptr);
               if_available(self.SetCountInner(0));
               if_available(self.SetHashInner(1));
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

            auto al  = self.AllocateFresh(self.RequestHeap(count));
            auto src = IterateHandles(from).begin();
            auto dst = IterateHandles(self).begin();
            try {
               while (src) {
                  if constexpr (CT::Copied<I>)
                     dst->EmplaceWithIntent(Refer(*src));
                  else
                     dst->EmplaceWithIntent(Clone(*src));
                  ++dst; ++src;
               }
            } catch (...) {
               // Partial success                                       
               auto n = src - IterateHandles(from).begin();
               if constexpr (requires { self.SetCountInner(1); }) {
                  self.SetCountInner(n);
                  self.ResetHash();
               }
               else {
                  // Partial success is not allowed - we have to        
                  // deallocate and make sure CountStatic reports as    
                  // empty.                                             
                  while (n) {
                     if constexpr (requires { dst->DestroyElementDeepCustomPointers(); })
                        dst->DestroyElementDeepCustomPointers();
                     else
                        dst->DestroyElement();
                     --dst; --n;
                  }
                  Allocator::Deallocate(al);
                  self.SetAllocationInner(nullptr);
                  if_available(self.SetHashInner(1));
               }
               throw;
            }
                     
            // Full success                                             
            if_available(self.SetCountInner(count));

            if constexpr (requires { from.GetHashInner(); }) {
                    if_available(self.SetHashInner(from.GetHashInner()))
               else if_available(self.ResetHash())
            }
         }
         else {
            // Move/Refer/Abandon/Disown other                          
            static_assert(I::IsShallow());
            self.SetType(from.GetType());
            self.SetHeapInner(from.GetHeapInner());

            if constexpr (I::IsKept()) {
               if constexpr (I::IsMoved()) {
                  // Move                                               
                  if constexpr (CT::AutoOwned<I>) {
                     from.SetHeapInner(nullptr);
                     if_available(from.ResetState());
                     if_available(from.ResetType());
                  }
               }
               else static_assert(CT::Referred<I>);
            }
         }
      }

      /// Reassign from any kind of container, respecting intents             
      ///   @param intent the intent and container to assign from             
      template<class C, CT::Intent I> requires CT::Container<I>
      auto AssignFrom(this C& self, I&& intent) -> C& {
         if constexpr (requires { &self == &intent.what; }) {
            // Make sure 'lhs' and 'rhs' are different instances,       
            // otherwise we lose rhs if we free lhs                     
            if (&self == &intent.what)
               return self;
         }

         self.Free();
         self.SetHeapInner(nullptr);
         self.ConstructFrom(FWD(intent));
         return self;
      }
      
      /// Allocate a fresh allocation                                         
      ///   @attention changes allocation, heap pointer and reserve count only
      ///   @param request request to fulfill                                 
      template<CT::Container C>
      auto AllocateFresh(this C& self, const Request& request) -> Allocation* {
         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            auto al = Allocator::Allocate(self.GetType(), request.mTotalBytes);
         #else
            auto al = Allocator::Allocate(self.GetAlignment(), request.mTotalBytes);
         #endif
         LglsAssert(al, "Out of memory");
         
         self.SetHeapInner(al->GetBlockStart() + request.mHeaderBytes);
         self.SetAllocationInner(al);
         if_available(self.SetReserveInner(request.mReserved));
         return al;
      }

      /// Allocate a number of elements, relying on the type of the container 
      ///   @attention assumes container is typed                             
      ///   @tparam CREATE true to call constructors and set count            
      ///   @tparam SETSIZE true to set count, despite not constructing       
      ///   @param elements number of elements to allocate                    
      template<bool CREATE = false, bool SETSIZE = false, CT::Container C>
      void AllocateMore(this C& self, const Count<C> elements) {
         LglsAssumeDev(elements > self.GetCount(), "Bad element count");
         const auto al = self.GetAllocation();
         const auto request = self.RequestHeap(elements);

         if (not al) {
            //                                                          
            // Allocate a fresh set of elements                         
            self.AllocateFresh(request);

            if constexpr (CREATE) {
               // Default-construct everything                          
               self.CropInner(self.GetCount(), elements).CreateDefault();
            }
            
            if constexpr (CREATE or SETSIZE)
               self.SetCount(elements);
            return;
         }

         //                                                             
         // Reallocate                                                  
         if (self.GetReserved() >= elements) {
            // Required memory is already available                     
            if constexpr (CREATE) {
               // But is not yet initialized, so do it                  
               if (self.GetCount() < elements) {
                  const auto count = elements - self.GetCount();
                  self.SelectInner(self.GetCount(), count).CreateDefault();
               }
            }

            if constexpr (CREATE or SETSIZE)
               self.SetCount(elements);
            return;
         }

         LglsAssumeDev(self.GetUses() == 1,
            "Can't reuse memory of a heap used from multiple places, "
            "BranchOut should've been called prior to AllocateMore"
         );

         C previous {Abandon {self}};
         
      #if LANGULUS_FEATURE(MANAGED_MEMORY)
         auto reallocated = Allocator::Reallocate(self.GetType(), request.mTotalBytes, al);
      #else
         auto reallocated = Allocator::Reallocate(request.mTotalBytes, al);
      #endif
         
         LglsAssert(reallocated, "Out of memory");
         self.SetAllocationInner(reallocated);

         if (reallocated != al) {
            self.SetHeapInner(reallocated->GetBlockStart() + request.mHeaderBytes);

            if (previous.GetCount()) {
               // Memory moved, and we should move all elements         
               // in it. We're moving to new memory, so no reverse      
               // is required.                                          
               auto from = IterateHandles(previous).begin();
               for (auto to : IterateHandles(self))
                  to.EmplaceWithIntent(Abandon(*(from++)));
            }
         }
         else {
            // Memory didn't move, but reserved count changed           
            // so all HeapRequests which are PerElement need to         
            // be moved around.                                         
            self.RemapHeapRequests(request.mReserved);
            previous.SetAllocationInner(nullptr);
         }

         if_available(self.SetReserveInner(request.mReserved));
         
         if constexpr (CREATE) {
            // Default-construct the rest                               
            const auto count = elements - self.GetCount();
            self.CropInner(self.GetCount(), count).CreateDefault();
         }

         if constexpr (CREATE or SETSIZE)
            self.SetCount(elements);
      }

      /// Shrink the block, depending on currently reserved	elements.         
      /// Initialized elements on the back will be destroyed.                 
      /// When MANAGED_MEMORY is enabled we have a strong guarantee that      
      /// allocations never move when shrinking.                              
      ///   @param desiredReserve number of elements to reserve               
      template<CT::Container C>
      void AllocateLess(this C& self, const Count<C> desiredReserve) {
         LglsAssumeDev(desiredReserve < self.GetReserved(),
            "Can't shrink allocation using more elements");
         const auto al = self.GetAllocation();
         LglsAssumeDev(al, "Invalid allocation");
         LglsAssumeDev(al->GetUses() == 1,
            "Can't reuse memory of a block used from multiple places, "
            "BranchOut should've been called prior to AllocateMore"
         );

         const auto request = self.RequestHeap(desiredReserve);
         if constexpr (CT::TypeErased<C>) {
            //                                                          
            // Type-erased shrinking                                    
            const auto T = self.GetType();
            LglsAssumeDev(T, "Invalid type");
            const auto currentCount = self.GetCount();
            
            if (currentCount > desiredReserve) {
               // Destroy elements on the back                          
               if (T.GetDestructor())
                  self.SelectInner(desiredReserve, currentCount - desiredReserve).FreeInner();
               if_available(self.SetCountInner(desiredReserve));
            }

            // Early return if reserve itself didn't change             
            if (request.mElementCount == self.GetReserved())
               return;

            self.RemapHeapRequests(request.mReserved);

            #if LANGULUS_FEATURE(MANAGED_MEMORY)
               self.SetAllocation(Allocator::Reallocate(T, request.mByteSize, al));
            #else
               self.SetAllocation(Allocator::Reallocate(request.mByteSize, al));
            #endif
         }
         else {
            //                                                          
            // Statically-typed shrinking                               
            using T = TypeOf<C>;
            const auto currentCount = self.GetCount();
            
            if (currentCount > desiredReserve) {
               // Destroy elements on the back                          
               if constexpr (CT::Destroyable<T>)
                  self.SelectInner(desiredReserve, currentCount - desiredReserve).FreeInner();
               if_available(self.SetCountInner(desiredReserve));
            }
            
            // Early return if reserve itself didn't change             
            if (request.mReserved == self.GetReserved())
               return;

            self.RemapHeapRequests(request.mReserved);

            #if LANGULUS_FEATURE(MANAGED_MEMORY)
               self.SetAllocationInner(Allocator::Reallocate(self.GetType(), request.mTotalBytes, al));
            #else
               self.SetAllocationInner(Allocator::Reallocate(request.mTotalBytes, al));
            #endif
         }

         if_available(self.SetReserveInner(request.mReserved));
      }

      /// Remap all heap requests onto the newly reserved count               
      template<CT::Container C>
      void RemapHeapRequests(this C& self, const Count<C> newReserved) {
         const auto reserved = self.GetReserved();
         const auto indirect = self.GetIndirections();
         if (self.GetHeapHeaderSize(reserved, indirect) == 0)
            return;

         //TODO when newReserved is larger than reserved stuff has to move to the right,
         // so it must be done in reverse so that we don't destroy any data. otherwise stuff moves to the left, and all that from/to calculations are not necessary
         size_t from[C::ComponentList::Count];
         size_t to  [C::ComponentList::Count];
         size_t idx = 1;
         bool continuous = false;
         from[0] = to[0] = 0;
         
         C::ComponentList::ForEach([&]<class COM>{
            if constexpr (requires { typename COM::HeapRequest; }) {
               using R = typename COM::HeapRequest;
               if constexpr (requires { R::AllocatedPerIndirection; }) {
                  if constexpr (requires { R::Type::AllocatedPerElement; }) {
                     const size_t shift = sizeof(typename R::Type::Type) * indirect;
                     if (continuous) {
                        from[idx] += shift * reserved;
                        to  [idx] += shift * newReserved;
                     }
                     else {
                        from[idx] = from[idx-1] + shift * reserved;
                        to  [idx] = to  [idx-1] + shift * newReserved;
                     }
                  }
                  else {
                     const size_t shift = sizeof(typename R::Type) * indirect;
                     if (continuous) {
                        from[idx] += shift;
                        to  [idx] += shift;
                     }
                     else {
                        from[idx] = from[idx-1] + shift;
                        to  [idx] = to  [idx-1] + shift;
                     }
                  }
                  
                  // Move index only when a gap forms, so that we       
                  // minimize the 'memmove' calls                       
                  ++idx;
                  continuous = false;
               }
               else if constexpr (requires { R::AllocatedPerElement; }) {
                  if constexpr (requires { R::Type::AllocatedPerIndirection; }) {
                     const size_t shift = sizeof(typename R::Type::Type) * indirect;
                     if (continuous) {
                        from[idx] += shift * reserved;
                        to  [idx] += shift * newReserved;
                     }
                     else {
                        from[idx] = from[idx-1] + shift * reserved;
                        to  [idx] = to  [idx-1] + shift * newReserved;
                     }
                  }
                  else {
                     const size_t shift = sizeof(typename R::Type);
                     if (continuous) {
                        from[idx] += shift * reserved;
                        to  [idx] += shift * newReserved;
                     }
                     else {
                        from[idx] = from[idx-1] + shift * reserved;
                        to  [idx] = to  [idx-1] + shift * newReserved;
                     }
                  }
                  
                  // Move index only when a gap forms, so that we       
                  // minimize the 'memmove' calls                       
                  ++idx;
                  continuous = false;
               }
               else {
                  if (continuous) {
                     from[idx] += sizeof(R);
                     to  [idx] += sizeof(R);
                  }
                  else {
                     from[idx] = from[idx-1] + sizeof(R);
                     to  [idx] = to  [idx-1] + sizeof(R);
                     continuous = true;
                  }
               }
            }            
         });

         // Move regions, starting from the back ones                   
         auto header = self.GetAllocation()->GetBlockStart();
         while (idx) {
            --idx;
            memmove(header + to[idx], header + from[idx], from[idx+1] - from[idx]);
         }
      }
   };
}
