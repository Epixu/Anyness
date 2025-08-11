///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Heap-Reference.hpp"
#include "../Allocator.hpp"
#include <Langulus/MetaOf.hpp>
#include <Langulus/CT/Index.hpp>
#include <Langulus/CT/Resolvable.hpp>
#include "Iteration-Range.hpp"


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Interfaces a heap allocation                                           
   /// Adds a member that points to the heap memory                           
   /// The pointer is allowed to move on reallocation                         
   ///   @tparam ID - multiple heap interfaces are supported                  
   template<unsigned ID = 0>
   struct HeapMovable : HeapReference<ID> {
      static constexpr bool HeapCanBeNull = true;

   protected:
      template<unsigned, class>
      friend struct ReserveEmergent;
      template<unsigned>
      friend struct IterationOperators;
      template<unsigned, class AS>
      friend struct Insertion;

      template<CT::Container C>
      using Count = typename Deref<C>::CountType;
      template<CT::Container C>
      static constexpr auto CountMax = ::std::numeric_limits<Count<C>>::max();
      template<CT::Container C>
      using Pick = Tif<CT::Mutable<C>, typename Deref<C>::PickMut, typename Deref<C>::Pick>;
      template<CT::Container C>
      using Deep = typename Deref<C>::DeepType;

   public:
      template<CT::NotVoid AS, CT::Container C>
      auto As(this C&& self) -> Pick<C>;

      template<CT::NotVoid AS, bool FATAL_FAILURE = true, CT::Container C>
      auto AsCast(this C const& self) -> AS;

      template<CT::Container C>
      auto GetItem(this C&&) has_assumptions -> Deep<C>;

      /// A safe way to get the first deep entry                              
      /// Will utilize any statically typed deep containers, if available     
      ///   @attention ignores sparseness                                     
      ///   @return a pointer to the first deep item, or nullptr if not deep  
      template<CT::Container C>
      auto GetDeep(this C&& self) noexcept -> Deep<C>* {
         if (not self.IsDeep())
            return nullptr;
         return self.template Get<Deep<C>*>();
      }

      template<CT::Container C>
      auto GetResolved(this C&&) -> Deep<C>;

      template<CT::Container C>
      auto GetDense(this C&&, Count<C> = CountMax<C>) -> Deep<C>;
      
   protected:      
      /// Default-initialize the component, defaulting members                
      /// A default-constructor isn't used for this to avoid duplication of   
      /// some calls                                                          
      template<CT::Container C>
      void ConstructDefault(this C& self) noexcept {
         self.mHeap = nullptr;
         if constexpr (requires { self.mReserved; })
            self.mReserved = 0;
         self.SetCount(0);
         self.SetHash(1);
         // Type should be default-initialized anyways                  
      }
      
      /// Transfer from any kind of container, respecting intents             
      ///   @param intent - the intent and container to transfer from         
      template<CT::Container C, CT::Intent I> requires CT::Container<I>
      void ConstructFrom(this C& self, I&& intent) {
         using IT = Decay<TypeOf<I>>;
         decltype(auto) from = FWD(intent.what);
         const auto count = from.GetCount();
         auto type = from.GetType();

         if constexpr (I::IsShallow()) {
            // Move/Copy/Refer/Abandon/Disown other                     
            if constexpr (I::IsKept()) {
               // Move/Copy/Refer other                                 
               if constexpr (I::IsMoved()) {
                  // Move                                               
                  self.mHeap = from.mHeap;
                  if constexpr (requires { self.mReserved; })
                     self.mReserved = from.GetReserved();
                  self.SetCount(count);
                  self.SetType(type);
                  self.SetHash(from.GetHashNoRecompute());

                  if constexpr (IT::Owned) {
                     from.mHeap = nullptr;
                     if constexpr (requires { from.mReserved; })
                        from.mReserved = 0;
                     from.SetCount(0);
                     if constexpr (requires { from.ResetState(); })
                        from.ResetState();
                     from.ResetType();
                     from.ResetHash();
                  }
               }
               else {
                  // Copy/Refer other                                   
                  if constexpr (CT::Referred<I>) {
                     // Refer                                           
                     self.mHeap = from.mHeap;
                     if constexpr (requires { self.mReserved; })
                        self.mReserved = from.GetReserved();
                     self.SetCount(count);
                     self.SetType(type);
                     self.SetHash(from.GetHashNoRecompute());
                  }
                  else {
                     // Do a shallow copy                               
                     // We're cloning first layer, so we guarantee,     
                     // that data is no longer static and constant      
                     // at first level of indirection                   
                     type = type.GetDecvq();
                     self.SetType(type);
                     if (0 == count) {
                        self.SetCount(0);
                        self.SetHash(1);
                        return;
                     }

                     // Pick a preferably typed block to optimize       
                     if constexpr (IT::TypeErased) {
                        // A runtime check is required before allocating
                        LglsAssert(type.GetReferConstructor(),
                           "Can't refer-construct elements"
                           " - no refer-constructor was reflected for type ",
                           type
                        );
                     }
                     else {
                        static_assert(CT::ReferConstructible<TypeOf<IT>>,
                           "Contained type is not refer-constructible");
                     }

                     self.AllocateFresh(self.RequestSize(count));
                     auto src = IterateHandles(from).begin();
                     auto dst = IterateHandles(self).begin();
                     try {
                        while (src != IteratorEnd {}) {
                           dst->EmplaceWithIntent(Refer(*src));
                           ++dst; ++src;
                        }
                     } catch (...) {
                        // Partial success                              
                        self.SetCount(src - IterateHandles(from).begin());
                        self.ResetHash();
                        throw;
                     }
                     
                     // Full success                                    
                     self.SetCount(count);
                     self.SetHash(from.GetHashNoRecompute());
                  }
               }
            }
            else if constexpr (I::IsMoved()) {
               // Abandon                                               
               self.mHeap = from.mHeap;
               if constexpr (requires { self.mReserved; })
                  self.mReserved = from.GetReserved();
               self.SetCount(count);
               self.SetType(type);
               self.SetHash(from.GetHashNoRecompute());
            }
            else {
               // Disown                                                
               self.mHeap = from.mHeap;
               if constexpr (requires { self.mReserved; })
                  self.mReserved = from.GetReserved();
               self.SetCount(count);
               self.SetType(type);
               self.SetHash(from.GetHashNoRecompute());
            }
         }
         else {
            // We're cloning, so we guarantee, that data is no longer   
            // constant at any level of indirection                     
            type = type.GetDecvqAll();
            self.SetType(type);
            if (0 == count) {
               self.SetCount(0);
               self.SetHash(1);
               return;
            }

            // Pick the typed block to optimize the construction        
            if constexpr (IT::TypeErased) {
               // A runtime check is required before allocating         
               LglsAssert(type.GetCloneConstructor(),
                  "Can't clone-construct elements"
                  " - no clone-constructor was reflected for type ",
                  type
               );
            }
            else {
               static_assert(CT::CloneConstructible<TypeOf<IT>>,
                  "Contained type is not clone-constructible");
            }

            self.AllocateFresh(self.RequestSize(count));
            const auto srcStart = IterateHandles(from).begin();
            auto src = srcStart;
            try {
               for (auto dst : IterateHandles(self)) {
                  dst.EmplaceWithIntent(Clone(*src));
                  ++src;
               }
            } catch (...) {
               // Partial success                                       
               self.SetCount(src - srcStart);
               //self.ResetHash();
               throw;
            }
                     
            // Full success                                             
            self.SetCount(count);
            self.SetHash(from.GetHashNoRecompute());
         }
      }

      /// Reassign from any kind of container, respecting intents             
      ///   @param intent - the intent and container to assign from           
      template<class C, CT::Intent I> requires CT::Container<I>
      void AssignFrom(this C& self, I&& intent) {
         // Make sure 'self' and 'intent' are different instances       
         if (&self == &intent.what)
            return;

         using IT = Decay<TypeOf<I>>;
         if constexpr (IT::TypeErased) {
            // Potentially absorb a container                           
            self.Free();
            new (&self) C {FWD(intent)};
         }
         else {
            // Potentially absorb a container                           
            self.Free();
            new (&self) C {FWD(intent)};
         }
      }

      /// Get a size based on reflected allocation page and count             
      ///   @param count - the number of elements to request                  
      ///   @return both the provided byte size and reserved count            
      template<CT::Container C>
      auto RequestSize(this const C& self, const Count<C> count) has_assumptions
      -> Allocation::Request {
         using T = TypeOf<C>;
         Allocation::Request result;

         if constexpr (C::TypeErased) {
            LglsAssumeDev(self.mType,
               "Requesting allocation size for an untyped container");

            // Check for reflected minimal allocation at runtime        
            result.mByteSize = Roof2(::std::max<Count<C>>(
               count * self.mType.GetSize(), self.mType.GetMinAlloc()));
            result.mElementCount = result.mByteSize / self.mType.GetSize();
         }
         else {
            // Check for reflected minimal allocation at compile-time   
            result.mByteSize = Roof2(::std::max<Count<C>>(
               count * sizeof(T), CT::GetMinAlloc<T>()));
            result.mElementCount = result.mByteSize / sizeof(T);
         }

         return result;
      }
      
      /// Allocate a fresh allocation                                         
      ///   @attention changes allocation, heap pointer and reserve count only
      ///   @param request - request to fulfill                               
      template<CT::Container C>
      void AllocateFresh(this C& self, const Allocation::Request& request) {
         Allocation* al;
         if constexpr (C::TypeErased) {
            if constexpr (CT::DeeplyOwned<C>) {
               // Deeply owned sparse containers have additional memory 
               // allocated for each pointer's entry                    
               al = Allocator::Allocate(self.mType,
                  request.mByteSize * (self.mType.IsSparse() ? 2 : 1)
               );
            }
            else al = Allocator::Allocate(self.mType, request.mByteSize);
         }
         else {
            // Deeply owned sparse containers have additional memory    
            // allocated for each pointer's entry                       
            al = Allocator::Allocate(self.GetType(),
               request.mByteSize * (CT::DeeplyOwned<C> and C::Sparse ? 2 : 1)
            );
         }

         LglsAssert(al, "Out of memory");
         self.mHeap = al->GetBlockStart();
         self.SetAllocation(al);
         if constexpr (requires { self.mReserved; })
            self.mReserved = request.mElementCount;
      }

      /// Allocate a number of elements, relying on the type of the container 
      ///   @attention assumes a valid and non-abstract type, if dense        
      ///   @tparam CREATE - true to call constructors and set count          
      ///   @tparam SETSIZE - true to set count, despite not constructing     
      ///   @param elements - number of elements to allocate                  
      template<bool CREATE = false, bool SETSIZE = false, CT::Container C>
      void AllocateMore(this C& self, const Count<C> elements) {
         const auto al = self.GetAllocation();
         LglsAssumeDev(elements > self.GetCount(), "Bad element count");

         if constexpr (CT::Typed<C>) {
            // Allocate/reallocate                                      
            using T = TypeOf<C>;
            const auto request = self.RequestSize(elements);
            if (al) {
               if (self.GetReserved() >= elements) {
                  // Required memory is already available               
                  if constexpr (CREATE) {
                     // But is not yet initialized, so initialize it    
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

               // Reallocate                                            
               typename C::PickRangeMut previous {self};
               auto reallocated = Allocator::Reallocate(
                  request.mByteSize * (CT::DeeplyOwned<C> and C::Sparse ? 2 : 1),
                  al
               );
               
               LglsAssert(reallocated, "Out of memory");
               self.SetAllocation(reallocated);
               if constexpr (requires { self.mReserved; })
                  self.mReserved = request.mElementCount;

               if (reallocated != previous.GetAllocation()) {
                  self.mHeap = reallocated->GetBlockStart();

                  if (previous.GetCount()) {
                     // Memory moved, and we should move all elements   
                     // in it. We're moving to new memory, so no reverse
                     // is required                                     
                     auto from = IterateHandles(previous).begin();
                     for (auto to : IterateHandles(self))
                        to.EmplaceWithIntent(Abandon(*(from++)));
                     previous.Free();
                  }
               }
               else {
                  // Memory didn't move, but reserved count changed     
                  if constexpr (C::Sparse) {
                     // Move entry data to its new place                
                     MoveMemory(self.GetEntries(), previous.GetEntries(), self.GetCount());
                  }
               }

               if constexpr (CREATE) {
                  // Default-construct the rest                         
                  const auto count = elements - self.GetCount();
                  self.CropInner(self.GetCount(), count).CreateDefault();
               }
            }
            else {
               // Allocate a fresh set of elements                      
               if constexpr (requires { self.mType; })
                  self.mType = MetaDataOf<T>();

               self.AllocateFresh(request);

               if constexpr (CREATE) {
                  // Default-construct everything                       
                  self.CropInner(self.GetCount(), elements).CreateDefault();
               }
            }
         }
         else {
            LglsAssert(self.mType,
               "Can't instantiate unknown type");
            LglsAssert(self.mType.IsSparse() or not self.mType.IsAbstract(),
               "Unable to instantiate ", elements, " elements of abstract type ", self.mType);

            if (self.GetReserved() >= elements) {
               // Required memory is already available                  
               if constexpr (CREATE) {
                  // But is not yet initialized, so initialize it       
                  if (self.GetCount() < elements) {
                     const auto count = elements - self.GetCount();
                     self.CropInner(self.GetCount(), count).CreateDefault();
                  }
               }
            }
            else AllocateInner<CREATE>(elements);
         }

         if constexpr (CREATE or SETSIZE)
            self.SetCount(elements);
      }

      /// Shrink the block, depending on currently reserved	elements          
      /// Initialized elements on the back will be destroyed                  
      /// When MANAGED_MEMORY is enabled we have a strong guarantee that      
      /// allocations never move when shrinking                               
      ///   @param desiredReserve - number of elements to reserve             
      template<CT::Container C>
      void AllocateLess(this C& self, const Count<C> desiredReserve) {
         LglsAssumeDev(desiredReserve < self.GetReserved(),
            "Can't shrink allocation using more elements");
         const auto allocation = self.GetAllocation();
         LglsAssumeDev(allocation, "Invalid allocation");
         LglsAssumeDev(allocation->GetUses() == 1,
            "Can't reuse memory of a block used from multiple places, "
            "BranchOut should've been called prior to AllocateMore"
         );

         const auto request = self.RequestSize(desiredReserve);
         if (request.mElementCount == self.GetReserved())
            return;

         if constexpr (C::TypeErased) {
            //                                                          
            // Type erased shrinking                                    
            const auto T = self.GetType();
            LglsAssumeDev(T, "Invalid type");

            const auto currentCount = self.GetCount();
            if (currentCount > desiredReserve) {
               // Destroy elements on the back                          
               if (T.GetDestructor())
                  self.SelectInner(desiredReserve, currentCount - desiredReserve).FreeInner();
               self.SetCount(desiredReserve);
            }

            if (T.IsSparse()) {
               // Move entry data to its new place                      
               MoveMemory(
                  self.GetEntries() - self.mReserved + request.mElementCount,
                  self.GetEntries(), currentCount
               );
            }

            self.SetAllocation(Allocator::Reallocate(
               request.mByteSize * (T.IsSparse() ? 2 : 1),
               allocation
            ));
         }
         else {
            //                                                          
            // Statically typed shrinking                               
            using T = TypeOf<C>;

            const auto currentCount = self.GetCount();
            if (currentCount > desiredReserve) {
               // Destroy elements on the back                          
               if constexpr (CT::Destroyable<T>)
                  self.SelectInner(desiredReserve, currentCount - desiredReserve).FreeInner();
               self.SetCount(desiredReserve);
            }

            if constexpr (CT::Sparse<T>) {
               // Move entry data to its new place                      
               MoveMemory(
                  self.GetEntries() - self.mReserved + request.mElementCount,
                  self.GetEntries(), currentCount
               );
            }

            self.SetAllocation(Allocator::Reallocate(
               request.mByteSize * (CT::Sparse<T> ? 2 : 1),
               allocation
            ));
         }

         if constexpr (requires { self.mReserved; })
            self.mReserved = request.mElementCount;
      }
      
      /// Reassign new value to the first element, with or without an intent  
      ///   @attention this overwrites previous handle without dereferencing  
      ///      it, and without destroying anything                            
      ///   @param rhs_with_intent - container to assign from?                
      template<CT::Container C>
      void AssignWithIntent(this C& self, CT::Container auto&& rhs_with_intent) {
         using S  = IntentOf<decltype(rhs_with_intent)>;
         using ST = TypeOf<S>;
         using STT = TypeOf<ST>;
         LglsAssumeDev(self.IsTyped(), "Invalid type");
         LglsAssumeDev(self.mHeap, "Invalid heap");
         auto& rhs = DeintCast(rhs_with_intent);

         if constexpr (C::TypeErased) {
            //                                                          
            // This container is type-erased                            
            //                                                          
            if (self.mType.IsSparse()) {
               LglsAssumeDev(rhs.IsSparse(), "Sparseness mismatch");

               if constexpr (S::Shallow) {
                  // Do a refer/copy/disown/abandon/move sparse LHS     
                  *self.mSparseHeap = *rhs.mSparseHeap;
                  if constexpr (CT::DeeplyOwned<C, ST>)
                     *self.GetEntry() = *rhs.GetEntry();

                  if constexpr (S::ResetsOnMove) {
                     *rhs.mSparseHeap = nullptr;
                     if constexpr (CT::DeeplyOwned<C, ST>)
                        *rhs.GetEntry() = nullptr;
                  }

                  if constexpr (CT::DeeplyOwned<C>) {
                     if constexpr (CT::DeeplyOwned<ST>)
                        self.template DeepKeep<S>(*rhs.GetEntry());
                     else
                        self.template DeepKeep<S>(nullptr);
                  }
               }
               else {
                  //TODO clone pointers
                  TODO();
               }
            }
            else {
               // Do a refer/copy/disown/abandon/move/clone dense LHS   
               LglsAssumeDev(CT::Dense<STT>, "Sparseness mismatch");

               if constexpr (CT::Moved<S>)
                  self.mType.MoveAssign   (self.mHeap, rhs.mHeap);
               else if constexpr (CT::Abandoned<S>)
                  self.mType.AbandonAssign(self.mHeap, rhs.mHeap);
               else if constexpr (CT::Referred<S>)
                  self.mType.ReferAssign  (self.mHeap, rhs.mHeap);
               else if constexpr (CT::Copied<S>)
                  self.mType.CopyAssign   (self.mHeap, rhs.mHeap);
               else if constexpr (CT::Disowned<S>)
                  self.mType.DisownAssign (self.mHeap, rhs.mHeap);
               else if constexpr (CT::Cloned<S>)
                  self.mType.CloneAssign  (self.mHeap, rhs.mHeap);
               else
                  static_assert(false, "Unsupported intent");
            }
         }
         else {
            //                                                          
            // This container is statically-typed                       
            //                                                          
            using T = TypeOf<C>;

            if constexpr (S::Shallow and CT::Sparse<T>) {
               // Do a copy/refer/disown/abandon/move sparse RHS        
               if constexpr (CT::AssignableFrom<T, STT>) {
                  *self.mSparseHeap = *rhs.mSparseHeap;
                  if constexpr (CT::DeeplyOwned<C, ST>)
                     *self.GetEntry() = *rhs.GetEntry();

                  if constexpr (S::ResetsOnMove) {
                     *rhs.mSparseHeap = nullptr;
                     if constexpr (CT::DeeplyOwned<C, ST>)
                        *rhs.GetEntry() = nullptr;
                  }

                  if constexpr (CT::DeeplyOwned<C>) {
                     if constexpr (CT::DeeplyOwned<ST>)
                        self.template DeepKeep<S>(*rhs.GetEntry());
                     else
                        self.template DeepKeep<S>(nullptr);
                  }
               }
               else static_assert(false, "Can't construct sparse T");
            }
            else if constexpr (CT::Dense<T>) {
               // Do a copy/disown/abandon/move/clone inside a dense    
               // handle                                                
               if constexpr (CT::AssignableFrom<T, typename S::template As<STT>>)
                  *self.GetRaw() = S::Nest(*rhs.GetRaw());
               else
                  static_assert(false, "Can't construct dense T");
            }
            else if constexpr (CT::Dense<Deptr<T>>) {
               // Clone sparse/dense data                               
               if constexpr (CT::Resolvable<Decay<T>>) {
                  // If T is resolvable, we need to always clone the    
                  // resolved (a.k.a the most concrete) type            
                  TODO();
               }
               else {
                  // Otherwise attempt cloning DT conventionally        
                  using DT = Decay<T>;
                  auto meta = MetaDataOf<DT>();
                  auto entry = Allocator::Allocate(meta, meta->RequestSize(1).mByteSize);
                  auto pointer = entry->template As<DT>();
                  IntentAssign(pointer, S::Nest(*rhs->Get()));

                  *self.GetRaw() = pointer;
                  if constexpr (CT::DeeplyOwned<C>)
                     *rhs.GetEntry() = entry;
               }
            }
            else {
               // Pointers of pointers                                  
               // Clone indirection layers by nesting                   
               TODO();
            }
         }
      }
   };
}
