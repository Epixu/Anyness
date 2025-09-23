///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
#include "../Allocator.hpp"
#include <Langulus/Assume.hpp>
#include <Langulus/CT/Allocatable.hpp>
#include <Langulus/CT/Referenced.hpp>


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Reserves a part of the heap to keep track of sparse element's          
   /// allocations                                                            
   ///   @tparam ID - which heap are we keeping track of?                     
   template<unsigned ID>
   struct DeepOwnershipHeap {
      using CTTI_Component = Yes<>;
      using HeapRequest = AllocationPtr;
      
      static constexpr bool DeeplyOwned = true;
      static constexpr int  ComponentPrecedence = 2000;

   protected:
      template<unsigned>
      friend struct HeapMovable;

      template<CT::Container C>
      using View = typename C::ViewType;
      template<CT::Container C>
      using Count = typename C::CountType;

      /// Get entry array if containing pointers                              
      /// If container is dense, it returns the main allocation               
      ///   @return the array of entries                                      
      template<CT::Container C>
      auto GetEntries(this C&& self) has_assumptions
      -> Tmut<C, AllocationPtr*, AllocationPtr const*> {
         using DC = Deref<C>;
         if constexpr (DC::TypeErased) {
            if (self.IsSparse()) {
               LglsAssumeDev(self.GetHeap(),
                  "No memory available");
               LglsAssumeDev(self.GetAllocation(),
                  "Entries do not exist for sparse containers which are out of jurisdiction");
               return reinterpret_cast<AllocationPtr*>(self.GetHeapEnd());
            }
            else return self.GetAllocationRef();
         }
         else {
            if constexpr (DC::Sparse) {
               LglsAssumeDev(self.GetHeap(),
                  "No memory available");
               LglsAssumeDev(self.GetAllocation(),
                  "Entries do not exist for sparse containers which are out of jurisdiction");
               return reinterpret_cast<AllocationPtr*>(self.GetHeapEnd());
            }
            else return self.GetAllocationRef();
         }
      }

      /// This function is called for all container components when the       
      /// allocation changes to update any heap-allocated data pointers       
      /*template<CT::Container C>
      void OnAllocationChange(this C& self, const View<C>& oldv) {
         AssumeDev(self.GetAllocation() != oldv.GetAllocation(),
            "Allocation didn't change");

         if constexpr (C::Sparse) {
            // Move entry data to its new place                         
            MoveMemory(self.GetEntry(), oldv.GetEntry(), self.GetCount());
         }
      }*/

      /// Reference the first pointer in the container                        
      ///   @attention assumes that *self.mSparseHeap has been set prior      
      ///   @tparam S - the intent used                                       
      template<CT::Intent S, CT::Container C>
      void KeepDeep(this C& self) {
         using ST = TypeOf<S>;
         using DT = Deptr<ST>;
         static_assert(S::Shallow);
         static_assert(CT::Sparse<ST>);
         static_assert(not CT::Null<ST>);

         // Raw pointers are always referenced, even when moved, as     
         // long as it's a keeper intent                                
         if constexpr (C::TypeErased) {
            LglsAssumeDev(self.IsSparse() and (CT::Void<DT> or self.template IsSimilar<ST>()),
               "Type mismatch");

            if constexpr (S::Keep and CT::Allocatable<DT>) {
               auto found = Allocator::Find(self.mType, *self.mSparseHeap);
               if (found) {
                  *self.GetEntry() = found;
                  found->Keep();
                  self.mType.Keep(*self.mSparseHeap, 1);
               }
               else *self.GetEntry() = nullptr;
            }
            else *self.GetEntry() = nullptr;
         }
         else if constexpr (CT::ConstructibleFrom<TypeOf<C>, ST>) {
            if constexpr (S::Keep and CT::Allocatable<DT>) {
               auto found = Allocator::Find(self.GetType(), *self.mSparseHeap);
               if (found) {
                  *self.GetEntry() = found;
                  found->Keep();
                  if constexpr (CT::Referenced<DT>)
                     (*self.GetRaw())->Reference(1);
               }
               else *self.GetEntry() = nullptr;
            }
            else *self.GetEntry() = nullptr;
         }
      }

      /// Reference the first pointer in the container                        
      ///   @attention assumes that *self.mSparseHeap and entry have been set 
      ///   @tparam S - the intent used                                       
      /*template<CT::Intent S, CT::Container C>
      void DeepKeep(this C& self, auto allocation) {
         using ST = TypeOf<S>;
         using DT = Deptr<ST>;
         using AL = decltype(allocation);
         static_assert(S::Shallow);
         static_assert(CT::Sparse<ST>);
         static_assert(not CT::Null<ST>);

         if constexpr (C::TypeErased) {
            AssumeDev(self.IsSparse() and (CT::Void<DT> or self.template IsSimilar<ST>()),
               "Type mismatch");

            if constexpr (S::Keep or S::Move) {
               if constexpr (CT::NotNull<AL>) {
                  // Entry is already available, no need to search      
                  if constexpr (not S::Move) {
                     if (*self.GetEntry()) {
                        self.GetEntry()->Keep();
                        self.mType.Keep(*self.mSparseHeap, 1);
                     }
                  }
               }
               else {
                  // Entry is not available yet - search for it         
                  auto found = Allocator::Find(self.mType, *self.mSparseHeap);
                  if (found) {
                     *self.GetEntry() = found;
                     found->Keep();
                     self.mType.Keep(*self.mSparseHeap, 1);
                  }
                  else *self.GetEntry() = nullptr;
               }
            }
            else *self.GetEntry() = nullptr;
         }
         else if constexpr (CT::ConstructibleFrom<TypeOf<C>, ST>) {
            if constexpr (S::Keep and CT::Allocatable<DT>) {
               auto found = Allocator::Find(self.GetType(), *self.mSparseHeap);
               if (found) {
                  *self.GetEntry() = found;
                  found->Keep();
                  if constexpr (CT::Referenced<DT>)
                     (*self.GetRaw())->Reference(1);
               }
               else *self.GetEntry() = nullptr;
            }
            else *self.GetEntry() = nullptr;
         }
      }*/
   };
}
