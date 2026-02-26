///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "OwnershipDeep-Emergent.hpp"


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Reserves a part of the heap to keep track of sparse element's          
   /// allocations. The pointer to the array of allocations is kept locally.  
   ///   @tparam ID which heap/stack are we keeping track of                  
   template<Cid ID>
   struct OwnershipDeepStack : OwnershipDeepEmergent<ID> {
      using StackRequest = EntryPtr;
      using HeapRequest  = PerElement<PerIndirection<AllocationPtr>>;

   protected:
      template<Cid> friend struct Emplacement;
      template<Cid> friend struct OwnershipDeepEmergent;

      /// Get the entry array (inner)                                         
      template<unsigned SELECTOR = ID> requires (SELECTOR == ID)
      constexpr auto& GetEntriesInner(this auto&& self) noexcept {
         return self.template AccessStack<OwnershipDeepStack>();
      }

      /// Set the entry array (inner)                                         
      template<unsigned SELECTOR = ID> requires (SELECTOR == ID)
      constexpr void SetEntriesInner(this auto& self, EntryPtr entries) noexcept {
         self.template GetEntriesInner<SELECTOR>() = DecvqAllCast(entries);
      }
      
      /// Transfer from any kind of container, respecting intents             
      ///   @param intent the intent and container to transfer from           
      template<CT::Container C, CT::Intent I> requires CT::Container<I>
      void ConstructFrom(this C& self, I&& intent) {
         decltype(auto) from = LglsFwd(intent.what);

         if constexpr (CT::Copied<I> or CT::Cloned<I>) {
            // Do a copy or clone.                                      
            // Since new memory is allocated, we recalculate the        
            // pointer to the entries. Populating the pointers is       
            // handled by the heap component.                           
            self.SetEntriesInner(self.template AccessHeap<OwnershipDeepStack>());
         }
         else if constexpr (I::IsKept() or I::IsMoved()) {
            // Move/Refer/Abandon other                                 
            static_assert(I::IsShallow());
            self.SetEntriesInner(from.GetEntriesInner());
         }
      }
      
   public:
      /// Get entry array if containing pointers                              
      auto GetEntries(this auto const& self) assumptious
      -> Decvq<Deref<decltype(self.GetEntriesInner())>> {
         if (self.IsSparse()) {
            LglsAssumeDev(self.GetRaw(), "No memory available");
            return self.GetEntriesInner();
         }
         return nullptr;
      }
   };
}
