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
   /// The pointer to the array of allocations for each element and           
   /// indirection is kept locally. Useful to carry allocation data inside    
   /// handles.                                                               
   ///   @tparam ID which heap/stack provider are we tracking ownership for?  
   template<Cid ID>
   struct OwnershipDeepReference : OwnershipDeepEmergent<ID> {
      using StackRequest = EntryPtr;
      //using HeapRequest  = PerElement<PerIndirection<AllocationPtr>>;

      /// Get entry array if containing pointers                              
      auto GetEntries(this auto const& self) assumptious
      -> Decvq<Deref<decltype(self.GetEntriesInner())>> {
         if (self.IsSparse()) {
            LglsAssumeDev(self.GetRaw(), "No memory available");
            return self.GetEntriesInner();
         }
         return nullptr;
      }

   protected:
      template<Cid> friend struct Emplacement;
      template<Cid> friend struct OwnershipDeepEmergent;

      /// Get the entry array (inner)                                         
      template<unsigned SELECTOR = ID> requires (SELECTOR == ID)
      constexpr auto& GetEntriesInner(this auto&& self) noexcept {
         return self.template AccessStack<OwnershipDeepReference>();
      }

      /// Set the entry array (inner)                                         
      template<unsigned SELECTOR = ID> requires (SELECTOR == ID)
      constexpr void SetEntriesInner(this auto& self, EntryPtr entries) noexcept {
         self.template GetEntriesInner<SELECTOR>() = DecvqAllCast(entries);
      }
      
      /// Transfer from any kind of container.                                
      /// This is only a reference to the entries and is not allowed          
      /// to allocate any new memory, so all this does is copy the            
      /// pointer, ignoring any intents.                                      
      ///   @param intent the intent and container to transfer from           
      template<CT::Intent I> requires CT::Container<I>
      void ConstructFrom(this auto& self, I&& intent) noexcept {
         self.SetEntriesInner(intent.what.GetEntriesInner());
      }

      /// Transfer from any kind of container, respecting intents             
      ///   @param intent the intent and container to transfer from           
      /*template<CT::Container C, CT::Intent I> requires CT::Container<I>
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
      }*/
   };
}
