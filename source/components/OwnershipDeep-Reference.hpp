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
   ///   @tparam REF_INDIVIDUAL toggles whether contained items that were     
   ///      reflected as CT::Referenced get referenced. Elements will get     
   ///      referenced even if no entry for the element exist, but you can    
   ///      avoid referencing altogether if you use the Disown intent.        
   ///      To be more specific - when GetReference() is nullptr and the      
   ///      entire container is considered disowned.                          
   template<Cid ID, bool REF_INDIVIDUAL>
   struct OwnershipDeepReference : OwnershipDeepEmergent<ID, REF_INDIVIDUAL> {
      using StackRequest = EntryPtr;

      /// Get entry array if containing pointers                              
      ///   @attention may contain invalid data for discontiguous containers  
      ///   @return the array of entries                                      
      template<Cid SID = ID>
      auto GetEntries(this auto const& self) assumptious -> Allocation const* const* {
         static_assert(SID == ID);
         if (self.template IsSparse<SID>()) {
            LglsAssumeDev(self.template GetRaw<SID>(), "No memory available");
            return self.template GetEntriesInner<SID>();
         }
         return nullptr;
      }

      /// Get entry array for all indirections of a specific element          
      ///   @return the array of entries                                      
      template<Cid SID = ID, CT::Container C> requires CT::Indexed<C>
      auto GetEntriesAt(this C const& self, CT::Index auto&& idx) assumptious -> Allocation const* const* {
         static_assert(SID == ID);
         if (self.template IsSparse<SID>()) {
            LglsAssumeDev(self.template GetRaw<SID>(), "No memory available");
            return self.template GetEntriesInner<SID>() + self.template SimplifyIndex<SID>(LglsFwd(idx));
         }
         return nullptr;
      }

   protected:
      template<Cid, bool>     friend struct OwnershipDeepEmergent;
      LglsComEmplacement(friend);

      /// Get the entry array (inner)                                         
      template<Cid SID = ID> requires (SID == ID)
      constexpr auto& GetEntriesInner(this auto&& self) noexcept {
         return self.template AccessStack<OwnershipDeepReference>();
      }

      /// Set the entry array (inner)                                         
      template<Cid SID = ID> requires (SID == ID)
      constexpr void SetEntriesInner(this auto& self, EntryPtr entries) noexcept {
         self.template GetEntriesInner<SID>() = DecvqAllCast(entries);
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
