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
   /// Manages deep ownership by holding a pointer to the entries locally     
   ///   @tparam ID - which heap/stack are we keeping track of                
   template<unsigned ID>
   struct OwnershipDeepStack : OwnershipDeepEmergent<ID> {
      using StackRequest = EntryPtr;

   protected:
      template<unsigned> friend struct Emplacement;
      
      /// Get the entry array (inner)                                         
      template<unsigned SELECTOR = ID> requires (SELECTOR == ID)
      constexpr auto& GetEntriesInner(this auto&& self) noexcept {
         return self.template AccessStack<OwnershipDeepStack>();
      }

      /// Set the entry array (inner)                                         
      template<unsigned SELECTOR = ID> requires (SELECTOR == ID)
      constexpr void SetEntriesInner(this auto& self, StackRequest entries) noexcept {
         self.template GetEntriesInner<SELECTOR>() = entries;
      }

      /// Get entry array if containing pointers                              
      auto GetEntries(this auto&& self) has_assumptions -> EntryPtr {
         if (self.IsSparse()) {
            LglsAssumeDev(self.GetRaw(), "No memory available");
            return self.GetEntriesInner();
         }
         return nullptr;
      }
   };
}
