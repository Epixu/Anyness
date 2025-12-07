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
   /// allocations. The pointer to the array of allocations is recomputed     
   /// every time, based on the heap.                                         
   ///   @tparam ID - which heap/stack are we keeping track of?               
   template<unsigned ID>
   struct OwnershipDeepHeap : OwnershipDeepEmergent<ID> {
      using HeapRequest = PerElement<PerIndirection<AllocationPtr>>;

   protected:
      template<unsigned> friend struct HeapMovable;
      template<unsigned> friend struct Removal;
      template<unsigned> friend struct Emplacement;
      template<unsigned> friend struct OwnershipDeepEmergent;

   IF_LANGULUS_TESTING(public:)
      /// Get entry array if containing pointers                              
      ///   @return the array of entries                                      
      template<CT::Container C>
      auto GetEntries(this C&& self) has_assumptions -> EntryPtr {
         if (self.IsSparse()) {
            LglsAssumeDev(self.GetRaw(),
               "No memory available");
            LglsAssumeDev(self.GetAllocation(),
               "Entries do not exist for sparse containers which are out of jurisdiction");
            return const_cast<EntryPtr>(self.template AccessHeap<OwnershipDeepHeap>());
         }
         return nullptr;
      }
   };
}
