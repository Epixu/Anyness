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
   /// allocations                                                            
   ///   @tparam ID - which heap are we keeping track of?                     
   template<unsigned ID>
   struct OwnershipDeepHeap : OwnershipDeepEmergent<ID> {
      using HeapRequest = PerElement<AllocationPtr>;

      static constexpr bool DeeplyOwned = true;
      static constexpr int  ComponentPrecedence = 2000;

   protected:
      template<unsigned> friend struct HeapMovable;
      template<unsigned> friend struct Removal;
      template<unsigned> friend struct Emplacement;

      //template<unsigned>
      //friend struct HeapMovable;

      //template<CT::Container C>
      //using View = typename C::ViewType;
      //template<CT::Container C>
      //using Count = typename C::CountType;

      /// Get entry array if containing pointers                              
      /// If container is dense, it returns the main allocation               
      ///   @return the array of entries                                      
      template<CT::Container C>
      auto GetEntries(this C&& self) has_assumptions -> EntryPtr {
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
   };
}
