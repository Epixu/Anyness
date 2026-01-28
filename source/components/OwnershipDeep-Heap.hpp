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
   ///   @tparam ID which heap/stack are we keeping track of?                 
   template<unsigned ID>
   struct OwnershipDeepHeap : OwnershipDeepEmergent<ID> {
      using HeapRequest = PerElement<PerIndirection<AllocationPtr>>;

   protected:
      template<unsigned> friend struct HeapMovable;
      template<unsigned> friend struct Removal;
      template<unsigned> friend struct Emplacement;
      template<unsigned> friend struct OwnershipDeepEmergent;

      /// Get entry array if containing pointers (inner)                      
      ///   @attention may be uninitialized                                   
      constexpr auto GetEntriesInner(this auto&& self) noexcept {
         return self.template AccessHeap<OwnershipDeepHeap>();
      }

   public:
      /// Get entry array if containing pointers                              
      ///   @return the array of entries                                      
      auto GetEntries(this auto const& self) assumptious
      -> decltype(self.GetEntriesInner()) {
         if (self.IsSparse() and self.GetRaw() and self.GetAllocation())
            return self.GetEntriesInner();
         return nullptr;
      }
   };
}
