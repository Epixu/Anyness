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
   /// every time, based on the heap. It is located in the heap footer and    
   /// moves every time the heap is reallocated. If contained type is int***, 
   /// the data has the following contiguous layout:                          
   ///   first  int*** allocations for each indirection [int**][int*][int]    
   ///   second int*** allocations for each indirection [int**][int*][int]    
   ///   third  int*** allocations... etc.},                                  
   /// essentially forming an array of indirections indexed like:             
   ///   entries[item_index * number_of_indirections + indirection_index]     
   ///   @tparam ID which heap provider are we using?                         
   template<Cid ID>
   struct OwnershipDeepHeap : OwnershipDeepEmergent<ID> {
      using HeapRequest = PerElement<PerIndirection<AllocationPtr>>;

   protected:
      template<Cid, unsigned, unsigned, CT::Sparse> friend struct HeapMovable;
      template<Cid>                                 friend struct Removal;
      template<Cid>                                 friend struct Emplacement;
      template<Cid>                                 friend struct OwnershipDeepEmergent;

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
