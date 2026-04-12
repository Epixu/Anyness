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
   /// Refers back to this particular component instance through the deduced  
   /// 'this'. Just for convenience. It is #undef-ed at the end of this file. 
   #define ThisCom self.OwnershipDeepHeap<ID, REF_INDIVIDUAL>

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
   ///   @tparam REF_INDIVIDUAL toggles whether contained items that were     
   ///      reflected as CT::Referenced get referenced. Elements will get     
   ///      referenced even if no entry for the element exist, but you can    
   ///      avoid referencing altogether if you use the Disown intent.        
   ///      To be more specific - when GetReference() is nullptr and the      
   ///      entire container is considered disowned.                          
   template<Cid ID, bool REF_INDIVIDUAL>
   struct OwnershipDeepHeap : OwnershipDeepEmergent<ID, REF_INDIVIDUAL> {
      using HeapRequest = PerElement<PerIndirection<AllocationPtr>>;

      /// Get entry array if containing pointers                              
      ///   @attention may contain invalid data for discontiguous containers  
      ///   @return the array of entries                                      
      template<Cid SID = ID> requires (SID == ID)
      auto GetEntries(this auto const& self) assumptious -> Allocation const* const* {
         if (self.template IsSparse<SID>()
         and self.template GetRaw<SID>() and self.template GetAllocation<SID>())
            return ThisCom::GetEntriesInner();
         return nullptr;
      }

      /// Get entry array for all indirections of a specific element          
      ///   @return the array of entries                                      
      template<Cid SID = ID, CT::Container C> requires (SID == ID and CT::Indexed<C>)
      auto GetEntriesAt(this C const& self, CT::Index auto&& idx) assumptious -> Allocation const* const* {
         static_assert(SID == ID);
         if constexpr (CT::TypeErased<C>) {
            auto T = self.GetType();
            if (T.IsSparse() and self.GetRaw() and self.GetAllocation()) {
               return ThisCom::GetEntriesInner()
                    + self.SimplifyIndex(LglsFwd(idx)) * T.GetIndirections();
            }
         }
         else {
            using T = TypeOf<C>;
            if constexpr (CT::Sparse<T>) {
               if (self.GetRaw() and self.GetAllocation()) {
                  return ThisCom::GetEntriesInner()
                       + self.SimplifyIndex(LglsFwd(idx)) * IndirectsOf<T>;
               }
            }
         }
         return nullptr;
      }

   protected:
      template<Cid, uint, uint, CT::HeapEntry...>  friend struct HeapMovable;
      template<Cid, Cid...>                        friend struct Removal;
      template<Cid, bool>                          friend struct OwnershipDeepEmergent;
      LglsComEmplacement(friend);

      /// Get entry array if containing pointers (inner)                      
      ///   @attention may be uninitialized                                   
      template<Cid SID = ID> requires (SID == ID)
      constexpr auto GetEntriesInner(this auto&& self) noexcept {
         return self.template AccessHeap<OwnershipDeepHeap>();
      }

      /// This method is called upon allocation to nullify entries            
      template<CT::Container C>
      constexpr void ConstructHeapRequest(this C& self) noexcept {
         const auto reserved = self.GetReserved();
         if constexpr (CT::TypeErased<C>) {
            const auto T = self.GetType();
            if (T.IsSparse()) {
               memset(
                  ThisCom::GetEntriesInner(), 0,
                  reserved * T.GetIndirections() * sizeof(AllocationPtr)
               );
            }
         }
         else {
            using T = TypeOf<C>;
            if constexpr (CT::Sparse<T>) {
               memset(
                  ThisCom::GetEntriesInner(), 0,
                  reserved * IndirectsOf<T> * sizeof(AllocationPtr)
               );
            }
         }
      }
   };

   #undef ThisCom
}
