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
   #define ThisCom self.OwnershipDeepHeap<STYLE, REF_INDIVIDUAL, ID, SHARED...>

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
   ///   @tparam STYLE whether ownership will be automatically applied on     
   ///      construction, reassignment and destruction. Usually 0 if container
   ///      is just a view, or in other cases where you want to carry an      
   ///      allocation pointer, but not necessarily reference it.             
   ///   @tparam REF_INDIVIDUAL toggles whether contained items that were     
   ///      reflected as CT::Referenced get referenced. Elements will get     
   ///      referenced even if no entry for the element exist, but you can    
   ///      avoid referencing altogether if you use the Disown intent.        
   ///      To be more specific - when GetAllocation() is nullptr and the     
   ///      entire container is considered disowned.                          
   ///   @tparam ID which heap/stack are we keeping track of?                 
   ///   @tparam SHARED additional provider IDs that share the same behavior  
   ///   @note this is used primarily for local handles and containers with   
   ///      ownership in general. Shouldn't be used for embedded containers.  
   template<uint STYLE, bool REF_INDIVIDUAL, Cid ID, Cid...SHARED>
   struct OwnershipDeepHeap : OwnershipDeepEmergent<STYLE, REF_INDIVIDUAL, ID, SHARED...> {
      using HeapRequest = PerDimension<PerElement<PerIndirection<AllocationPtr>>>;
      using Id = typename OwnershipDeepEmergent<STYLE, REF_INDIVIDUAL, ID, SHARED...>::Id;

      template<Cid SID>
      static constexpr bool Relevant = Id::template Contains<SID>;

      /// Get entry array if containing pointers                              
      ///   @attention may contain invalid data for discontiguous containers  
      ///   @return the array of entries                                      
      template<Cid SID = ID> requires Relevant<SID>
      auto GetEntries(this auto const& self) assumptious
      -> Allocation const* const* {
         if (self.template IsSparse<SID>() and self.template GetRaw<SID>()
         and self.template GetAllocation<SID>())
            return ThisCom::template GetEntriesInner<SID>();
         return nullptr;
      }

      /// Get entry array for all indirections of a specific element          
      ///   @return the array of entries                                      
      template<Cid SID = ID, CT::Container C> requires (Relevant<SID> and CT::Indexed<C>)
      auto GetEntriesAt(this C const& self, CT::Index auto&& idx) assumptious
      -> Allocation const* const* {
         if constexpr (CT::TypeErased<C>) {
            auto T = self.template GetType<SID>();
            if (T.IsSparse() and self.template GetRaw<SID>() and self.template GetAllocation<SID>()) {
               return ThisCom::template GetEntriesInner<SID>()
                    + self.SimplifyIndex(LglsFwd(idx)) * T.GetIndirections();
            }
         }
         else {
            using T = TypeOf<C, SID>;
            if constexpr (CT::Sparse<T>) {
               if (self.template GetRaw<SID>() and self.template GetAllocation<SID>()) {
                  return ThisCom::template GetEntriesInner<SID>()
                       + self.SimplifyIndex(LglsFwd(idx)) * IndirectsOf<T>;
               }
            }
         }
         return nullptr;
      }

   protected:
      LglsComHeapMovable(friend);
      LglsComRemoval(friend);
      LglsComOwnershipDeepEmergent(friend);
      LglsComEmplacement(friend);
      LglsComIterationOperators(friend);

      /// Get entry array if containing pointers (inner)                      
      ///   @attention may be uninitialized                                   
      template<Cid SID = ID> requires Relevant<SID>
      constexpr auto GetEntriesInner(this auto&& self) noexcept {
         return self.template AccessHeap<OwnershipDeepHeap, SID>();
      }

      /// This method is called upon allocation to nullify all entries for    
      /// a particular dimension.                                             
      ///   @attention works in one dimension at a time!                      
      template<Cid SID = ID, CT::Container C> requires Relevant<SID>
      constexpr void ConstructHeapRequestPerDimension(this C& self) noexcept {
         auto count = 0;
         if constexpr (CT::TypeErased<C>) {
            const auto T = self.template GetType<SID>();
            if (T.IsSparse())
               count += self.template GetReserved<SID>() * T.GetIndirections();
         }
         else {
            using T = TypeOf<C, SID>;
            if constexpr (CT::Sparse<T>)
               count += self.template GetReserved<SID>() * IndirectsOf<T>;
         }

         memset(ThisCom::template GetEntriesInner<SID>(), 0, count * sizeof(AllocationPtr));
      }
      
      /// Refer all allocations pointed to by all indirections on absorption. 
      ///   @note When entries are stored on the heap, it is assumed that     
      ///      SetAllocationInner has been called in a previous component.    
      ///   @param intent The intent and container to transfer from           
      ///   @important Notice that Copy and Clone intents are not handled     
      ///      here. They're handled in heap components instead, in case      
      ///      something throws an exception while constructing.              
      template<class SELF, CT::Intent I> requires CT::Container<I>
      void ConstructFrom(this SELF& self, I&& intent) {
         if constexpr (not CT::Copied<I> and not CT::Cloned<I> and (STYLE & OnCreateAndDestroy) != 0) {
            decltype(auto) from = LglsFwd(intent.what);
            LglsAssumeDev(self.template GetAllocationInner<ID>(),
               "Allocation should've been initialized");

            if constexpr (CT::Referred<I>) {
               // Refer                                                 
               ThisCom::Keep();
            }
            else if constexpr (CT::Abandoned<I> or CT::Moved<I>) {
               // Abandon/Move                                          
               if constexpr (requires { from.template GetAllocationInner<ID>(); }) {
                  LglsAssumeDev(from.template GetAllocationInner<ID>() == nullptr,
                     "Remote container should've been disowned at this point");
               }
               else if constexpr (from.OwnedDeep & OnCreateAndDestroy) {
                  // 'from' is likely emergent, we have to reference    
                  ThisCom::Keep();
               }
            }
         }
      }
   };

   #undef ThisCom
}
