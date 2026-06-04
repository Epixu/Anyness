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
   #define ThisCom self.OwnershipDeepReference<STYLE, REF_INDIVIDUAL, ID, SHARED...>

   ///                                                                        
   /// The pointer to the array of allocations for each element and           
   /// indirection is kept locally. Useful to carry allocation data inside    
   /// handles.                                                               
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
   template<uint STYLE, bool REF_INDIVIDUAL, Cid ID, Cid...SHARED>
   struct OwnershipDeepReference : OwnershipDeepEmergent<STYLE, REF_INDIVIDUAL, ID, SHARED...> {
      using StackRequest = EntryPtr;
      using Id = typename OwnershipDeepEmergent<STYLE, REF_INDIVIDUAL, ID, SHARED...>::Id;

      template<Cid SID>
      static constexpr bool Relevant = Id::template Contains<SID>;

      /// Get entry array if containing pointers                              
      ///   @attention may contain invalid data for discontiguous containers  
      ///   @return the array of entries                                      
      template<Cid SID = ID> requires Relevant<SID>
      auto GetEntries(this auto const& self) assumptious
      -> Allocation const* const* {
         if (self.template IsSparse<SID>())
            return ThisCom::GetEntriesInner();
         return nullptr;
      }

      /// Get entry array for all indirections of a specific element          
      ///   @return the array of entries                                      
      template<Cid SID = ID, CT::Container C> requires (Relevant<SID> and CT::Indexed<C>)
      auto GetEntriesAt(this C const& self, CT::Index auto&& idx) assumptious
      -> Allocation const* const* {
         if (self.template IsSparse<SID>()) {
            LglsAssumeDev(self.template GetRaw<SID>(), "No memory available");
            return ThisCom::GetEntriesInner() + self.template SimplifyIndex<SID>(LglsFwd(idx));
         }
         return nullptr;
      }

   protected:
      LglsComOwnershipDeepEmergent(friend);
      LglsComEmplacement(friend);
      LglsComIterationOperators(friend);

      /// Get the entry array (inner, unsafe)                                 
      template<Cid SID = ID> requires Relevant<SID>
      constexpr auto& GetEntriesInner(this auto&& self) noexcept {
         return self.template AccessStack<OwnershipDeepReference>();
      }

      /// Set the entry array (inner)                                         
      constexpr void SetEntriesInner(this auto& self, EntryPtr entries) noexcept {
         ThisCom::GetEntriesInner() = DecvqAllCast(entries);
      }

      /// Default-initialization of this component                            
      void ConstructDefault(this auto& self) noexcept {
         ThisCom::SetEntriesInner(nullptr);
      }

      /// Copy the pointer to the entries, and reference if we have to        
      ///   @param intent The intent and container to transfer from.          
      template<class SELF, CT::Intent I> requires CT::Container<I>
      void ConstructFrom(this SELF& self, I&& intent) noexcept {
         ThisCom::SetEntriesInner(intent.what.template GetEntries<ID>());

         if constexpr (not CT::Copied<I> and not CT::Cloned<I> and (STYLE & OnCreateAndDestroy) != 0) {
            static_assert(not CT::Disowned<I>,
               "Disownment is not allowed for emergent deep ownership");
            decltype(auto) from = LglsFwd(intent.what);

            if constexpr (CT::Referred<I>) {
               // Refer                                                 
               ThisCom::Keep();
            }
            else if constexpr (CT::Abandoned<I> or CT::Moved<I>) {
               // Abandon/Move                                          
               // We must reference only if we can't guarantee a        
               // transfer of deep ownership.                           
               if constexpr (requires { from.template ResetEntries<ID>(); }) {
                  Id::ForEach([&from]<Cid D> {
                     // Note - all dimensions _must_ be resettable   
                     // to be well formed.                           
                     from.template ResetEntries<D>();
                  });
               }
               else {
                  // We can't reset source entries (they are probably
                  // emergent, too), which means that source _may_   
                  // dereference them when destroyed. We must        
                  // reference here as well to prevent segfault.     
                  ThisCom::Keep();
               }
            }
         }

      }
   };

   #undef ThisCom
}
