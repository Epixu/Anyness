///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Combines multiple deep ownership components into a unified interface to
   /// combat C++ base method ambiguities, and to add a bit more convenience. 
   ///   @tparam TC0, TC1, TCN... all the deep ownership components to unify  
   template<CT::Component TC0, CT::Component TC1, CT::Component...TCN>
   struct LANGULUS_EBCO MultiownDeep : TC0, TC1, TCN... {
      using CTTI_Component = Yes<>;
      using CTTI_ReflectAs = void;
      using Subcomponents  = Types<TC0, TC1, TCN...>;
      using Id             = ConcatenateValueLists<typename TC0::Id,
                                                   typename TC1::Id,
                                                   typename TCN::Id...>;
      static_assert(TC0::Id::Count == 1
              and   TC1::Id::Count == 1
              and ((TCN::Id::Count == 1) and ...),
              "Each subcomponent needs to be dedicated to their single dimension");

      static constexpr int ComponentPrecedence = 2000;
      static_assert(TC0::ComponentPrecedence == 2000
              and   TC1::ComponentPrecedence == 2000
              and ((TCN::ComponentPrecedence == 2000) and ...),
              "All precedences should match");

      
      /// Get entry array if containing pointers                              
      ///   @attention may contain invalid data for discontiguous containers  
      ///   @return the array of entries                                      
      template<Cid SID>
      auto GetEntries(this auto const& self) assumptious -> Allocation const* const* {
         return Subcomponents::ForEachConstOr([&]<class C> assumptious {
            if constexpr (C::Id::First == SID)
               return self.C::GetEntries();
            else
               return No{};
         });
      }

      /// Get entry array for all indirections of a specific element          
      ///   @return the array of entries                                      
      template<Cid SID>
      auto GetEntriesAt(this auto const& self, auto const& idx) assumptious -> Allocation const* const* {
         return Subcomponents::ForEachConstOr([&]<class C> assumptious {
            if constexpr (C::Id::First == SID)
               return self.C::GetEntriesAt(idx);
            else
               return No{};
         });
      }

   protected:
      LglsComHeapMovable(friend);
      LglsComRemoval(friend);
      LglsComOwnershipDeepEmergent(friend);
      LglsComEmplacement(friend);
      LglsComIterationOperators(friend);

      /// Get entry array if containing pointers (inner)                      
      ///   @attention may be uninitialized                                   
      template<Cid SID>
      constexpr decltype(auto) GetEntriesInner(this auto&& self) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept -> decltype(auto) {
            if constexpr (C::Id::First == SID)
               return self.C::GetEntriesInner();
            else
               return No{};
         });
      }

      /// Set the entry array (inner)                                         
      template<Cid SID>
      constexpr void SetEntriesInner(this auto& self, auto entries) noexcept {
         Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::First == SID)
               return self.C::SetEntriesInner(entries);
            else
               return No{};
         });
      }

      /// Default-initialization of this component                            
      constexpr void ConstructDefault(this auto& self) noexcept {
         Subcomponents::ForEach([&]<class C> noexcept {
            self.C::ConstructDefault();
         });
      }

      /// Transfer from any kind of container.                                
      /// This is only a reference to the entries and is not allowed          
      /// to allocate any new memory, so all this does is copy the            
      /// pointer, ignoring any intents.                                      
      ///   @param intent the intent and container to transfer from           
      constexpr void ConstructFrom(this auto& self, auto&& intent) noexcept {
         Subcomponents::ForEach([&]<class C> noexcept {
            self.C::ConstructFrom(LglsFwd(intent));
         });
      }

      /// This method is called upon allocation to nullify all entries        
      /// for a specific dimension.                                           
      template<Cid SID>
      constexpr void ConstructHeapRequest(this auto& self) noexcept {
         Subcomponents::ForEach([&]<class C> noexcept {
            if constexpr (C::Id::First == SID)
               self.C::ConstructHeapRequest();
         });
      }

      /// Deep-reference an element                                           
      ///   @attention works on one dimension at a time!                      
      template<bool FIND_MISSING, Cid SID>
      constexpr void KeepElementDeep(this auto& self) assumptious {
         Subcomponents::ForEachConstOr([&]<class C> assumptious {
            if constexpr (C::Id::First == SID)
               return self.C::template KeepElementDeep<FIND_MISSING>();
            else
               return No{};
         });
      }
   };
}
