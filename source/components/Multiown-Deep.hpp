///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
LglsDisableWarningPush
LglsDisableWarning_UnusedLocalTypedef


namespace Langulus::Anyness::Component
{
   template<class...> struct MultiownDeep;

   template<CT::Component...TN> requires (CountEnabled<TN...> == 0)
   struct MultiownDeep<TN...> {
      using CTTI_Component = Yes<>;
      static constexpr bool SkipThisComponent = true;
   };

   ///                                                                        
   /// Combines multiple deep ownership components into a unified interface to
   /// combat C++ base method ambiguities, and to add a bit more convenience. 
   ///   @tparam TN... all the deep ownership components to unify              
   template<CT::Component...TN> requires (CountEnabled<TN...> >= 2)
   struct LANGULUS_EBCO MultiownDeep<TN...> : TN... {
      using CTTI_Component = Yes<>;
      using CTTI_ReflectAs = void;
      using Subcomponents  = decltype( Types<TN...>::Discard([]<class C> static { return requires { C::SkipThisComponent; }; }));
      using Id             = decltype(Subcomponents::Extract([]<class C> static { return typename C::Id{}; }));

      static_assert(Subcomponents::ForEachIndexedAnd([]<class C, size_t I> {
         return C::Id::Count == 1 and C::Id::First == I; }),
         "Each enabled subcomponent needs to be dedicated to their single dimension, "
         "and all subcomponents need to be sequential"
      );

      static constexpr int ComponentPrecedence = 2000;
      static_assert(Subcomponents::ForEachAnd([]<class C> { return C::ComponentPrecedence == 2000; }),
         "All precedences should match");

      #define if_inherits(...) requires (Subcomponents::ForEachOr([&]<class C> { \
         return requires { self.C::__VA_ARGS__; }; }))

      /// Get entry array if containing pointers                              
      ///   @attention may contain invalid data for discontiguous containers  
      ///   @return the array of entries                                      
      template<Cid SID = 0>
      auto GetEntries(this auto const& self) assumptious -> Allocation const* const* {
         using C = typename Subcomponents::template At<SID>;
         return self.C::GetEntries();
      }

      /// Get entry array for all indirections of a specific element          
      ///   @return the array of entries                                      
      template<Cid SID = 0>
      auto GetEntriesAt(this auto const& self, auto const& idx) assumptious -> Allocation const* const* {
         using C = typename Subcomponents::template At<SID>;
         return self.C::GetEntriesAt(idx);
      }

   protected:
      LglsComHeapMovable(friend);
      LglsComRemoval(friend);
      LglsComOwnershipDeepEmergent(friend);
      LglsComEmplacement(friend);
      LglsComIterationOperators(friend);

      /// Get entry array if containing pointers (inner)                      
      ///   @attention may be uninitialized                                   
      template<Cid SID = 0>
      constexpr decltype(auto) GetEntriesInner(this auto&& self) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::GetEntriesInner();
      }

      /// Set the entry array (inner)                                         
      template<Cid SID = 0>
      constexpr void SetEntriesInner(this auto& self, auto entries) noexcept {
         using C = typename Subcomponents::template At<SID>;
         self.C::SetEntriesInner(entries);
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
      template<Cid SID = 0>
      constexpr void ConstructHeapRequestPerDimension(this auto& self) noexcept 
      if_inherits(ConstructHeapRequestPerDimension()) {
         using C = typename Subcomponents::template At<SID>;
         if_available(self.C::ConstructHeapRequestPerDimension());
      }

      /// Deep-reference an element                                           
      ///   @attention works on one dimension at a time!                      
      template<bool FIND_MISSING = false, Cid SID = 0>
      constexpr void KeepElementDeep(this auto& self) assumptious {
         using C = typename Subcomponents::template At<SID>;
         self.C::template KeepElementDeep<FIND_MISSING>();
      }

      /// Deep-dereference (and eventually destroy) an element                
      ///   @attention works on one dimension at a time!                      
      template<bool DESTROY = true, Cid SID = 0>
      constexpr void DestroyElementDeep(this auto& self) assumptious {
         using C = typename Subcomponents::template At<SID>;
         self.C::template DestroyElementDeep<DESTROY>();
      }

      template<Cid SID = 0>
      void EmplaceEntries(this auto& self, auto&& intent) {
         using C = typename Subcomponents::template At<SID>;
         self.C::EmplaceEntries(LglsFwd(intent));
      }

      template<Cid SID = 0>
      void ResetEntries(this auto& self) {
         using C = typename Subcomponents::template At<SID>;
         self.C::ResetEntries();
      }

      #undef if_inherits
   };
}

LglsDisableWarningPop
