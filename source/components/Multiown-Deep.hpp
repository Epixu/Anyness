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

      static constexpr uint OwnedDeep = Subcomponents::First::OwnedDeep;
      static_assert(Subcomponents::ForEachAnd([]<class C> { return C::OwnedDeep == OwnedDeep; }),
         "Currently all deep ownerships must be of the same style");

      #define if_inherits(...) requires requires { self.C::__VA_ARGS__; }

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
      template<Cid SID = 0, class C = typename Subcomponents::template At<SID>>
      constexpr decltype(auto) GetEntriesInner(this auto&& self) noexcept 
      if_inherits(GetEntriesInner()) {
         return self.C::GetEntriesInner();
      }

      /// Set the entry array (inner)                                         
      template<Cid SID = 0, class C = typename Subcomponents::template At<SID>>
      constexpr void SetEntriesInner(this auto& self, auto entries) noexcept 
      if_inherits(GetEntriesInner()) {
         self.C::SetEntriesInner(entries);
      }

      /// Called on container destruction                                     
      ///   @attention this never modifies any state                          
      template<class SELF>
      constexpr void Destroy(this SELF& self) noexcept {
         Subcomponents::Reverse::ForEach([&]<class C> noexcept {
            if_available_gcc(C::template Destroy<SELF>)();
         });
      }

      /// Reference all entries once.                                         
      template<class SELF>
      constexpr void Keep(this SELF& self) noexcept {
         Subcomponents::ForEach([&]<class C> noexcept {
            if_available_gcc(C::template Keep<SELF>)();
         });
      }

      /// Dereference all entries once, always deallocate fully dereferenced  
      template<bool DEALLOCATE = true, class SELF>
      constexpr void Free(this SELF& self) noexcept {
         Subcomponents::Reverse::ForEach([&]<class C> noexcept {
            if_available_gcc(C::template Free<DEALLOCATE, SELF>)();
         });
      }

      /// Default-initialization of this component                            
      template<class SELF>
      constexpr void ConstructDefault(this SELF& self) noexcept {
         Subcomponents::ForEach([&]<class C> noexcept {
            if_available_gcc(C::template ConstructDefault<SELF>)();
         });
      }

      /// Transfer from any kind of container.                                
      /// This is only a reference to the entries and is not allowed          
      /// to allocate any new memory, so all this does is copy the            
      /// pointer, ignoring any intents.                                      
      ///   @param intent the intent and container to transfer from           
      template<class SELF, CT::Intent I> requires CT::Container<I>
      constexpr void ConstructFrom(this SELF& self, I&& intent) noexcept {
         Subcomponents::ForEach([&]<class C> noexcept {
            if_available_gcc(C::template ConstructFrom<SELF, I>)(LglsFwd(intent));
         });
      }

      /// This method is called upon allocation to nullify all entries        
      /// for a specific dimension.                                           
      template<Cid SID = 0, class SELF, class C = typename Subcomponents::template At<SID>>
      constexpr void ConstructHeapRequestPerDimension(this SELF& self) noexcept
      if_inherits(ConstructHeapRequestPerDimension()) {
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
