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
   template<class...> struct Multiown;

   template<CT::Component...TN> requires (CountEnabled<TN...> == 0)
   struct Multiown<TN...> {
      using CTTI_Component = Yes<>;
      static constexpr bool SkipThisComponent = true;
   };

   ///                                                                        
   /// Combines multiple ownership components into a unified interface to     
   /// combat C++ base method ambiguities, and to add a bit more convenience. 
   ///   @tparam TN... all the ownership components to unify                  
   template<CT::Component...TN> requires (CountEnabled<TN...> >= 2)
   struct LANGULUS_EBCO Multiown<TN...> : TN... {
      using CTTI_Component = Yes<>;
      using CTTI_ReflectAs = void;
      using Subcomponents  = decltype( Types<TN...>::Discard([]<class C> static { return requires { C::SkipThisComponent; }; }));
      using Id             = decltype(Subcomponents::Extract([]<class C> static { return typename C::Id{}; }));

      static_assert(Subcomponents::ForEachIndexedAnd([]<class C, size_t I> {
         return C::Id::Count == 1 and C::Id::First == I; }),
         "Each enabled subcomponent needs to be dedicated to their single dimension, "
         "and all subcomponents need to be sequential"
      );

      static constexpr int ComponentPrecedence = 1000;
      static_assert(Subcomponents::ForEachAnd([]<class C> { return C::ComponentPrecedence == 1000; }),
         "All precedences should match");

      static constexpr uint Owned = Subcomponents::First::Owned;
      static_assert(Subcomponents::ForEachAnd([]<class C> { return C::Owned == Owned; }),
         "Currently all shallow ownerships must be of the same style");

      /// Get the allocation                                                  
      template<Cid SID = 0>
      constexpr auto GetAllocation(this auto const& self) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::GetAllocation();
      }

      /// Get the memory reference count                                      
      template<Cid SID = 0>
      constexpr auto GetUses(this auto const& self) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::GetUses();
      }

      /// Shallow-copy all initialized elements in memory to another          
      /// allocation, that is owned once only by this container.              
      ///   @attention if we already own the memory just Keep() it once       
      template<Cid SID = 0>
      void TakeOwnership(this auto& self) {
         using C = typename Subcomponents::template At<SID>;
         self.C::TakeOwnership();
      }

   protected:
      LglsComHeapReference(friend);
      LglsComHeapMovable(friend);
      LglsComRemoval(friend);
      LglsComEmplacement(friend);

      /// Get allocation (inner)                                              
      ///   @attention may be uninitialized                                   
      template<Cid SID = 0>
      constexpr decltype(auto) GetAllocationInner(this auto&& self) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::GetAllocationInner();
      }
      
      /// Set allocation (inner)                                              
      ///   @attention this will not dereference previous allocation          
      template<Cid SID = 0>
      constexpr void SetAllocationInner(this auto& self, Allocation const* a) noexcept {
         using C = typename Subcomponents::template At<SID>;
         self.C::SetAllocationInner(a);
      }

      /// Automatically set the allocation by searching for it using the heap 
      /// pointer. If allocation wasn't found, it will be set to nullptr.     
      ///   @attention this will not dereference previous allocation          
      template<Cid SID = 0>
      void FindAllocationInner(this auto& self) noexcept {
         using C = typename Subcomponents::template At<SID>;
         self.C::FindAllocationInner();
      }

      /// Resets allocation and all of its derivatives                        
      template<Cid SID = 0>
      constexpr void ResetAllocationInner(this auto&& self) noexcept {
         using C = typename Subcomponents::template At<SID>;
         self.C::ResetAllocationInner();
      }

      /// Resets all allocations                                              
      constexpr void ResetAllAllocations(this auto&& self) noexcept {
         Subcomponents::ForEach([&]<class C> noexcept {
            if_available_gcc(C::ResetAllocationInner)();
         });
      }

      /// Default-initialize the component                                    
      ///   @attention this will not dereference previous allocation          
      template<class SELF>
      constexpr void ConstructDefault(this SELF& self) noexcept {
         Subcomponents::ForEach([&]<class C> noexcept {
            if_available_gcc(C::template ConstructDefault<SELF>)();
         });
      }
      
      /// Transfer from any kind of container, respecting intents             
      ///   @attention this will not dereference previous allocation          
      ///   @param self deduced this                                          
      ///   @param intent the intent and container to transfer from           
      ///   @important notice that Copy and Clone intents are not handled     
      ///      here. They're handled in heap components instead, in case      
      ///      something throws an exception while constructing.              
      template<class SELF, CT::Intent I> requires CT::Container<I>
      constexpr void ConstructFrom(this SELF& self, I&& intent) {
         Subcomponents::ForEach([&]<class C> {
            if_available_gcc(C::template ConstructFrom<SELF, I>)(LglsFwd(intent));
         });
      }
      
      /// Called on container destruction                                     
      ///   @attention this never modifies any state                          
      template<class SELF>
      constexpr void Destroy(this SELF& self) assumptious {
         Subcomponents::Reverse::ForEach([&]<class C> assumptious{
            if_available_gcc(C::template Destroy<SELF>)();
         });
      }

      /// Reference all allocations once.                                     
      template<class SELF>
      constexpr void Keep(this SELF& self) assumptious {
         Subcomponents::ForEach([&]<class C> assumptious{
            if_available_gcc(C::template Keep<SELF>)();
         });
      }

      /// Dereference all allocations once, optionally deallocate             
      template<bool DEALLOCATE = true, class SELF>
      constexpr void Free(this SELF& self) assumptious {
         Subcomponents::Reverse::ForEach([&]<class C> assumptious{
            if_available_gcc(C::template Free<DEALLOCATE, SELF>)();
         });
      }
   };
}

LglsDisableWarningPop
