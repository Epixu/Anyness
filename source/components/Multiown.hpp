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
      using Subcomponents  = decltype( Types<TN...>::Discard([]<class C>{ return requires { C::SkipThisComponent; }; }));
      using Id             = decltype(Subcomponents::Extract([]<class C>{ return typename C::Id{}; }));

      static_assert(Subcomponents::ForEachIndexedAnd([]<class C, size_t I> {
         return C::Id::Count == 1 and C::Id::First == I; }),
         "Each enabled subcomponent needs to be dedicated to their single dimension, "
         "and all subcomponents need to be sequential"
      );

      static constexpr int ComponentPrecedence = 1000;
      static_assert(Subcomponents::ForEachAnd([]<class C> { return C::ComponentPrecedence == 1000; }),
         "All precedences should match");
      
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

      /// Default-initialize the component                                    
      ///   @attention this will not dereference previous allocation          
      constexpr void ConstructDefault(this auto& self) noexcept {
         Subcomponents::ForEach([&]<class C> noexcept {
            self.C::ConstructDefault();
         });
      }
      
      /// Transfer from any kind of container, respecting intents             
      ///   @attention this will not dereference previous allocation          
      ///   @param self deduced this                                          
      ///   @param intent the intent and container to transfer from           
      ///   @important notice that Copy and Clone intents are not handled     
      ///      here. They're handled in heap components instead, in case      
      ///      something throws an exception while constructing.              
      constexpr void ConstructFrom(this auto& self, auto&& intent) {
         Subcomponents::ForEach([&]<class C> {
            self.C::ConstructFrom(LglsFwd(intent));
         });
      }

      
      /// Called on container destruction                                     
      ///   @attention this never modifies any state                          
      constexpr void Destroy(this auto& self) noexcept {
         Subcomponents::ForEach([&]<class C> noexcept {
            self.C::Destroy();
         });
      }

      /// Reference the allocation once.                                      
      /// If container has DeepOwnership component, all entries will be       
      /// individually referenced as well.                                    
      template<Cid SID = 0>
      constexpr void Keep(this auto& self) noexcept {
         using C = typename Subcomponents::template At<SID>;
         self.C::Keep();
      }

      /// Dereference memory block once and destroy all elements if data was  
      /// fully dereferenced                                                  
      ///   @attention this never modifies any state                          
      template<Cid SID = 0>
      constexpr void Free(this auto& self) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::Free();
      }
      
      /// Destroy the first element                                           
      ///   @attention doesn't perform any referencing or indirection         
      ///   @attention assumes first element is validly constructed           
      ///   @attention does not modify any container state                    
      template<Cid SID = 0>
      constexpr void DestroyElementShallow(this auto& self) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::DestroyElementShallow();
      }
   };
}

LglsDisableWarningPop
