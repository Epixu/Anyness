///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Own.hpp"
#include "../../../source/components/Ownership-Stack.hpp"


namespace Langulus::Anyness::Inner
{

   template<CT::Sparse T>
   using RefBase = Container<
      Component::Stack<T>,              // Data on the heap             
      Component::OwnershipStack<>,      // Allocation is referenced     
      Component::TypedStatic<DMeta, T>, // Statically typed             
      Component::Assignment,            // Can be reassigned            
      Component::Comparison             // Can be compared              
   >;

} // namespace Langulus::Anyness::Inner

namespace Langulus::Anyness
{

   template<class T>
   struct Ref;

   ///                                                                        
   /// A statically typed shared pointer                                      
   /// Works fine with packed pointers as well                                
   /// Has ownership                                                          
   ///                                                                        
   template<CT::Sparse T>
   struct Ref<T> : Inner::RefBase<T> {
      using Base = Inner::RefBase<T>;

      constexpr Ref() noexcept = default;
      explicit constexpr Ref(const Ref&) noexcept = default;
      explicit constexpr Ref(Ref&&) noexcept = default;

      template<template<class> class S> requires CT::IntentConstructible<S, T>
      explicit constexpr Ref(S<Ref>&& other)
         : Base {other.template Forward<Base>()} {}

      template<class A> requires CT::ConstructibleFrom<T, A>
      constexpr Ref(A&& pointer) {
         EmplaceWithIntent(::std::forward<A>(pointer));
      }

      constexpr ~Ref() = default;
   };

   /// A dense ref is isomorphic to a simple Own container                    
   //template<CT::Dense T>
   //struct Ref<T> : Own<T> {};

} // namespace Langulus::Anyness
