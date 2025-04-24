///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../../../source/Container.hpp"
#include "../../../source/components/Heap-Movable.hpp"
#include "../../../source/components/Ownership-Stack.hpp"
#include "../../../source/components/DeepOwnership.hpp"
#include "../../../source/components/Typed-Static.hpp"
#include "../../../source/rtti/MetaData.hpp"


namespace Langulus::Anyness::Inner
{

   template<CT::NotVoid T>
   using RefBase = Container<
      Component::HeapMovable<>,        // Data on the heap              
      Component::OwnershipStack<>,     // Allocation is referenced      
      Component::DeepOwnership<>,      // Referenced indirections       
      Component::TypedStatic<DMeta, T> // Statically typed              
   >;

} // namespace Langulus::Anyness::Inner

namespace Langulus::Anyness
{

   ///                                                                        
   /// A statically typed shared pointer                                      
   ///                                                                        
   template<CT::NotVoid T>
   struct Ref : Inner::RefBase<T> {
      using Base = Inner::RefBase<T>;

      constexpr Ref() noexcept = default;
      explicit constexpr Ref(const Ref&) noexcept = default;
      explicit constexpr Ref(Ref&&) noexcept = default;

      template<template<class> class S> requires CT::IntentConstructible<S, T*>
      explicit constexpr Ref(S<Ref>&& other)
         : Base {other.template Forward<Base>()} {}

      template<class A> requires CT::ConstructibleFrom<T*, A>
      constexpr Ref(A&& pointer) {
         EmplaceWithIntent(::std::forward<A>(pointer));
      }

      constexpr ~Ref() = default;
   };

} // namespace Langulus::Anyness
