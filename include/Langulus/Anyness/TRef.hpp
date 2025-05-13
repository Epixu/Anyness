///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../../../source/Container.hpp"
#include "../../../source/components/Stack.hpp"
#include "../../../source/components/Ownership-Stack.hpp"
#include "../../../source/components/Typed-Static.hpp"
#include "../../../source/components/Assignment.hpp"
#include "../../../source/components/Comparison.hpp"
#include "../../../source/rtti/MetaData.hpp"


namespace Langulus::Anyness::Inner
{

   template<CT::Sparse T>
   using TRefBase = Container<
      Component::Stack<T>,              // Data on the heap             
      Component::OwnershipStack<>,      // Allocation is referenced     
      Component::TypedStatic<DMeta, T>, // Statically typed             
      Component::Assignment,            // Can be reassigned            
      Component::Comparison             // Can be compared              
   >;

} // namespace Langulus::Anyness::Inner

namespace Langulus::Anyness
{

   ///                                                                        
   /// A statically typed shared pointer                                      
   /// Works fine with packed pointers as well                                
   /// Has deep ownership, but no states are applied. You can use TAny        
   /// instead if you want encryption/compression/linking.                    
   ///                                                                        
   template<CT::Sparse T>
   struct TRef : Inner::TRefBase<T> {
      using Base = Inner::RefBase<T>;

      constexpr TRef() noexcept = default;
      explicit constexpr TRef(const TRef&) noexcept = default;
      explicit constexpr TRef(TRef&&) noexcept = default;

      template<template<class> class S> requires CT::IntentConstructible<S, T>
      explicit constexpr TRef(S<TRef>&& other)
         : Base {other.template Forward<Base>()} {}

      template<class A> requires CT::ConstructibleFrom<T, A>
      constexpr TRef(A&& pointer) {
         EmplaceWithIntent(::std::forward<A>(pointer));
      }

      constexpr ~TRef() = default;
   };

} // namespace Langulus::Anyness
