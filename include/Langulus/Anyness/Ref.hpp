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


namespace Langulus::Anyness
{

   ///                                                                        
   /// A statically typed shared pointer                                      
   ///                                                                        
   template<CT::NotVoid T>
   struct Ref : Container<
      Component::HeapMovable<>,        // Data on the heap              
      Component::OwnershipStack<>,     // Allocation is referenced      
      Component::DeepOwnership<>,      // Referenced indirections       
      Component::TypedStatic<DMeta, T> // Statically typed              
   > {
      constexpr Ref() noexcept = default;
      explicit constexpr Ref(const Ref&);
      explicit constexpr Ref(Ref&&);

      template<template<class> class S> requires CT::IntentConstructible<S, T*>
      explicit constexpr Ref(S<Ref>&&);

      template<class A> requires CT::ConstructibleFrom<T*, A>
      constexpr Ref(A&&);

      constexpr ~Ref();
   };

} // namespace Langulus::Anyness
