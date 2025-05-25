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
#include "../../../source/components/Typed-Static.hpp"
#include "../../../source/components/Count-Static.hpp"
#include "../../../source/components/Emplacement.hpp"
#include "../../../source/components/Assignment.hpp"
#include "../../../source/components/Comparison.hpp"
#include "../../../source/rtti/MetaData.hpp"


namespace Langulus::Anyness::Inner
{

   ///                                                                        
   template<CT::Sparse T>
   using TRefBase = Container<
      Com::HeapMovable<>,                 // Data on the heap          
      Com::OwnershipStack<>,              // Allocation is referenced  
      Com::TypedStatic<DMeta, Deptr<T>>,  // Statically typed          
      Com::CountStatic<1u>,               // Statically sized          
      Com::Emplacement<>,                 // Can be emplaced           
      Com::Assignment<>,                  // Can be reassigned         
      Com::Comparison                     // Can be compared           
   >;

} // namespace Langulus::Anyness::Inner

namespace Langulus::Anyness
{

   ///                                                                        
   ///   A statically typed shared pointer                                    
   ///                                                                        
   ///   Works fine with packed pointers as well. Has deep ownership, but no  
   /// states are applied. You can use TAny instead if you want               
   /// encryption/compression/linking.                                        
   ///                                                                        
   template<CT::Sparse T>
   struct TRef : Inner::TRefBase<T> {
      using Base    = Inner::TRefBase<T>;
      using Pick    = THandle<T const&>;
      using PickMut = THandle<T&>;

      ///                                                                     
      ///   Construction                                                      
      constexpr TRef() noexcept = default;
      explicit constexpr TRef(const TRef&) noexcept = default;
      explicit constexpr TRef(TRef&&) noexcept = default;

      /// Intent constructor                                                  
      template<template<class> class S> requires CT::IntentConstructible<S, T>
      explicit constexpr TRef(S<TRef>&& other)
         : Base {other.template Forward<Base>()} {}

      /// Raw pointer constructor                                             
      /// The allocation behind the pointer will be sought                    
      ///   @param pointer - the pointer to initialize with                   
      template<class A> requires CT::ConstructibleFrom<T, A>
      constexpr TRef(A&& pointer) {
         if constexpr (CT::Null<A>) {
            (void) pointer;
            return;
         }
         else EmplaceWithIntent(::std::forward<A>(pointer));
      }

      constexpr ~TRef() = default;
   };

} // namespace Langulus::Anyness
