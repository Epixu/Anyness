///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../../../source/components/Heap-Reference.hpp"
#include "../../../source/components/Ownership-Stack.hpp"
#include "../../../source/components/Typed-Static.hpp"
#include "../../../source/components/Assignment.hpp"
#include "../../../source/components/Comparison.hpp"
#include "TOwn.hpp"
#include "TRef.hpp"


namespace Langulus::Anyness
{

   ///                                                                        
   /// Either an embedded element, or one on the stack                        
   ///                                                                        
   template<class T> struct THandle;


   ///                                                                        
   /// When T is a dense reference, then element is embedded inside container 
   ///   @tparam T - the handle type                                          
   ///                                                                        
   template<CT::Reference T> requires CT::Dense<T>
   struct THandle<T> : Container<
      Component::HeapReference<>,
      Component::TypedStatic<DMeta, Deref<T>>,
      Component::Assignment<>,
      Component::Comparison
   > {
      using CTTI_Handle = Yes;
      using CTTI_Typed = Deref<T>;
      using CTTI_ReflectAs = void;

      using Base = Container<
         Component::HeapReference<>,
         Component::TypedStatic<DMeta, Deref<T>>,
         Component::Assignment<>,
         Component::Comparison
      >;

      ///                                                                     
      /// Construction                                                        
      THandle() = delete;
      explicit constexpr THandle(const THandle&) noexcept = default;
      explicit constexpr THandle(THandle&&) noexcept = default;

      /// Intent constructor                                                  
      template<template<class> class S> requires CT::IntentConstructible<S, T>
      explicit constexpr THandle(S<THandle>&& other)
         : Base {other.template Forward<Base>()} {}

      using Component::Comparison::operator ==;
   };
   

   ///                                                                        
   /// When T is a sparse reference, then pointer is embedded inside container
   /// Deep ownership will be managed on reassignment                         
   ///   @tparam T - the handle type                                          
   ///                                                                        
   template<CT::Reference T> requires CT::Sparse<T>
   struct THandle<T> : Container<
      Component::HeapReference<>,
      Component::OwnershipStack<0, false>,
      Component::TypedStatic<DMeta, Deref<T>>,
      Component::Assignment<>,
      Component::Comparison
   > {
      using CTTI_Handle = Yes;
      using CTTI_Typed = Deref<T>;
      using CTTI_ReflectAs = void;

      using Base = Container<
         Component::HeapReference<>,
         Component::OwnershipStack<0, false>,
         Component::TypedStatic<DMeta, Deref<T>>,
         Component::Assignment<>,
         Component::Comparison
      >;

      ///                                                                     
      /// Construction                                                        
      THandle() = delete;
      explicit constexpr THandle(const THandle&) noexcept = default;
      explicit constexpr THandle(THandle&&) noexcept = default;

      /// Intent constructor                                                  
      template<template<class> class S> requires CT::IntentConstructible<S, T>
      explicit constexpr THandle(S<THandle>&& other)
         : Base {other.template Forward<Base>()} {}

      using Component::Comparison::operator ==;
   };


   ///                                                                        
   /// When T is not a reference, then it is not embedded                     
   /// Such dense handles are isomorphic to TOwn<T>                           
   ///   @tparam T - the handle type                                          
   ///                                                                        
   template<CT::NotReference T> requires CT::Dense<T>
   struct THandle<T> : TOwn<T> {
      using CTTI_Handle = Yes;
      using CTTI_ReflectAs = void;
   };
   

   ///                                                                        
   /// When T is not a reference, then it is not embedded                     
   /// Such sparse handles are isomorphic to TRef<T>                          
   ///   @tparam T - the handle type                                          
   ///                                                                        
   template<CT::NotReference T> requires CT::Sparse<T>
   struct THandle<T> : TRef<T> {
      using CTTI_Handle = Yes;
      using CTTI_ReflectAs = void;
   };

} // namespace Langulus::Anyness

