///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../../../source/components/Heap-Reference.hpp"
#include "../../../source/components/DeepOwnership-Stack.hpp"
#include "../../../source/components/Typed-Static.hpp"
#include "../../../source/components/Count-Static.hpp"
#include "../../../source/components/Assignment.hpp"
#include "../../../source/components/Comparison.hpp"
#include "../../../source/components/Iteration-Operators.hpp"
#include "TOwn.hpp"
#include "TRef.hpp"


namespace Langulus::Anyness
{
   namespace Inner
   {
      template<CT::Reference T>
      using THandleBase = Container<
         Com::HeapReference<>,
         Com::DeepOwnershipStack<>,
         Com::TypedStatic<DMeta, Deref<T>>,
         Com::CountStatic<1u>,
         Com::Assignment<>,
         Com::Emplacement<>,
         Com::Comparison,
         Com::IterationOperators<>
      >;
      
      template<CT::Reference T>
      using THandleDisownedBase = Container<
         Com::HeapReference<>,
         Com::TypedStatic<DMeta, Deref<T>>,
         Com::CountStatic<1u>,
         Com::Assignment<>,
         Com::Emplacement<>,
         Com::Comparison,
         Com::IterationOperators<>
      >;
   }

   template<class T> struct THandle;
   template<class T> struct THandleDisowned;


   ///                                                                        
   /// When T is a dense reference, then element is embedded inside container 
   ///   @attention memory is never (de)referenced upon construction and      
   ///      destruction - only on reassignment                                
   ///   @tparam T - the contained type                                       
   ///                                                                        
   template<CT::Reference T>
   struct THandle<T> : Inner::THandleBase<T> {
      using CTTI_Handle    = Yes<>;
      using CTTI_Typed     = Deref<T>;
      using CTTI_ReflectAs = void;
      using Base = Inner::THandleBase<T>;

      ///                                                                     
      /// Construction                                                        
      explicit constexpr THandle(THandle const&) noexcept = default;
      explicit constexpr THandle(THandle&&) noexcept = default;

      /// Intent constructor                                                  
      /*template<template<class> class S> requires CT::IntentConstructible<S, T>
      explicit constexpr THandle(S<THandle>&& other)
         : Base {other.template Forward<Base>()} {}*/

      /// Manual constructor                                                  
      ///   @param element - embedded element                                 
      /*explicit*/ constexpr THandle(Deref<T>* data, AllocationPtr* entry) noexcept
         : Base {Base::InitList::Expand, data, entry} {}

      using Com::Comparison::operator ==;
   };
   

   ///                                                                        
   /// When T is a dense reference, then element is embedded inside container 
   /// This handle never propagates or modifies ownership                     
   ///   @tparam T - the contained type                                       
   ///                                                                        
   template<CT::Reference T>
   struct THandleDisowned<T> : Inner::THandleDisownedBase<T> {
      using CTTI_Handle    = Yes<>;
      using CTTI_Typed     = Deref<T>;
      using CTTI_ReflectAs = void;
      using Base = Inner::THandleDisownedBase<T>;

      ///                                                                     
      /// Construction                                                        
      /*explicit*/ constexpr THandleDisowned(THandleDisowned const&) noexcept = default;
      /*explicit*/ constexpr THandleDisowned(THandleDisowned&&) noexcept = default;

      /// Intent constructor                                                  
      template<template<class> class S> requires CT::IntentConstructible<S, T>
      explicit constexpr THandleDisowned(S<THandleDisowned>&& other)
         : Base {other.template Forward<Base>()} {}

      /// Manual constructor                                                  
      ///   @param element - embedded element                                 
      /*explicit*/ constexpr THandleDisowned(Deref<T>* element) noexcept 
         : Base {Base::InitList::Expand, element} {}

      using Com::Comparison::operator ==;
   };
   

   ///                                                                        
   /// When T is not a reference, then it is not embedded                     
   /// Such dense handles are isomorphic to TOwn<T> - data is on the stack    
   ///   @tparam T - the contained type                                       
   ///                                                                        
   template<CT::NotReference T> requires CT::Dense<T>
   struct THandle<T> : TOwn<T> {
      using CTTI_Handle    = Yes<>;
      using CTTI_ReflectAs = void;
   };
   

   ///                                                                        
   /// When T is not a reference, then it is not embedded                     
   /// Such sparse handles are isomorphic to TRef<T>                          
   ///   @tparam T - the contained type                                       
   ///                                                                        
   template<CT::NotReference T> requires CT::Sparse<T>
   struct THandle<T> : TRef<T> {
      using CTTI_Handle    = Yes<>;
      using CTTI_ReflectAs = void;
   };
}
