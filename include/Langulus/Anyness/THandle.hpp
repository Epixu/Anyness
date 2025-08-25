///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../../../source/components/Typed-Static.hpp"
#include "../../../source/components/Heap-Reference.hpp"
#include "../../../source/components/Count-Static.hpp"
#include "../../../source/components/DeepOwnership-Stack.hpp"
#include "../../../source/components/Assignment.hpp"
#include "../../../source/components/Emplacement.hpp"
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
         Com::TypedStatic<DMeta, Deref<T>>,
         Com::HeapReference<>,
         Com::CountStatic<1u>,
         Com::DeepOwnershipStack<>,
         Com::Assignment<>,
         Com::Emplacement<>,
         Com::Comparison,
         Com::IterationOperators<>
      >;
      
      template<CT::Reference T>
      using THandleDisownedBase = Container<
         Com::TypedStatic<DMeta, Deref<T>>,
         Com::HeapReference<>,
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
      using Base           = Inner::THandleBase<T>;
      
      using Base::Base;
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
      using Base           = Inner::THandleDisownedBase<T>;
      
      using Base::Base;
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
