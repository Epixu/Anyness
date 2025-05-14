///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../../../source/components/Stack.hpp"
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


   /// When T is a dense reference, then element is embedded inside container 
   ///   @tparam T - the handle type                                          
   template<CT::Reference T> requires CT::Dense<T>
   struct THandle<T> : Container<
      Component::Stack<Deref<T>*>,
      Component::TypedStatic<DMeta, Deref<T>>,
      Component::Assignment<>,
      Component::Comparison
   > {
      using CTTI_Handle = Yes;
      using CTTI_Typed = Deref<T>;
      using CTTI_ReflectAs = void;

      THandle() = delete;
   };
   

   /// When T is a sparse reference, then pointer is embedded inside container
   /// Deep ownership will be managed                                         
   ///   @tparam T - the handle type                                          
   template<CT::Reference T> requires CT::Sparse<T>
   struct THandle<T> : Container<
      Component::Stack<Deref<T>*>,
      Component::OwnershipStack<0, false>,
      Component::TypedStatic<DMeta, Deref<T>>,
      Component::Assignment<>,
      Component::Comparison
   > {
      using CTTI_Handle = Yes;
      using CTTI_Typed = Deref<T>;
      using CTTI_ReflectAs = void;

      THandle() = delete;
   };


   /// When T is not a reference, then it is not embedded                     
   /// Such dense handles are isomorphic to TOwn<T>                           
   ///   @tparam T - the handle type                                          
   template<CT::NotReference T> requires CT::Dense<T>
   struct THandle<T> : TOwn<T> {
      using CTTI_Handle = Yes;
      using CTTI_ReflectAs = void;
   };
   

   /// When T is not a reference, then it is not embedded                     
   /// Such sparse handles are isomorphic to TRef<T>                          
   ///   @tparam T - the handle type                                          
   template<CT::NotReference T> requires CT::Sparse<T>
   struct THandle<T> : TRef<T> {
      using CTTI_Handle = Yes;
      using CTTI_ReflectAs = void;
   };

} // namespace Langulus::Anyness

