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
#include "../../../source/components/Typed-Stack.hpp"
#include "../../../source/components/Assignment.hpp"
#include "../../../source/components/Comparison.hpp"


namespace Langulus::Anyness
{

   ///                                                                        
   /// A type-erased mutable handle                                           
   /// It refers to a picked element inside a type-erased container           
   ///                                                                        
   struct HandleMut : Container<
      Component::HeapReference<>,
      Component::OwnershipStack<0, false>,
      Component::TypedStack<DMeta>,
      Component::Assignment<>,
      Component::Comparison
   > {
      using CTTI_Handle = Yes;
      using CTTI_ReflectAs = void;

      HandleMut() = delete;
   };
   

   ///                                                                        
   /// A type-erased immutable handle                                         
   /// It refers to a picked element inside a type-erased container           
   ///                                                                        
   struct Handle : Container<
      Component::HeapReference<>,
      Component::OwnershipStack<0, false>,
      Component::TypedStack<DMeta>,
      Component::Comparison
   > {
      using CTTI_Handle = Yes;
      using CTTI_ReflectAs = void;

      Handle() = delete;
   };
   
} // namespace Langulus::Anyness

