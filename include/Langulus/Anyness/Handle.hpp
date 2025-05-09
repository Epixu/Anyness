///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../../../source/components/Stack.hpp"
#include "../../../source/components/Heap-Movable.hpp"
#include "../../../source/components/Ownership-Heap.hpp"
#include "../../../source/components/DeepOwnership.hpp"
#include "../../../source/components/Assignment.hpp"
#include "../../../source/components/Typed-Static.hpp"


namespace Langulus::Anyness
{

   template<class T> struct Handle;
   template<class T> struct HandleLocal;

   template<CT::Dense T>
   struct Handle<T> : Container<
      Component::Stack<T&>,
      Component::TypedStatic<DMeta, T>,
      Component::Assignment
   > {
      Handle() = delete;
   };
   
   template<CT::Sparse T>
   struct Handle<T> : Container<
      Component::Stack<T&>,
      Component::OwnershipStack<0, false>,
      Component::DeepOwnership<>,
      Component::TypedStatic<DMeta, T>,
      Component::Assignment
   > {
      Handle() = delete;
   };
   
   template<CT::Dense T>
   struct HandleLocal<T> : Container<
      Component::Stack<T>,
      Component::TypedStatic<DMeta, T>,
      Component::Assignment
   > {
      HandleLocal() = delete;
   };
   
   template<CT::Sparse T>
   struct HandleLocal<T> : Container<
      Component::Stack<T>,
      Component::OwnershipStack<>,
      Component::DeepOwnership<>,
      Component::TypedStatic<DMeta, T>,
      Component::Assignment
   > {
      HandleLocal() = delete;
   };

} // namespace Langulus::Anyness

