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
#include "../../../source/components/Assignment.hpp"
#include "../../../source/components/Typed-Static.hpp"


namespace Langulus::Anyness
{
   
   ///                                                                        
   /// A statically typed stack-based container of size 1                     
   /// Mainly serves to transfer values and/or pointers on move               
   template<CT::NotVoid T>
   struct Own : Container<
      Component::Stack<T>,             // Element on the stack          
      Component::TypedStatic<DMeta, T>,// Statically typed              
      Component::Assignment            // Allows for assignment         
   > {};

} // namespace Langulus::Anyness
