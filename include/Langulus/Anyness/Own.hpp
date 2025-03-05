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
#include "../../../source/components/Count-Static.hpp"
#include "../../../source/components/Typed-Static.hpp"


namespace Langulus::Anyness
{
   
   /// A statically typed stack-based container of size 1                     
   template<CT::NotVoid T>
   struct Own : Detail::Container<
      Component::Stack<T>,             // Element on the stack          
      Component::TypedStatic<T>,       // Statically typed              
      Component::CountStatic<1>        // Statically sized to 1         
   > {};

} // namespace Langulus::Anyness
