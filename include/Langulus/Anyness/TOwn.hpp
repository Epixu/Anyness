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
#include "../../../source/components/Typed-Static.hpp"
#include "../../../source/components/Assignment.hpp"
#include "../../../source/components/Comparison.hpp"
#include "../../../source/rtti/MetaData.hpp"


namespace Langulus::Anyness
{

   ///                                                                        
   /// A statically typed stack-based container of size 1                     
   /// Mainly serves to transfer values and/or pointers on move               
   /// No ownership or states are applied - if you need those use TAny instead
   ///                                                                        
   template<CT::NotVoid T>
   struct TOwn : Container<
      Component::Stack<T>,             // Element on the stack          
      Component::TypedStatic<DMeta, T>,// Statically typed              
      Component::Assignment,           // Allows for reassignment       
      Component::Comparison            // Can be compared               
   > {};

} // namespace Langulus::Anyness
