///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Integer.hpp"
#include "Real.hpp"


namespace Langulus::CTTI
{
   /// Can be used in two ways to satisfy CT::Number<T>:                      
   /// All custom/built-in integers and real numbers are CT::Number           
   /// @attention bool types are not considered numbers                       
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Number = Yes<>;` in T                      
   template<class T>
   struct Number {
      static constexpr bool Default = true;
      static constexpr bool Enabled = CT::Integer<T> or CT::Real<T>;
   };
}

LANGULUS_CTTI_CONCEPT_DECVQ(Number);
