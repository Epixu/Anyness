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
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Number = Yes/No;` in T                     
   template<class T>
   struct Number {
      static constexpr bool Enabled = CT::Integer<T> or CT::Real<T>;
   };
   
} // namespace Langulus::CTTI

LANGULUS_CTTI_CONCEPT(Number);