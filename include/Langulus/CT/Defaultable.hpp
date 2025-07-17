///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Abstract.hpp"


namespace Langulus::CTTI
{
   /// Can be used in two ways to satisfy CT::Defaultable<T>:                 
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Defaultable = Yes;` in T                   
   template<class T>
   struct Defaultable {
      static constexpr bool Enabled = not CT::Abstract<T> and requires { T {}; };
   };
}

LANGULUS_CTTI_CONCEPT(Defaultable);