///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../Typenav.hpp"


namespace Langulus::CTTI
{
   /// Can be used in two ways to satisfy CT::Allocatable<T>:                 
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Allocatable = Yes/No;` in T                
   template<class T>
   struct Allocatable {
      static constexpr bool Enabled = true;
   };
}

LANGULUS_CTTI_CONCEPT(Allocatable);
