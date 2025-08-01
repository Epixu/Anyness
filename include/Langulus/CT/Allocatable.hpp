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
   /// Non-void types are allocatable by default                              
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Allocatable = Yes<>;` in T                 
   template<class T>
   struct Allocatable {
      static constexpr bool Default = true;
      static constexpr bool Enabled = not CT::Void<T>;
   };
}

LANGULUS_CTTI_CONCEPT_DECVQ(Allocatable);
