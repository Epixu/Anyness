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
   /// Can be used in two ways to satisfy CT::Real<T>:                        
   /// By default, all T that satisfy std::floating_point are true            
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Real = Yes<>;` in T                        
   template<class T>
   struct Real {
      static constexpr bool Default = true;
      static constexpr bool Enabled = ::std::floating_point<T>;
   };
}

LANGULUS_CTTI_CONCEPT_DECVQ(Real);
