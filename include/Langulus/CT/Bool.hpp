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
   /// Can be used in two ways to satisfy CT::Bool<T>:                        
   /// `bool` is CT::Bool by default                                          
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Bool = Yes<>;` in T                        
   template<class T>
   struct Bool {
      static constexpr bool Default = true;
      static constexpr bool Enabled = ::std::same_as<T, bool>;
   };
}

LANGULUS_CTTI_CONCEPT_DECVQ(Bool);
