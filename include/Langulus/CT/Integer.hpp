///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Bool.hpp"


namespace Langulus::CTTI
{

   /// Can be used in two ways to satisfy CT::Integer<T>:                     
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Integer = Yes;` in T                       
   template<class T>
   struct Integer {
      static constexpr bool Enabled = ::std::integral<T> and not CT::Bool<T>;
   };
   
} // namespace Langulus::CTTI

LANGULUS_CTTI_CONCEPT(Integer);