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
   /// All T satisfying std::integer are CT::Integer by default, with the     
   /// exception of custom and built-in boolean types                         
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Integer = Yes<>;` in T                     
   template<class T>
   struct Integer {
      static constexpr bool Default = true;
      static constexpr bool Enabled = ::std::integral<T> and not CT::Bool<T>;
   };
}

LANGULUS_CTTI_CONCEPT_DECVQ(Integer);
