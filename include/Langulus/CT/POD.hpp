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

   /// Can be used in two ways to satisfy CT::POD<T>:                         
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_POD = Yes;` in T                           
   template<class T>
   struct POD {
      static constexpr bool Value = not Abstract<T>::Value and (
         Fundamental<T>::Value or Sparse<T>::Value
         or (   ::std::is_trivial_v<T>
            and ::std::is_standard_layout_v<T>
            and ::std::is_destructible_v<T>)
      );
   };
   
} // namespace Langulus::CTTI

LANGULUS_CTTI_CONCEPT(POD);