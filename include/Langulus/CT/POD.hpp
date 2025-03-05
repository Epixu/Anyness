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
      static constexpr bool Enabled = not Abstract<T>::Enabled and (
         sizeof(T) == 1 or Sparse<T>::Enabled or Fundamental<T>::Enabled
      );
   };
   
} // namespace Langulus::CTTI

LANGULUS_CTTI_CONCEPT(POD);