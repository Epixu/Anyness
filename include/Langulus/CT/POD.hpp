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

   /// Affects CT::POD<T>                                                     
   template<class T>
   struct POD {
      static constexpr bool Enabled = not CT::Abstract<T> and (
         sizeof(T) == 1 or CT::Sparse<T> or CT::Fundamental<T>
      );
   };
   
} // namespace Langulus::CTTI

LANGULUS_CTTI_CONCEPT(POD);