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
   /// Can be used in two ways to satisfy CT::Nullable<T>:                    
   /// Pointers and fundamentals are nullable by default                      
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Nullable = Yes<>;` in T                    
   template<class T>
   struct Nullable {
      static constexpr bool Default = true;
      static constexpr bool Enabled = ::std::is_pointer_v<T> or ::std::is_fundamental_v<T>;
   };
}

LANGULUS_CTTI_CONCEPT_DECVQ(Nullable);
