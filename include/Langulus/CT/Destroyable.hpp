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
   /// Can be used in two ways to satisfy CT::Destroyable<T>:                 
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Destroyable = Yes/No;` in T                
   template<class T>
   struct Destroyable {
      static constexpr bool Enabled = not ::std::is_trivially_destructible_v<T>
                                      and ::std::is_destructible_v<T>;
   };
}

LANGULUS_CTTI_CONCEPT(Destroyable);