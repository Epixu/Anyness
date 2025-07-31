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
   /// Can be used in two ways to satisfy CT::Abstract<T>:                    
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Abstract = Yes<>;` in T                    
   template<class T>
   struct Abstract {
      static constexpr bool Enabled = ::std::is_abstract_v<T>;
   };
}

LANGULUS_CTTI_CONCEPT_DECVQ(Abstract);
