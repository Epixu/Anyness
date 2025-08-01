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
   /// Can be used in two ways to satisfy CT::Deep<T>:                        
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Deep = Yes<>;` in T                        
   template<class T>
   struct Deep;
}

LANGULUS_CTTI_CONCEPT_DECVQ(Deep);

namespace Langulus::CT
{
   template<class...T>
   concept Flat = PartialValidate<T...> and (NotDeep<T> and ...);
}
