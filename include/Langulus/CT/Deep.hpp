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
   /// Affects CT::Deep<T> and CT::Flat<T>                                    
   template<class T>
   struct Deep;
}

namespace Langulus::CT
{
   /// Checks whether all decayed T are marked as deep                        
   template<class...T>
   concept Deep = Validate<Decay<Shed<T>>...>
       and (LANGULUS_CTTI_CHECK(Decay<Deref<Shed<T>>>, Deep) and ...);
   
   /// Checks whether all decayed T are not marked as deep                    
   template<class...T>
   concept NotDeep = Validate<Decay<Shed<T>>...>
       and ((not LANGULUS_CTTI_CHECK(Decay<Deref<Shed<T>>>, Deep)) and ...);

   /// Same as CT::NotDeep                                                    
   template<class...T>
   concept Flat = NotDeep<T...>;
}
