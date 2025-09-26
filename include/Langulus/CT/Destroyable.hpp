///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "POD.hpp"


namespace Langulus::CT
{
   /// Checks whether all T need their destructor to be called before         
   /// deallocating their storage. POD types are not destroyable              
   template<class...T>
   concept Destroyable = Validate<T...> and ((
          ::std::is_destructible_v<T> and not POD<T>
       ) and ...);
   
   template<class...T>
   concept NotDestroyable = Validate<T...> and ((
          not ::std::is_destructible_v<T> or POD<T>
       ) and ...);
}
