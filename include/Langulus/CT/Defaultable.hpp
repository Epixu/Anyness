///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Abstract.hpp"


namespace Langulus::CT
{
   /// Checks whether all T are default-constructible                         
   /// You can make them default-constructible if you add a default           
   /// constructor. You can disable default-construction automatically, if    
   /// you make your type CT::Abstract                                        
   template<class...T>
   concept Defaultable = PartialValidate<T...> and ((
       not CT::Abstract<Decvq<Deref<Shed<T>>>>
       and requires { Decvq<Deref<Shed<T>>> {}; }
      ) and ...);
   
   template<class...T>
   concept NotDefaultable = PartialValidate<T...> and ((
       CT::Abstract<Decvq<Deref<Shed<T>>>>
       or not requires { Decvq<Deref<Shed<T>>> {}; }
      ) and ...);
}
