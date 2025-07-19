///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../Core.hpp"
#include <concepts>


namespace Langulus::CT
{
   /// Check if any T is the built-in one that signifies lack of support      
   template<class...T>
   concept Unsupported = (::std::same_as<::Langulus::Unsupported, T> or ...);

   /// Check if all T are supported                                           
   template<class...T>
   concept Supported = ((not Unsupported<T>) and ...);
}
