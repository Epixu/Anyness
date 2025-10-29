///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include <cstdint>


namespace Langulus
{
   /// An unsigned power-of-two number, represented by the index of the       
   /// most significant bit. Supports numbers in the range [0; 2^255]         
   struct pot_t {
      uint8_t bit;
   };
}
