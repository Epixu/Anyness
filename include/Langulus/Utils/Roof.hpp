///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../Core.hpp"
#include <bit>

#if LANGULUS(SAFE)
   #include "../Assume.hpp"
   #include "../CT/Signed.hpp"
#endif

namespace Langulus
{
   /// Round to the upper power-of-two                                        
   ///   @param x the unsigned integer to round up                            
   ///   @return the closest upper power-of-two to x                          
   template<class T> LANGULUS(ALWAYS_INLINED)
   constexpr T Roof2(const T x) assumptious {
      #if LANGULUS(SAFE)
         static_assert(CT::Unsigned<T>, "T should be unsigned");
         constexpr T lastPowerOfTwo = (T {1}) << (T {sizeof(T) * 8 - 1});
         LglsAssumeDev(x <= lastPowerOfTwo, "Roof2 overflowed");
      #endif
      return ::std::bit_ceil(x);
   }
}
