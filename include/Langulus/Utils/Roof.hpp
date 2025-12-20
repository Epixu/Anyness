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

      if consteval {
         T n = x;
         --n;
         n |= n >> 1;
         n |= n >> 2;
         n |= n >> 4;
         if constexpr (sizeof(T) > 1)
            n |= n >> 8;
         if constexpr (sizeof(T) > 2)
            n |= n >> 16;
         if constexpr (sizeof(T) > 4)
            n |= n >> 32;
         static_assert(sizeof(T) <= 8, "Not implemented");

         ++n;
         return n;
      }
      else {
         // Pick a well optimized intrinsic function if not constexpr   
         return x <= 1 ? x : static_cast<T>((T {1}) <<
            static_cast<T>(sizeof(T) * 8 - ::std::countl_zero(static_cast<T>(x - 1))));
      }
   }
}
