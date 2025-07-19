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
   ///   @tparam SAFE - set to true if you want it to throw on overflow       
   ///   @param x - the unsigned integer to round up                          
   ///   @return the closest upper power-of-two to x                          
   template<class T> LANGULUS(ALWAYS_INLINED)
   constexpr T Roof2(const T x) IF_UNSAFE(noexcept) {
      #if LANGULUS(SAFE)
         static_assert(CT::Unsigned<T>, "T should be unsigned");
         constexpr T lastPowerOfTwo = (T {1}) << (T {sizeof(T) * 8 - 1});
         AssumeDev(x <= lastPowerOfTwo, HERE(), "Roof2 overflowed");
      #endif

      // Pick a well optimized intrinsic function if not constexpr      
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
         return x <= 1 ? x : static_cast<T>((T {1}) << 
            static_cast<T>(sizeof(T) * 8 - ::std::countl_zero(static_cast<T>(x - 1))));
      }
   }
}
