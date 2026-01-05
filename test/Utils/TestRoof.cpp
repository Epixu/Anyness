///                                                                           
/// Langulus::Fractalloc                                                      
/// Copyright (c) 2015 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "../Main.hpp"
#include <Langulus/Utils/Roof.hpp>

using namespace Langulus;


TEMPLATE_TEST_CASE("Testing Roof2 calls", "[allocator]",
   uint8_t, uint16_t, uint32_t, uint64_t
) {
   using T = TestType;
   const T numbers[]{0, 1, 2, 3, 4, 5, 6, 11, 16, 64,  99, 120, 128};
   const T results[]{0, 1, 2, 4, 4, 8, 8, 16, 16, 64, 128, 128, 128};

   WHEN("Roof2 is executed") {
      for (unsigned i = 0; i < sizeof(numbers) / sizeof(T); ++i) {
         if (numbers[i] <= 128 || sizeof(T) > 1) {
            REQUIRE(Roof2(numbers[i]) == results[i]);
         }
         else {
            REQUIRE_THROWS(Roof2(numbers[i]));
         }
      }

      #if LANGULUS(BENCHMARK) // Last result: 
         ///TODO test if std::bit_ceil is better, benchmark it!
         BENCHMARK_ADVANCED("Roof2 with instrinsics") (timer meter) {
            meter.measure([&](int i) {
               return Roof2(static_cast<T>(i % 256));
               });
         };
         BENCHMARK_ADVANCED("Roof2 without intrinsics") (timer meter) {
            meter.measure([&](int i) {
               return Roof2cexpr(static_cast<T>(i % 256));
               });
         };
      #endif
   }
}
