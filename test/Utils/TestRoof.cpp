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
   const T results[]{1, 1, 2, 4, 4, 8, 8, 16, 16, 64, 128, 128, 128};

   WHEN("Roof2 is executed") {
      STATIC_REQUIRE(Roof2(0u) == 1u);
      STATIC_REQUIRE(Roof2(1u) == 1u);
      STATIC_REQUIRE(Roof2(2u) == 2u);
      STATIC_REQUIRE(Roof2(3u) == 4u);
      STATIC_REQUIRE(Roof2(4u) == 4u);
      STATIC_REQUIRE(Roof2(99u) == 128u);

      for (unsigned i = 0; i < sizeof(numbers) / sizeof(T); ++i) {
         if (numbers[i] <= 128 || sizeof(T) > 1) {
            REQUIRE(Roof2(numbers[i]) == results[i]);
         }
         else {
            REQUIRE_THROWS(Roof2(numbers[i]));
         }
      }

      #if LANGULUS(BENCHMARK)
         constexpr T limit = ::std::numeric_limits<T>::max() >> 1;

         for (volatile int i = 0; i < 10000; i += 1) {
            CTRACK_NAME_PERSIST("Test/Langulus::Roof2");
            [[maybe_unused]] volatile auto r = Roof2(static_cast<T>(i % limit));
         }

         for (volatile int i = 0; i < 10000; i += 1) {
            CTRACK_NAME("Test/std::bit_ceil");
            [[maybe_unused]] volatile auto r = ::std::bit_ceil(static_cast<T>(i % limit));
         }

         auto results = ctrack::result_get_detail_table();
         REQUIRE(results.check_highscore());
         REQUIRE(results.check_same("Test/Langulus::Roof2", "Test/std::bit_ceil"));
      #endif
   }
}
