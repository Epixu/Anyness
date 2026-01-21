///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "Main.hpp"
#include <Langulus/Sequence.hpp>

using namespace Langulus;


TEST_CASE_TEMPLATE("Sequences"/*, "[sequence]"*/, T
   , int
   , unsigned long long
) {
   using s = Sequence<T{50}>;
   int counter_noexcept = 0;
   s::ForEach([&]<auto IDX> noexcept {      
      REQUIRE(IDX == counter_noexcept);
      ++counter_noexcept;
   });
   REQUIRE(counter_noexcept == 50);

   T counter = 0;
   s::ForEach([&]<auto IDX> {      
      REQUIRE(IDX == counter);
      ++counter;
   });
   REQUIRE(counter == 50);

   T counter2 = 0;
   LglsSequence(20, {
      ((counter2 += I), ...);
   });
   REQUIRE(counter2 == 190);
   
   T counter2_noexcept = 0;
   LglsSequence(20, noexcept {
      ((counter2_noexcept += I), ...);
   });
   REQUIRE(counter2_noexcept == 190);
}