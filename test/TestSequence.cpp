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


SCENARIO("Sequences", "[sequence]") {
   using s = Sequence<50>;
   int counter_noexcept = 0;
   s::ForEach([&]<int IDX> noexcept {      
      REQUIRE(IDX == counter_noexcept);
      ++counter_noexcept;
   });
   REQUIRE(counter_noexcept == 50);

   int counter = 0;
   s::ForEach([&]<int IDX> {      
      REQUIRE(IDX == counter);
      ++counter;
   });
   REQUIRE(counter == 50);

   int counter2 = 0;
   LglsSequence(20, {
      ((counter2 += I), ...);
   });
   REQUIRE(counter2 == 200);
   
   int counter2_noexcept = 0;
   LglsSequence(20, noexcept {
      ((counter2_noexcept += I), ...);
   });
   REQUIRE(counter2_noexcept == 200);
}
