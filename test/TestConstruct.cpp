///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#include <Anyness/Construct.hpp>
#include "Common.hpp"


SCENARIO("Constructs", "[construct]") {
   static Allocator::State memoryState;

	GIVEN("A complex descriptor") {
      Many descriptor;

		WHEN("Normalized") {
			Construct c;

			THEN("The requirements should be met") {
				REQUIRE(true);
			}
		}
	}

   REQUIRE(memoryState.Assert());
}
