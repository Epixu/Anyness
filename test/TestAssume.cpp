///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "Main.hpp"
#include <Langulus/Assume.hpp>

using namespace Langulus;


SCENARIO("Testing whether assumptions throw properly, in both constant-evaluated and not contexts", "[assume]") {
   constexpr auto test = [] {
      if consteval {
         LglsAssert(false, "Message");
         LglsAssertWarn(false, "Message");
         LglsAssumeUser(false, "Message");
         LglsAssumeUserWarn(false, "Message");
         LglsAssumeDev(false, "Message");
         LglsAssumeDevWarn(false, "Message");
         LglsAssume(0, false, "Message");
         LglsAssumeWarn(0, false, "Message");
      }
      else {
         REQUIRE_THROWS(LglsAssert(false, "Message"));
         REQUIRE_NOTHROW(LglsAssert(true, "Message"));
         REQUIRE_NOTHROW(LglsAssertWarn(false, "Message"));
         REQUIRE_NOTHROW(LglsAssertWarn(true, "Message"));

         if constexpr (LANGULUS(SAFE) > 0) {
            REQUIRE_THROWS(LglsAssumeUser(false, "Message"));
         }
         else {
            REQUIRE_NOTHROW(LglsAssumeUser(false, "Message"));
         }
         REQUIRE_NOTHROW(LglsAssumeUser(true, "Message"));

         REQUIRE_NOTHROW(LglsAssumeUserWarn(false, "Message"));
         REQUIRE_NOTHROW(LglsAssumeUserWarn(true, "Message"));

         if constexpr (LANGULUS(SAFE) > 1) {
            REQUIRE_THROWS(LglsAssumeDev(false, "Message"));
         }
         else {
            REQUIRE_NOTHROW(LglsAssumeDev(false, "Message"));
         }
         REQUIRE_NOTHROW(LglsAssumeDev(true, "Message"));

         REQUIRE_NOTHROW(LglsAssumeDevWarn(false, "Message"));
         REQUIRE_NOTHROW(LglsAssumeDevWarn(true, "Message"));

         REQUIRE_THROWS(LglsAssume(0, false, "Message"));
         REQUIRE_NOTHROW(LglsAssume(0, true, "Message"));

         REQUIRE_NOTHROW(LglsAssumeWarn(0, false, "Message"));
         REQUIRE_NOTHROW(LglsAssumeWarn(0, true, "Message"));
      }

      return true;
   };

   test();
   static_assert(test());
}
