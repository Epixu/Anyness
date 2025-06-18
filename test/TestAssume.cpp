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
         Assert(false, HERE(), "Message");
         AssertWarn(false, HERE(), "Message");
         AssumeUser(false, HERE(), "Message");
         AssumeUserWarn(false, HERE(), "Message");
         AssumeDev(false, HERE(), "Message");
         AssumeDevWarn(false, HERE(), "Message");
         Assume<0>(false, HERE(), "Message");
         AssumeWarn<0>(false, HERE(), "Message");
      }
      else {
         REQUIRE_THROWS(Assert(false, HERE(), "Message"));
         REQUIRE_NOTHROW(Assert(true, HERE(), "Message"));
         REQUIRE_NOTHROW(AssertWarn(false, HERE(), "Message"));
         REQUIRE_NOTHROW(AssertWarn(true, HERE(), "Message"));

         if constexpr (LANGULUS(SAFE) > 0)
            REQUIRE_THROWS(AssumeUser(false, HERE(), "Message"));
         else
            REQUIRE_NOTHROW(AssumeUser(false, HERE(), "Message"));
         REQUIRE_NOTHROW(AssumeUser(true, HERE(), "Message"));

         REQUIRE_NOTHROW(AssumeUserWarn(false, HERE(), "Message"));
         REQUIRE_NOTHROW(AssumeUserWarn(true, HERE(), "Message"));

         if constexpr (LANGULUS(SAFE) > 1)
            REQUIRE_THROWS(AssumeDev(false, HERE(), "Message"));
         else
            REQUIRE_NOTHROW(AssumeDev(false, HERE(), "Message"));
         REQUIRE_NOTHROW(AssumeDev(true, HERE(), "Message"));

         REQUIRE_NOTHROW(AssumeDevWarn(false, HERE(), "Message"));
         REQUIRE_NOTHROW(AssumeDevWarn(true, HERE(), "Message"));

         REQUIRE_THROWS(Assume<0>(false, HERE(), "Message"));
         REQUIRE_NOTHROW(Assume<0>(true, HERE(), "Message"));

         REQUIRE_NOTHROW(AssumeWarn<0>(false, HERE(), "Message"));
         REQUIRE_NOTHROW(AssumeWarn<0>(true, HERE(), "Message"));
      }

      return true;
   };

   test();
   static_assert(test());
}