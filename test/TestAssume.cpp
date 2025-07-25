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
         AssumeUserInner(false, HERE(), "Message");
         AssumeUserWarnInner(false, HERE(), "Message");
         AssumeDevInner(false, HERE(), "Message");
         AssumeDevWarnInner(false, HERE(), "Message");
         Assume<0>(false, HERE(), "Message");
         AssumeWarn<0>(false, HERE(), "Message");
      }
      else {
         REQUIRE_THROWS(Assert(false, HERE(), "Message"));
         REQUIRE_NOTHROW(Assert(true, HERE(), "Message"));
         REQUIRE_NOTHROW(AssertWarn(false, HERE(), "Message"));
         REQUIRE_NOTHROW(AssertWarn(true, HERE(), "Message"));

         if constexpr (LANGULUS(SAFE) > 0)
            REQUIRE_THROWS(AssumeUserInner(false, HERE(), "Message"));
         else
            REQUIRE_NOTHROW(AssumeUserInner(false, HERE(), "Message"));
         REQUIRE_NOTHROW(AssumeUserInner(true, HERE(), "Message"));

         REQUIRE_NOTHROW(AssumeUserWarnInner(false, HERE(), "Message"));
         REQUIRE_NOTHROW(AssumeUserWarnInner(true, HERE(), "Message"));

         if constexpr (LANGULUS(SAFE) > 1)
            REQUIRE_THROWS(AssumeDevInner(false, HERE(), "Message"));
         else
            REQUIRE_NOTHROW(AssumeDevInner(false, HERE(), "Message"));
         REQUIRE_NOTHROW(AssumeDevInner(true, HERE(), "Message"));

         REQUIRE_NOTHROW(AssumeDevWarnInner(false, HERE(), "Message"));
         REQUIRE_NOTHROW(AssumeDevWarnInner(true, HERE(), "Message"));

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
