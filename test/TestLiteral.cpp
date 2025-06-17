///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "Main.hpp"
#include <Langulus/Literal.hpp>
#include <concepts>
#include <string>
#include <string_view>

using namespace Langulus;

namespace
{
   Literal fixedString = "Test String";
   ::std::string justString = "Test String";
   const char carrayString[] = "Test String";
   const char* cptrString = "Test String";
   ::std::string_view viewString = "Test String";

   template<Literal SENT_AS_TEMPLATE_ARGUMENT>
   consteval auto FixedStringAsTemplateArgument() {
      return SENT_AS_TEMPLATE_ARGUMENT;
   }
}


///                                                                           
/// CT::FixedString                                                           
///                                                                           
SCENARIO("Testing CT::FixedString", "[ct]") {
   //static_assert(CT::FixedString<>); // shouldn't compile
   static_assert(    CT::FixedString<decltype(fixedString)>);
   static_assert(not CT::FixedString<decltype(justString)>);
   static_assert(not CT::FixedString<decltype(carrayString)>);
   static_assert(not CT::FixedString<decltype(viewString)>);

   static_assert(    CT::FixedString<decltype(fixedString), decltype(fixedString), decltype(fixedString)>);
   static_assert(not CT::FixedString<decltype(fixedString), decltype(fixedString), decltype(justString)>);
}


///                                                                           
/// CT::FixedChar                                                             
///                                                                           
SCENARIO("Testing CT::FixedChar", "[ct]") {
   //static_assert(CT::FixedChar<>); // shouldn't compile
   static_assert(    CT::FixedChar<char, wchar_t, char8_t, char16_t, char32_t>);
   static_assert(not CT::FixedChar<char, wchar_t, char8_t, char16_t, int>);
}


///                                                                           
/// Literal                                                                   
///                                                                           
TEMPLATE_TEST_CASE("Testing Literal", "[ct]",
   char, wchar_t, char8_t, char16_t, char32_t
) {
   STATIC_REQUIRE(FixedStringAsTemplateArgument<"Template String">());

   WHEN("Constructed") {
      Literal defaultConstructed;
      REQUIRE(not defaultConstructed);
      REQUIRE(defaultConstructed.size() == 0);
      REQUIRE(defaultConstructed.empty() == true);

      constexpr Literal defaultConstructedCxpr;
      STATIC_REQUIRE(not defaultConstructedCxpr);
      STATIC_REQUIRE(defaultConstructedCxpr.size() == 0);
      STATIC_REQUIRE(defaultConstructedCxpr.empty() == true);

      Literal arrayConstructed = "array constructed";
      REQUIRE(arrayConstructed);
      REQUIRE(arrayConstructed.size() == 17);
      REQUIRE(arrayConstructed.empty() == false);

      constexpr Literal arrayConstructedCxpr = "array constructed";
      STATIC_REQUIRE(arrayConstructedCxpr);
      STATIC_REQUIRE(arrayConstructedCxpr.size() == 17);
      STATIC_REQUIRE(arrayConstructedCxpr.empty() == false);

      Literal emptyArrayConstructed = "";
      REQUIRE(not emptyArrayConstructed);
      REQUIRE(emptyArrayConstructed.size() == 0);
      REQUIRE(emptyArrayConstructed.empty() == true);

      constexpr Literal emptyArrayConstructedCxpr = "";
      STATIC_REQUIRE(not emptyArrayConstructedCxpr);
      STATIC_REQUIRE(emptyArrayConstructedCxpr.size() == 0);
      STATIC_REQUIRE(emptyArrayConstructedCxpr.empty() == true);
   }

   WHEN("Assigned") {

   }

   WHEN("Iterated") {

   }

   WHEN("Accessed") {

   }

   WHEN("Resized") {

   }

   WHEN("Substring") {

   }

   WHEN("Searched") {

   }

   WHEN("Compared") {

   }

   WHEN("Swapped") {

   }

   WHEN("Concatenated") {

   }

   WHEN("Hashed") {

   }
}
