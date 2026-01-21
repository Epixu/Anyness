///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "Main.hpp"
#include <Langulus/Literal.hpp>
#include <string>
#include <string_view>

using namespace Langulus;

namespace
{
   constexpr Literal emptyUndefined {};
   constexpr Literal emptyString2 = "";
   constexpr Literal emptyString3 = "\0";
   constexpr Literal emptyString4 = "\0\0\0";
   constexpr Literal fixedString = "Test String";
   constexpr const char carrayString[] = "Test String";
   constexpr const char* cptrString = "Test String";
   constexpr ::std::string_view viewString = "Test String";
   ::std::string justString = "Test String";

   constexpr Literal fixedValue = 5.5f;
   constexpr Literal fixedValueChar = 'a';

   template<Literal SENT_AS_TEMPLATE_ARGUMENT>
   consteval auto LiteralAsTemplateArgument() {
      return SENT_AS_TEMPLATE_ARGUMENT;
   }
}


///                                                                           
/// CT::Literal                                                               
///                                                                           
SCENARIO("Testing CT::Literal") {
   //static_assert(CT::Literal<>); // shouldn't compile
   static_assert(    CT::Literal<decltype(emptyUndefined)>);
   static_assert(    CT::Literal<decltype(fixedString)>);
   static_assert(    CT::Literal<decltype(fixedValue)>);
   static_assert(    CT::Literal<decltype(fixedValueChar)>);
   static_assert(not CT::Literal<decltype(justString)>);
   static_assert(not CT::Literal<decltype(carrayString)>);
   static_assert(not CT::Literal<decltype(viewString)>);

   static_assert(    CT::Literal<decltype(fixedString), decltype(emptyUndefined), decltype(fixedValue)>);
   static_assert(not CT::Literal<decltype(fixedString), decltype(emptyUndefined), decltype(justString)>);
}


///                                                                           
/// CT::LiteralUndefined                                                      
///                                                                           
SCENARIO("Testing CT::LiteralUndefined") {
   //static_assert(CT::LiteralUndefined<>); // shouldn't compile
   static_assert(    CT::LiteralUndefined<decltype(emptyUndefined)>);
   static_assert(not CT::LiteralUndefined<decltype(emptyString2)>);
   static_assert(not CT::LiteralUndefined<decltype(emptyString3)>);
   static_assert(not CT::LiteralUndefined<decltype(emptyString4)>);
   static_assert(not CT::LiteralUndefined<decltype(fixedString)>);
   static_assert(not CT::LiteralUndefined<decltype(justString)>);
   static_assert(not CT::LiteralUndefined<decltype(carrayString)>);
   static_assert(not CT::LiteralUndefined<decltype(viewString)>);
   static_assert(not CT::LiteralUndefined<decltype(fixedValue)>);
   static_assert(not CT::LiteralUndefined<decltype(fixedValueChar)>);

   static_assert(    CT::LiteralUndefined<decltype(emptyUndefined), decltype(emptyUndefined)>);
   static_assert(not CT::LiteralUndefined<decltype(emptyUndefined), decltype(emptyString3)>);
}


///                                                                           
/// CT::LiteralString                                                         
///                                                                           
SCENARIO("Testing CT::LiteralString") {
   //static_assert(CT::LiteralString<>); // shouldn't compile
   static_assert(not CT::LiteralString<decltype(emptyUndefined)>);
   static_assert(    CT::LiteralString<decltype(emptyString2)>);
   static_assert(    CT::LiteralString<decltype(emptyString3)>);
   static_assert(    CT::LiteralString<decltype(emptyString4)>);
   static_assert(    CT::LiteralString<decltype(fixedString)>);
   static_assert(not CT::LiteralString<decltype(justString)>);
   static_assert(not CT::LiteralString<decltype(carrayString)>);
   static_assert(not CT::LiteralString<decltype(viewString)>);
   static_assert(not CT::LiteralString<decltype(fixedValue)>);
   static_assert(not CT::LiteralString<decltype(fixedValueChar)>);

   static_assert(    CT::LiteralString<decltype(fixedString), decltype(emptyString3), decltype(emptyString4)>);
   static_assert(not CT::LiteralString<decltype(fixedString), decltype(emptyString3), decltype(justString)>);
}


///                                                                           
/// CT::LiteralValue                                                          
///                                                                           
SCENARIO("Testing CT::LiteralValue") {
   //static_assert(CT::LiteralValue<>); // shouldn't compile
   static_assert(not CT::LiteralValue<decltype(emptyUndefined)>);
   static_assert(not CT::LiteralValue<decltype(emptyString2)>);
   static_assert(not CT::LiteralValue<decltype(emptyString3)>);
   static_assert(not CT::LiteralValue<decltype(emptyString4)>);
   static_assert(not CT::LiteralValue<decltype(fixedString)>);
   static_assert(not CT::LiteralValue<decltype(justString)>);
   static_assert(not CT::LiteralValue<decltype(carrayString)>);
   static_assert(not CT::LiteralValue<decltype(viewString)>);
   static_assert(    CT::LiteralValue<decltype(fixedValue)>);
   static_assert(    CT::LiteralValue<decltype(fixedValueChar)>);

   static_assert(    CT::LiteralValue<decltype(fixedValue), decltype(fixedValueChar)>);
   static_assert(not CT::LiteralValue<decltype(fixedValue), decltype(fixedString)>);
}


///                                                                           
/// CT::LiteralChar                                                           
///                                                                           
SCENARIO("Testing CT::LiteralChar") {
   //static_assert(CT::LiteralChar<>); // shouldn't compile
   static_assert(    CT::LiteralChar<char, wchar_t, char8_t, char16_t, char32_t>);
   static_assert(not CT::LiteralChar<char, wchar_t, char8_t, char16_t, int>);
}


///                                                                           
/// Literal strings                                                           
///                                                                           
TEST_CASE_TEMPLATE("Testing literal strings", TestType,
   char, wchar_t, char8_t, char16_t, char32_t
) {
   static_assert(    LiteralAsTemplateArgument<"string">());
   static_assert(    LiteralAsTemplateArgument<5.5f>());
   static_assert(not LiteralAsTemplateArgument<"">());
   static_assert(not LiteralAsTemplateArgument<0>());

   WHEN("Constructed") {
      Literal defaultConstructed;
      REQUIRE(not defaultConstructed);
      REQUIRE(defaultConstructed.size() == 0);
      REQUIRE(defaultConstructed.empty() == true);
      REQUIRE(defaultConstructed == emptyUndefined);
      REQUIRE(defaultConstructed == emptyString2);
      REQUIRE(defaultConstructed == emptyString3);
      REQUIRE(defaultConstructed == emptyString4);

      constexpr Literal defaultConstructedCxpr;
      static_assert(not defaultConstructedCxpr);
      static_assert(defaultConstructedCxpr.size() == 0);
      static_assert(defaultConstructedCxpr.empty() == true);
      static_assert(defaultConstructedCxpr == emptyUndefined);
      static_assert(defaultConstructedCxpr == emptyString2);
      static_assert(defaultConstructedCxpr == emptyString3);
      static_assert(defaultConstructedCxpr == emptyString4);

      Literal arrayConstructed = "array constructed";
      REQUIRE(arrayConstructed);
      REQUIRE(arrayConstructed.size() == 17);
      REQUIRE(arrayConstructed.empty() == false);
      REQUIRE(arrayConstructed != emptyUndefined);
      REQUIRE(arrayConstructed != emptyString2);
      REQUIRE(arrayConstructed != emptyString3);
      REQUIRE(arrayConstructed != emptyString4);

      constexpr Literal arrayConstructedCxpr = "array constructed";
      static_assert(arrayConstructedCxpr);
      static_assert(arrayConstructedCxpr.size() == 17);
      static_assert(arrayConstructedCxpr.empty() == false);
      static_assert(arrayConstructedCxpr != emptyUndefined);
      static_assert(arrayConstructedCxpr != emptyString2);
      static_assert(arrayConstructedCxpr != emptyString3);
      static_assert(arrayConstructedCxpr != emptyString4);

      Literal emptyArrayConstructed = "";
      REQUIRE(not emptyArrayConstructed);
      REQUIRE(emptyArrayConstructed.size() == 0);
      REQUIRE(emptyArrayConstructed.empty() == true);
      REQUIRE(emptyArrayConstructed == emptyUndefined);
      REQUIRE(emptyArrayConstructed == emptyString2);
      REQUIRE(emptyArrayConstructed == emptyString3);
      REQUIRE(emptyArrayConstructed == emptyString4);

      constexpr Literal emptyArrayConstructedCxpr = "";
      static_assert(not emptyArrayConstructedCxpr);
      static_assert(emptyArrayConstructedCxpr.size() == 0);
      static_assert(emptyArrayConstructedCxpr.empty() == true);
      static_assert(emptyArrayConstructedCxpr == emptyUndefined);
      static_assert(emptyArrayConstructedCxpr == emptyString2);
      static_assert(emptyArrayConstructedCxpr == emptyString3);
      static_assert(emptyArrayConstructedCxpr == emptyString4);
   }

   WHEN("Assigned") {
      Literal local = fixedString;
      local = carrayString;
      REQUIRE(local == "Test String");
   }

   WHEN("Iterated") {
      for (size_t i = 0; i < fixedString.size(); ++i) {
         REQUIRE(fixedString[i] == carrayString[i]);
      }

      std::string accumulate;
      for (auto& c : fixedString)
         accumulate += c;
      REQUIRE(accumulate == "Test String");
   }

   WHEN("Accessed") {
      IF_SAFE(volatile size_t idx = fixedString.size() + 1);
      IF_SAFE(REQUIRE_THROWS(fixedString[idx]));
      static_assert(fixedString[0] == carrayString[0]);
      //STATIC_REQUIRE(fixedString[fixedString.size() + 1]); // shouldn't compile
   }

   WHEN("Resized") {

   }

   WHEN("Substring") {

   }

   WHEN("Searched") {

   }

   WHEN("Compared") {
      Literal local = fixedString;
      REQUIRE(local == cptrString);
   }

   WHEN("Swapped") {

   }

   WHEN("Concatenated") {

   }

   WHEN("Hashed") {

   }
}
