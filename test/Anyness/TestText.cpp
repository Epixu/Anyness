///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#include "../Main.hpp"
#include <Langulus/Anyness/Text.hpp>

using namespace Langulus;
using Anyness::Text;
using Anyness::Allocator;

namespace
{
   /// A type that is reflected, as convertible to Text                       
   ///    @attention this causes MSVC to ICE since 19.40.33811.0 :(           
   ///      good thing we don't support MSVC any longer :)                    
   struct Stringifiable {
      using CTTI_MapsTo = Text;
      // ReSharper disable once CppMemberFunctionMayBeConst
      explicit operator Text() { return "Stringifiable converted to Text"; }
   };

   /// A type that is reflected as convertible to Text                        
   struct StringifiableConst {
      using CTTI_MapsTo = Text;
      explicit operator Text() const { return "StringifiableConst converted to Text"; }
   };
}

/// Possible states:                                                          
void Text_CheckState_Default(const Text&);
void Text_CheckState_Invariant(const Text&);
void Text_CheckState_OwnedFull(const Text&);
void Text_CheckState_OwnedFullConst(const Text&);
void Text_CheckState_OwnedEmpty(const Text&);
void Text_CheckState_DisownedFull(const Text&);
void Text_CheckState_DisownedFullConst(const Text&);
void Text_CheckState_Abandoned(const Text&);

TEMPLATE_TEST_CASE("Testing text containers", "[text]",
   Text
   //TODO Path
) {
   using T = TestType;
   static Allocator::State memoryState;
   static_assert(    CT::Typed<T>, "Container not typed");
   static_assert(not CT::Array<T>, "Wrongly typed container");
   static_assert(    CT::Exact<TypeOf<T>, char>, "Wrongly typed container");

   GIVEN("Default text container") {
      T text;

      Text_CheckState_Default(text);
      REQUIRE_FALSE(text.IsConstant());

      WHEN("Capacity is reserved") {
         text.Reserve(500);

         Text_CheckState_OwnedEmpty(text);
         REQUIRE_FALSE(text.IsConstant());
         REQUIRE(text.GetReserved() >= 500);
      }

      WHEN("Directly assigned to itself") {
         LglsDisableWarningPush
         LglsDisableWarning_SelfAssign
         // ReSharper disable once CppIdenticalOperandsInBinaryExpression
         text = text;
         LglsDisableWarningPop
         
         Text_CheckState_Default(text);
         REQUIRE_FALSE(text.IsConstant());
      }

      WHEN("Indirectly assigned to itself") {
         const auto anothertext = text;
         text = anothertext;

         Text_CheckState_Default(text);
         REQUIRE_FALSE(text.IsConstant());
      }
   }

   GIVEN("Uninitialized text container") {
      T* text = nullptr;

      WHEN("Constructed with a null-terminated c-string") {
         text = new T {"test1"};

         Text_CheckState_OwnedFull(*text);
         REQUIRE((*text).GetCount() == 5);
         REQUIRE((*text).GetReserved() >= 5);
         REQUIRE((*text) == "test1");
         REQUIRE((*text)[0] == 't');
         REQUIRE((*text)[1] == 'e');
         REQUIRE((*text)[2] == 's');
         REQUIRE((*text)[3] == 't');
         REQUIRE((*text)[4] == '1');
         REQUIRE_THROWS((*text)[5] == '?');
      }

      WHEN("Constructed with a count-terminated string") {
         text = new T {Text::FromText("test2", 5)};

         Text_CheckState_DisownedFullConst(*text);
         REQUIRE((*text).GetCount() == 5);
         REQUIRE((*text).GetReserved() >= 5);
         REQUIRE((*text) == "test2");
         REQUIRE((*text)[0] == 't');
         REQUIRE((*text)[1] == 'e');
         REQUIRE((*text)[2] == 's');
         REQUIRE((*text)[3] == 't');
         REQUIRE((*text)[4] == '2');
         REQUIRE_THROWS((*text)[5] == '?');
      }

      WHEN("Constructed with a bounded array string") {
         char test1[] = "test3";
         text = new T {test1};

         Text_CheckState_OwnedFull(*text);
         REQUIRE((*text).GetCount() == 5);
         REQUIRE((*text).GetReserved() >= 5);
         REQUIRE((*text) == "test3");
         REQUIRE((*text)[0] == 't');
         REQUIRE((*text)[1] == 'e');
         REQUIRE((*text)[2] == 's');
         REQUIRE((*text)[3] == 't');
         REQUIRE((*text)[4] == '3');
         REQUIRE_THROWS((*text)[5] == '?');
      }

      WHEN("Constructed with a nullptr_t") {
         text = new T {nullptr};

         Text_CheckState_Default(*text);
      }

      WHEN("Constructed with a nullptr c-array") {
         text = new T {(char*)nullptr};

         Text_CheckState_Default(*text);
      }

      WHEN("Constructed with empty c-array") {
         text = new T {""};

         Text_CheckState_Default(*text);
      }

      WHEN("Constructed with a single character") {
         text = new T {'?'};

         Text_CheckState_OwnedFull(*text);
         REQUIRE((*text).GetCount() == 1);
         REQUIRE((*text).GetReserved() >= 1);
         REQUIRE((*text)[0] == '?');
         REQUIRE_THROWS((*text)[1] == '?');
      }

      if (text)
         delete text;
   }

   GIVEN("Reserved text container") {
      T text;
      text.Reserve(500);
      auto memory = text.GetRaw();

      WHEN("Text is extended") {
         auto region = text.Extend(10);

         REQUIRE(text.GetCount() == 10);
         REQUIRE(text.GetReserved() >= 500);
         REQUIRE(text.GetRaw() == memory);
         REQUIRE(text.GetAllocation());
         REQUIRE(region.GetCount() == 10);
         REQUIRE(region.GetRaw() == memory);
      }

      WHEN("Text is concatenated") {
         text += "test";

         REQUIRE(text.GetCount() == 4);
         REQUIRE(text.GetReserved() >= 500);
         REQUIRE(text.GetRaw() == memory);
         REQUIRE(text.GetAllocation());
         REQUIRE(text == "test");
      }

      WHEN("Text is cleared") {
         text += "test";
         text.Clear();

         REQUIRE(text.GetCount() == 0);
         REQUIRE(text.GetReserved() >= 500);
         REQUIRE(text.GetRaw() == memory);
         REQUIRE(text.GetAllocation());
         REQUIRE(text != "test");
      }

      WHEN("Text is reset") {
         text += "test";
         text.Reset();

         REQUIRE(text.GetCount() == 0);
         REQUIRE(text.GetReserved() == 0);
         REQUIRE(text.GetRaw() == nullptr);
         REQUIRE(text.GetType() == MetaOf<char>());
         REQUIRE_FALSE(text.GetAllocation());
         REQUIRE(text != "test");
      }
   }

   GIVEN("Full text container") {
      T text {"test1"};
      auto memory = text.GetRaw();

      WHEN("Add more text") {
         text += "test2";

         REQUIRE(text == "test1test2");
         REQUIRE(text.GetCount() == 10);
         REQUIRE(text.GetReserved() >= 10);
         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            REQUIRE(text.GetRaw() == memory);
         #endif
         REQUIRE(text.GetAllocation());
         REQUIRE(text.template Is<char>());
      }

      WHEN("More capacity is reserved") {
         text.Reserve(20);

         REQUIRE(text.GetCount() == 5);
         REQUIRE(text.GetReserved() >= 20);
         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            REQUIRE(text.GetRaw() == memory);
         #endif
         REQUIRE(text.GetAllocation());
      }

      WHEN("More capacity is reserved, via Extend()") {
         auto region = text.Extend(10);

         REQUIRE(text.GetCount() == 15);
         REQUIRE(text.GetReserved() >= 15);
         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            REQUIRE(text.GetRaw() == memory);
         #endif
         REQUIRE(text.GetAllocation());
         REQUIRE(region.GetCount() == 10);
         REQUIRE(region.GetRaw() == text.GetRaw() + 5);
      }

      WHEN("Less capacity is reserved") {
         text.Reserve(2);

         REQUIRE(text.GetCount() == 2);
         REQUIRE(text.GetReserved() >= 5);
         REQUIRE(text.GetRaw() == memory);
         REQUIRE(text.GetAllocation());
      }

      WHEN("Text is cleared") {
         text.Clear();

         REQUIRE(text.GetCount() == 0);
         REQUIRE(text.GetReserved() >= 5);
         REQUIRE(text.GetRaw() == memory);
         REQUIRE(text.GetAllocation());
         REQUIRE(text.template Is<char>());
      }

      WHEN("Text is reset") {
         text.Reset();

         REQUIRE(text.GetCount() == 0);
         REQUIRE(text.GetReserved() == 0);
         REQUIRE_FALSE(text.GetRaw());
         REQUIRE(text.template Is<char>());
      }

      WHEN("Text is copied shallowly") {
         T copy = text;

         REQUIRE(text.GetCount() == copy.GetCount());
         REQUIRE(text.GetReserved() == copy.GetReserved());
         REQUIRE(text.GetRaw() == copy.GetRaw());
         REQUIRE(text.GetType() == copy.GetType());
         REQUIRE(text.GetAllocation());
         REQUIRE(copy.GetAllocation());
         REQUIRE(copy.GetUses() == 2);
         REQUIRE(text.GetUses() == 2);
      }

      WHEN("Text is cloned (deep copy)") {
         T copy = Clone(text);

         REQUIRE(text.GetCount() == copy.GetCount());
         REQUIRE(text.GetReserved() >= copy.GetReserved());
         REQUIRE(text.GetRaw() != copy.GetRaw());
         REQUIRE(text.GetType() == copy.GetType());
         REQUIRE(text.GetAllocation());
         REQUIRE(copy.GetAllocation());
         REQUIRE(copy.GetUses() == 1);
         REQUIRE(text.GetUses() == 1);
      }

      WHEN("Text is reset, then allocated again") {
         text.Reset();
         text += "kurec";

         REQUIRE(text.GetCount() == 5);
         REQUIRE(text.GetReserved() >= 5);
         REQUIRE(text.GetAllocation());
         REQUIRE(text.template Is<char>());
      }

      WHEN("Texts are compared") {
         REQUIRE(text == "test1");
         REQUIRE(text != "Tests");
      }
   }

   REQUIRE(memoryState.Assert());
   REQUIRE_FALSE(Allocator::CollectGarbage());
}

TEMPLATE_TEST_CASE("Unsigned number stringification", "[text]",
   uint8_t, uint16_t, uint32_t, uint64_t
) {
   static Allocator::State memoryState;

   WHEN("Constructed Text with a number") {
      Text* text = new Text {Text::FromNumber(TestType{66})};

      REQUIRE((*text).GetCount() == 2);
      REQUIRE((*text).GetReserved() >= 2);
      REQUIRE((*text).Is<char>());
      REQUIRE((*text).GetRaw());
      REQUIRE((*text).GetAllocation());
      REQUIRE((*text) == "66");

      delete text;
   }

   /*WHEN("Constructed Path with a number") {
      Path* text = new Path {TestType{66}};

      REQUIRE((*text).GetCount() == 2);
      REQUIRE((*text).GetReserved() >= 2);
      REQUIRE((*text).Is<char>());
      REQUIRE((*text).GetRaw());
      REQUIRE((*text).GetAllocation());
      REQUIRE((*text) == "66");

      delete text;
   }*/

   REQUIRE(memoryState.Assert());
   REQUIRE_FALSE(Allocator::CollectGarbage());
}

TEMPLATE_TEST_CASE("Signed number stringification", "[text]",
   int8_t, int16_t, int32_t, int64_t
) {
   static Allocator::State memoryState;

   WHEN("Constructed Text with a number") {
      Text* text = new Text {Text::FromNumber(TestType{-66})};

      REQUIRE((*text).GetCount() == 3);
      REQUIRE((*text).GetReserved() >= 3);
      REQUIRE((*text).Is<char>());
      REQUIRE((*text).GetRaw());
      REQUIRE((*text).GetAllocation());
      REQUIRE((*text) == "-66");

      delete text;
   }

   /*WHEN("Constructed Path with a number") {
      Path* text = new Path {TestType{-66}};

      REQUIRE((*text).GetCount() == 3);
      REQUIRE((*text).GetReserved() >= 3);
      REQUIRE((*text).Is<char>());
      REQUIRE((*text).GetRaw());
      REQUIRE((*text).GetAllocation());
      REQUIRE((*text) == "-66");

      delete text;
   }*/

   REQUIRE(memoryState.Assert());
   REQUIRE_FALSE(Allocator::CollectGarbage());
}

TEMPLATE_TEST_CASE("Real number stringification", "[text]",
   float, double
) {
   static Allocator::State memoryState;

   WHEN("Constructed Text with a number") {
      Text* text = new Text {Text::FromNumber(TestType{-66.666}, 2)};

      REQUIRE((*text).GetCount() == 3);
      REQUIRE((*text).GetReserved() >= 3);
      REQUIRE((*text).Is<char>());
      REQUIRE((*text).GetRaw());
      REQUIRE((*text).GetAllocation());
      REQUIRE((*text) == "~-66.67");

      delete text;
   }

   /*WHEN("Constructed Path with a number") {
      Path* text = new Path {TestType{-66}};

      REQUIRE((*text).GetCount() == 3);
      REQUIRE((*text).GetReserved() >= 3);
      REQUIRE((*text).Is<char>());
      REQUIRE((*text).GetRaw());
      REQUIRE((*text).GetAllocation());
      REQUIRE((*text) == "-66");

      delete text;
   }*/

   REQUIRE(memoryState.Assert());
   REQUIRE_FALSE(Allocator::CollectGarbage());
}

TEMPLATE_TEST_CASE("Logging text containers", "[text]", Text/*TODO , Path*/) {
   static Allocator::State memoryState;

   WHEN("Logging") {
      TestType text {"some text"};
      Logger::Info("You should also see ", text);
   }

   WHEN("Logging literal") {
      Logger::Info("You should also see ", "some text"_text);
   }

   REQUIRE(memoryState.Assert());
   REQUIRE_FALSE(Allocator::CollectGarbage());
}

TEMPLATE_TEST_CASE("Reflected coverters to text", "[text]", /*Stringifiable,*/ StringifiableConst) {
   static Allocator::State memoryState;

   GIVEN("A stringifiable type") {
      const auto debugMeta = MetaOf<Text>();
      const auto meta = MetaOf<TestType>();
      TestType instance;

      WHEN("Converted") {
         // Calling static_cast<Debug> here doesn't work, because of MSVC bug
         const auto staticallyConverted = instance.operator Text();
         
         Text rttiConverted;
         meta.GetMorphism(debugMeta)(&instance, &rttiConverted);

         REQUIRE(staticallyConverted == rttiConverted);
         REQUIRE(staticallyConverted == "Stringifiable converted to Text");
      }
   }

   REQUIRE(memoryState.Assert());
   REQUIRE_FALSE(Allocator::CollectGarbage());
}

/*TEMPLATE_TEST_CASE("Text container interoperability", "[text]",
   (TypePair<Path, Text>),
   (TypePair<Text, Path>)
) {
   static Allocator::State memoryState;

   using LHS = typename TestType::LHS;
   using RHS = typename TestType::RHS;

   GIVEN("Two types of text containers") {
      WHEN("Constructed") {
         LHS text {RHS{"one"}};

         REQUIRE(text == "one");
      }

      WHEN("Assigned") {
         LHS text {"one"};
         text = RHS {"two"};

         REQUIRE(text == "two");
      }

      WHEN("Concatenated (destructively)") {
         LHS text {"one"};
         text += RHS {"two"};

         REQUIRE(text == "onetwo");
      }

      WHEN("Concatenated") {
         LHS text {"one"};
         LHS text2 = text + RHS {"two"};

         REQUIRE(text == "one");
         REQUIRE(text2 == "onetwo");
      }
   }

   REQUIRE(memoryState.Assert());

   // Destroy BANK before static data - otherwise problems happen if    
   // not using managed reflection                                      
   BANK.Reset();

   REQUIRE_FALSE(Allocator::CollectGarbage());
}*/

/*TEMPLATE_TEST_CASE("Text container conversion at runtime", "[text]",
   (TypePair<Text, Path>)
) {
   static Allocator::State memoryState;

   using LHS = typename TestType::LHS;
   using RHS = typename TestType::RHS;

   GIVEN("Two types of text containers") {
      WHEN("Reflected") {
         auto lhs = MetaDataOf<LHS>();
         auto rhs = MetaDataOf<RHS>();

         REQUIRE(lhs->GetConverter(rhs) != nullptr);
      }
   }

   REQUIRE(memoryState.Assert());

   // Destroy BANK before static data - otherwise problems happen if    
   // not using managed reflection                                      
   BANK.Reset();

   REQUIRE_FALSE(Allocator::CollectGarbage());
}*/

/*TEMPLATE_TEST_CASE("Containing literals", "[text]",
   Many, Tag
) {
   static Allocator::State memoryState;

   GIVEN("Two types of text containers") {
      WHEN("Constructed") {
         TestType text {"one"};

         REQUIRE(text.GetCount() == 1);
         REQUIRE(text.template IsExact<Text>());
         REQUIRE(text.template As<Text>() == "one");
      }

      WHEN("Assigned") {
         TestType text {"one"};
         text = "two";

         REQUIRE(text.GetCount() == 1);
         REQUIRE(text.template IsExact<Text>());
         REQUIRE(text.template As<Text>() == "two");
      }

      WHEN("Concatenated (destructively)") {
         TestType text {"one"};
         text += TestType {"two"};

         REQUIRE(text.GetCount() == 2);
         REQUIRE(text.template IsExact<Text>());
         REQUIRE(text.template AsAt<Text>(0) == "one");
         REQUIRE(text.template AsAt<Text>(1) == "two");
      }

      WHEN("Concatenated") {
         TestType text {"one"};
         TestType text2 = text + TestType {"two"};

         REQUIRE(text.GetCount() == 1);
         REQUIRE(text2.GetCount() == 2);
         REQUIRE(text.template IsExact<Text>());
         REQUIRE(text2.template IsExact<Text>());
         REQUIRE(text.template As<Text>() == "one");
         REQUIRE(text2.template AsAt<Text>(0) == "one");
         REQUIRE(text2.template AsAt<Text>(1) == "two");
      }
   }

   REQUIRE(memoryState.Assert());
   REQUIRE_FALSE(Allocator::CollectGarbage());
}*/

void Text_CheckState_Default(const Text& text) {
   REQUIRE_FALSE(text.IsConstant());
   REQUIRE_FALSE(text.IsDeep());
   REQUIRE_FALSE(text.IsSparse());
   REQUIRE      (text.IsTyped());
   REQUIRE_FALSE(text.IsUntyped());
   REQUIRE_FALSE(text.IsValid());
   REQUIRE      (text.IsEmpty());
   REQUIRE_FALSE(text.GetAllocation());
   REQUIRE      (text.IsTypeConstrained());
   REQUIRE      (text.GetType() == MetaOf<char>());
   REQUIRE      (text.Is<char>());
   REQUIRE      (text.IsDense());
   REQUIRE      (text.GetCount() == 0);
   REQUIRE      (text.GetReserved() == 0);
   REQUIRE      (text.GetUses() == 0);
   REQUIRE      (text.GetRaw() == nullptr);
   REQUIRE      (text == nullptr);
   REQUIRE_FALSE(text != nullptr);
   REQUIRE      (text == (char*)nullptr);
   REQUIRE_FALSE(text != (char*)nullptr);
   REQUIRE      (not text);
   REQUIRE_FALSE(text);
   REQUIRE      (text == "");
   REQUIRE_FALSE(text != "");
   REQUIRE_FALSE(text == "no match");
}

void Text_CheckState_OwnedEmpty(const Text& text) {
   REQUIRE_FALSE(text.IsConstant());
   REQUIRE_FALSE(text.IsDeep());
   REQUIRE_FALSE(text.IsSparse());
   REQUIRE      (text.IsTyped());
   REQUIRE_FALSE(text.IsUntyped());
   REQUIRE_FALSE(text.IsValid());
   REQUIRE      (text.IsEmpty());
   REQUIRE      (text.GetAllocation());
   REQUIRE      (text.IsTypeConstrained());
   REQUIRE      (text.GetType() == MetaOf<char>());
   REQUIRE      (text.Is<char>());
   REQUIRE      (text.IsDense());
   REQUIRE      (text.GetCount() == 0);
   REQUIRE      (text.GetReserved() > 0);
   REQUIRE      (text.GetUses() == 1);
   REQUIRE      (text.GetRaw());
   REQUIRE      (text == nullptr);
   REQUIRE_FALSE(text != nullptr);
   REQUIRE      (text == (char*)nullptr);
   REQUIRE_FALSE(text != (char*)nullptr);
   REQUIRE      (not text);
   REQUIRE_FALSE(text);
   REQUIRE      (text == "");
   REQUIRE_FALSE(text != "");
   REQUIRE_FALSE(text == "no match");
}

void Text_CheckState_OwnedFull(const Text& text) {
   REQUIRE_FALSE(text.IsConstant());
   REQUIRE_FALSE(text.IsDeep());
   REQUIRE_FALSE(text.IsSparse());
   REQUIRE      (text.IsTyped());
   REQUIRE_FALSE(text.IsUntyped());
   REQUIRE      (text.IsValid());
   REQUIRE_FALSE(text.IsEmpty());
   REQUIRE      (text.GetAllocation());
   REQUIRE      (text.IsTypeConstrained());
   REQUIRE      (text.GetType() == MetaOf<char>());
   REQUIRE      (text.Is<char>());
   REQUIRE      (text.IsDense());
   REQUIRE      (text.GetCount() > 0);
   REQUIRE      (text.GetReserved() > 0);
   REQUIRE      (text.GetUses() > 0);
   REQUIRE      (text.GetRaw());
   REQUIRE      (text != nullptr);
   REQUIRE_FALSE(text == nullptr);
   REQUIRE      (text != (char*)nullptr);
   REQUIRE_FALSE(text == (char*)nullptr);
   REQUIRE      (text);
   REQUIRE_FALSE(not text);
   REQUIRE      (text != "");
   REQUIRE_FALSE(text == "");
   REQUIRE_FALSE(text == "no match");
}

void Text_CheckState_DisownedFullConst(const Text& text) {
   REQUIRE      (text.IsConstant());
   REQUIRE_FALSE(text.IsDeep());
   REQUIRE_FALSE(text.IsSparse());
   REQUIRE      (text.IsTyped());
   REQUIRE_FALSE(text.IsUntyped());
   REQUIRE      (text.IsValid());
   REQUIRE_FALSE(text.IsEmpty());
   REQUIRE_FALSE(text.GetAllocation());
   REQUIRE      (text.IsTypeConstrained());
   REQUIRE      (text.GetType() == MetaOf<char>());
   REQUIRE      (text.Is<char>());
   REQUIRE      (text.IsDense());
   REQUIRE      (text.GetCount() > 0);
   REQUIRE      (text.GetReserved() > 0);
   REQUIRE      (text.GetUses() == 0);
   REQUIRE      (text.GetRaw());
   REQUIRE      (text != nullptr);
   REQUIRE_FALSE(text == nullptr);
   REQUIRE      (text != (char*)nullptr);
   REQUIRE_FALSE(text == (char*)nullptr);
   REQUIRE      (text);
   REQUIRE_FALSE(not text);
   REQUIRE      (text != "");
   REQUIRE_FALSE(text == "");
   REQUIRE_FALSE(text == "no match");
}
