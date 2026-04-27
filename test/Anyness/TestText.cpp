///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#include "../Main.hpp"
#include "any/TestAnyCommon.hpp"
#include <Langulus/Anyness/Text.hpp>
#include <Langulus/Anyness/SerializeText.hpp>

using namespace Langulus;
using Anyness::Text;
using Anyness::operator""_text;

namespace doctest
{
   template<>
   struct StringMaker<Text> {
      static String convert(Text const& value) {
         return "\"" + toString(static_cast<::std::string>(value)) + "\"_text";
      }
   };
}

namespace
{
   /// A type that is reflected, as convertible to Text                       
   ///    @attention this causes MSVC to ICE since 19.40.33811.0 :(           
   ///      good thing we don't support MSVC any longer :)                    
   struct Stringifiable {
      using CTTI_MapsTo = Text;
      explicit operator Text() {
         return "Stringifiable converted to Text";
      }
   };

   /// A type that is reflected as convertible to Text                        
   struct StringifiableConst {
      using CTTI_MapsTo = Text;
      explicit operator Text() const { 
         return "StringifiableConst converted to Text";
      }
   };
}

/// Possible states:                                                          
void Text_CheckState_Default(const CT::Container auto& text) {
   REQUIRE      (text.IsConstant());
   REQUIRE_FALSE(text.IsDeep());
   REQUIRE_FALSE(text.IsSparse());
   REQUIRE      (text.IsTyped());
   REQUIRE_FALSE(text.IsValid());
   REQUIRE      (text.IsEmpty());
   REQUIRE_FALSE(text.GetAllocation());
   REQUIRE      (text.IsTypeConstrained());
   REQUIRE      (text.GetType() == MetaOf<char>());
   REQUIRE      (text.template IsExact<char>());
   REQUIRE      (text.GetCount() == 0);
   REQUIRE      (text.GetReserved() == 0);
   REQUIRE      (text.GetUses() == 0);
   //REQUIRE      (text.GetRaw() == nullptr); // not really a requirement
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

void Text_CheckState_OwnedEmpty(const CT::Container auto& text) {
   REQUIRE_FALSE(text.IsConstant());
   REQUIRE_FALSE(text.IsDeep());
   REQUIRE_FALSE(text.IsSparse());
   REQUIRE      (text.IsTyped());
   REQUIRE_FALSE(text.IsValid());
   REQUIRE      (text.IsEmpty());
   REQUIRE      (text.GetAllocation());
   REQUIRE      (text.IsTypeConstrained());
   REQUIRE      (text.GetType() == MetaOf<char>());
   REQUIRE      (text.template IsExact<char>());
   REQUIRE      (text.GetCount() == 0);
   REQUIRE      (text.GetReserved() > 0);
   REQUIRE      (text.GetUses() == 1);
   //REQUIRE      (text.GetRaw());
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

void Text_CheckState_OwnedFull(const CT::Container auto& text) {
   REQUIRE_FALSE(text.IsConstant());
   REQUIRE_FALSE(text.IsDeep());
   REQUIRE_FALSE(text.IsSparse());
   REQUIRE      (text.IsTyped());
   REQUIRE      (text.IsValid());
   REQUIRE_FALSE(text.IsEmpty());
   REQUIRE      (text.GetAllocation());
   REQUIRE      (text.IsTypeConstrained());
   REQUIRE      (text.GetType() == MetaOf<char>());
   REQUIRE      (text.template IsExact<char>());
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

void Text_CheckState_DisownedFullConst(const CT::Container auto& text) {
   REQUIRE      (text.IsConstant());
   REQUIRE_FALSE(text.IsDeep());
   REQUIRE_FALSE(text.IsSparse());
   REQUIRE      (text.IsTyped());
   REQUIRE      (text.IsValid());
   REQUIRE_FALSE(text.IsEmpty());
   REQUIRE_FALSE(text.GetAllocation());
   REQUIRE      (text.IsTypeConstrained());
   REQUIRE      (text.GetType() == MetaOf<char>());
   REQUIRE      (text.template IsExact<char>());
   REQUIRE      (text.GetCount() > 0);
   REQUIRE      (text.GetReserved() == 0);
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



TEST_CASE_TEMPLATE("Testing text containers", T,
   Text
   //TODO Path
) {
   static MemoryState memoryState;

   using E = TypeOf<T>;
   static_assert(    CT::Typed<T>, "Container not typed");
   static_assert(not CT::Array<T>, "Wrongly typed container");
   static_assert(not CT::Handle<T>);
   static_assert(    Exact<E, char>, "Wrongly typed container");
   static_assert(not CT::Deep<T>);
   static_assert(not CT::ContainsOne<T>);
   static_assert(    CT::ContainsMany<T>);
   static_assert(    CT::HasVariableCount<T>);
   static_assert(    CT::HeapAllocated<T>);
   static_assert(not CT::DeeplyOwned<T>);
   static_assert(    CT::Owned<T>);
   static_assert(    CT::StronglyOwned<T>);
   static_assert(    CT::Comparable<T, T>);
   static_assert(    CT::Comparable<T, E>);
   static_assert(    ::std::ranges::range<T>);
   static_assert(    ::std::ranges::contiguous_range<T>);
   static_assert(    CT::Contiguous<T>);

   static_assert(    requires (T pack)         { pack.Get(); });
   static_assert(    requires (T pack)         { pack.template As<E>(); });
   //static_assert(not requires (T pack)         { pack.GetDeep(); });
   static_assert(not requires (T pack)         { pack.GetResolved(); });
   static_assert(not requires (T pack)         { pack.GetDense(); });
   static_assert(    requires (T pack)         { {pack +   pack} -> ::std::same_as<T >; });
   static_assert(    requires (T pack, E item) { {pack +   item} -> ::std::same_as<T >; });
   static_assert(    requires (T pack)         { {pack +=  pack} -> ::std::same_as<T&>; });
   static_assert(    requires (T pack, E item) { {pack +=  item} -> ::std::same_as<T&>; });
   static_assert(    requires (T pack, E item) { {pack <<  item} -> ::std::same_as<T&>; });
   static_assert(    requires (T pack, E item) { {pack >>  item} -> ::std::same_as<T&>; });
   static_assert(not requires (T pack, E item) { {pack <<= item} -> ::std::same_as<T&>; }); //TODO add pattern mathing?
   static_assert(not requires (T pack, E item) { {pack >>= item} -> ::std::same_as<T&>; }); //TODO add pattern mathing?
   static_assert(    requires (T pack, E item) { pack.InsertAt(Index::Back, item); });
   static_assert(not requires (T pack, E item) { pack.EmplaceAt(Index::Back, item); });
   static_assert(    requires (T pack, E item) { pack.Remove(item); });
   static_assert(    requires (T pack, E item) { pack.RemoveAt(Index::Front); });
   static_assert(    requires (T pack, E item) { pack.Reserve(20); });
   static_assert(not requires (T pack, E item) { pack.EnableOr(); });
   static_assert(not requires (T pack, E item) { pack.IsOr(); });
   static_assert(    requires (T pack, E item) { pack.Find(item); });
   static_assert(    requires (T pack, E item) { pack.ForEach([](const int&) {}); });
   static_assert(    requires (T pack, E item) { pack.ForEachRev([](const int&) {}); });

   Common_GapTest<T, ::std::string>();
   static_assert(sizeof(T) <= sizeof(::std::string));

   GIVEN("Default text container") {
      T text;

      Text_CheckState_Default(text);
      
      WHEN("Cleared") {
         text.Clear();

         Text_CheckState_Default(text);
      }

      WHEN("Reserve") {
         text.Reserve(500);

         Text_CheckState_OwnedEmpty(text);
         REQUIRE(text.GetReserved() >= 500);
      }

      WHEN("Self-assign") {
         LglsDisableWarningPush
         LglsDisableWarning_SelfAssign
         text = text;
         LglsDisableWarningPop
         
         Text_CheckState_Default(text);
      }

      WHEN("Indirect self-assign") {
         const auto anothertext = text;
         text = anothertext;

         Text_CheckState_Default(text);
      }

      WHEN("Compared") {
         static_assert(not static_cast<bool>(T{}));

         static_assert(T{} == T{});
         static_assert(T{} == nullptr);
         static_assert(nullptr == T{});
         static_assert(T{} == "");
         static_assert("" == T{});
         static_assert(T{ nullptr } == T{ nullptr });
         static_assert(T{ "" } == T{ "" });
         static_assert(nullptr == T{ nullptr });
         static_assert(T{ "" } == "");
         static_assert("" == T{ "" });
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
         IF_SAFE(REQUIRE_THROWS((*text)[5] == '?'));
      }

      WHEN("Constructed with a count-terminated string") {
         text = new T {Text::FromText("test2", 2)};

         Text_CheckState_DisownedFullConst(*text);
         REQUIRE((*text).GetCount() == 2);
         REQUIRE((*text) == "te");
         REQUIRE((*text)[0] == 't');
         REQUIRE((*text)[1] == 'e');
         IF_SAFE(REQUIRE_THROWS((*text)[2] == 's'));
         IF_SAFE(REQUIRE_THROWS((*text)[3] == 't'));
         IF_SAFE(REQUIRE_THROWS((*text)[4] == '2'));
         IF_SAFE(REQUIRE_THROWS((*text)[5] == '?'));
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
         IF_SAFE(REQUIRE_THROWS((*text)[5] == '?'));
      }

      WHEN("Constructed with a nullptr_t") {
         text = new T {nullptr};

         Text_CheckState_Default(*text);
      }

      WHEN("Constructed with a nullptr c-string") {
         text = new T {(char*)nullptr};

         Text_CheckState_Default(*text);
      }

      WHEN("Constructed with empty c-string") {
         text = new T {""};

         Text_CheckState_Default(*text);
      }

      WHEN("Constructed with a single character") {
         text = new T {'?'};

         Text_CheckState_OwnedFull(*text);
         REQUIRE((*text).GetCount() == 1);
         REQUIRE((*text).GetReserved() >= 1);
         REQUIRE((*text) == "?");
         REQUIRE((*text)[0] == '?');
         IF_SAFE(REQUIRE_THROWS((*text)[1] == '?'));
      }

      delete text;
   }

   GIVEN("Reserved text container") {
      T text;
      text.Reserve(500);
      auto memory = text.GetRaw();

      /*WHEN("Text is extended") { 
         auto region = text.Extend(10); //TODO this requires to return a container with ownership component that allows for modification without branching

         Text_CheckState_OwnedFull(text);
         Text_CheckState_OwnedFull(region);
         REQUIRE(text.GetCount() == 10);
         REQUIRE(text.GetReserved() >= 500);
         REQUIRE(text.GetRaw() == memory);
         REQUIRE(region.GetCount() == 10);
         REQUIRE(region.GetRaw() == memory);
      }*/

      WHEN("Text is concatenated") {
         text += "test";

         Text_CheckState_OwnedFull(text);
         REQUIRE(text.GetCount() == 4);
         REQUIRE(text.GetReserved() >= 500);
         REQUIRE(text.GetRaw() == memory);
         REQUIRE(text == "test");
      }

      WHEN("Text is cleared") {
         text += "test";
         text.Clear();

         Text_CheckState_OwnedEmpty(text);
         REQUIRE(text.GetReserved() >= 500);
         REQUIRE(text.GetRaw() == memory);
         REQUIRE(text != "test");
      }

      WHEN("Text is reset") {
         text += "test";
         text.Reset();

         Text_CheckState_Default(text);
         REQUIRE(text != "test");
      }
   }

   GIVEN("Full text container") {
      T text {"test1"};
      Text_CheckState_OwnedFull(text);
      auto memory = text.GetRaw();

      WHEN("Add more text") {
         text += "test2";

         Text_CheckState_OwnedFull(text);
         REQUIRE(text == "test1test2");
         REQUIRE(text.GetCount() == 10);
         REQUIRE(text.GetReserved() >= 10);
         IF_LANGULUS_MANAGED_MEMORY(REQUIRE(text.GetRaw() == memory));
      }

      WHEN("More capacity is reserved") {
         text.Reserve(20);

         Text_CheckState_OwnedFull(text);
         REQUIRE(text.GetCount() == 5);
         REQUIRE(text.GetReserved() >= 20);
         IF_LANGULUS_MANAGED_MEMORY(REQUIRE(text.GetRaw() == memory));
      }

      /*WHEN("More capacity is reserved, via Extend()") {
         auto region = text.Extend(10); //TODO this requires to return a container with ownership component that allows for modification without branching

         Text_CheckState_OwnedFull(text);
         Text_CheckState_OwnedFull(region);
         REQUIRE(text.GetCount() == 15);
         REQUIRE(text.GetReserved() >= 15);
         IF_LANGULUS_MANAGED_MEMORY(REQUIRE(text.GetRaw() == memory));
         REQUIRE(region.GetCount() == 10);
         REQUIRE(region.GetRaw() == text.GetRaw() + 5);
      }*/

      WHEN("Less capacity is reserved") {
         text.Reserve(2);

         Text_CheckState_OwnedFull(text);
         REQUIRE(text.GetCount() == 2);
         REQUIRE(text.GetReserved() >= 5);
         REQUIRE(text.GetRaw() == memory);
         REQUIRE(text == "te");
      }

      WHEN("Text is cleared") {
         text.Clear();

         Text_CheckState_OwnedEmpty(text);
         REQUIRE(text.GetReserved() >= 5);
         REQUIRE(text.GetRaw() == memory);
         REQUIRE(text == "");
      }

      WHEN("Text is reset") {
         text.Reset();

         Text_CheckState_Default(text);
         REQUIRE(text == "");
      }

      WHEN("Text is copied shallowly") {
         T copy = text;

         Text_CheckState_OwnedFull(text);
         Text_CheckState_OwnedFull(copy);
         REQUIRE(text.GetCount() == copy.GetCount());
         REQUIRE(text.GetReserved() == copy.GetReserved());
         REQUIRE(text.GetRaw() == copy.GetRaw());
         REQUIRE(text.GetType() == copy.GetType());
         REQUIRE(copy.GetUses() == 2);
         REQUIRE(text.GetUses() == 2);
         REQUIRE(text == copy);
      }

      WHEN("Text is cloned (deep copy)") {
         T copy = Clone(text);

         Text_CheckState_OwnedFull(text);
         Text_CheckState_OwnedFull(copy);
         REQUIRE(text.GetCount() == copy.GetCount());
         REQUIRE(text.GetReserved() >= copy.GetReserved());
         REQUIRE(text.GetRaw() != copy.GetRaw());
         REQUIRE(text.GetType() == copy.GetType());
         REQUIRE(copy.GetUses() == 1);
         REQUIRE(text.GetUses() == 1);
         REQUIRE(text == copy);
      }

      WHEN("Text is reset, then allocated again") {
         text.Reset();
         text += "kurec";

         Text_CheckState_OwnedFull(text);
         REQUIRE(text.GetCount() == 5);
         REQUIRE(text.GetReserved() >= 5);
         REQUIRE(text == "kurec");
      }

      WHEN("Texts are compared") {
         REQUIRE(text == "test1");
         REQUIRE(text != "Tests");
      }
   }

   REQUIRE(memoryState.Assert());
   REQUIRE_FALSE(Allocator::CollectGarbage());
}

TEST_CASE_TEMPLATE("Unsigned number stringification", TestType,
   uint8_t, uint16_t, uint32_t, uint64_t
) {
   static MemoryState memoryState;

   WHEN("Constructed Text with a number") {
      Text* text = new Text {Text::FromNumber(TestType{66})};

      Text_CheckState_OwnedFull(*text);
      REQUIRE((*text).GetCount() == 2);
      REQUIRE((*text).GetReserved() >= 2);
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

TEST_CASE_TEMPLATE("Signed number stringification", TestType,
   int8_t, int16_t, int32_t, int64_t
) {
   static MemoryState memoryState;

   WHEN("Constructed Text with a number") {
      Text* text = new Text {Text::FromNumber(TestType{-66})};
      
      Text_CheckState_OwnedFull(*text);
      REQUIRE((*text).GetCount() == 3);
      REQUIRE((*text).GetReserved() >= 3);
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

TEST_CASE_TEMPLATE("Real number stringification", TestType,
   float, double
) {
   static MemoryState memoryState;

   WHEN("Constructed Text with a number") {
      Text* text = new Text {Text::FromNumber(TestType{-66.666}, 2)};

      Text_CheckState_OwnedFull(*text);
      REQUIRE((*text).GetCount() == 7);
      REQUIRE((*text).GetReserved() >= 7);
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

TEST_CASE_TEMPLATE("Logging text containers", TestType,
   Text/*TODO , Path*/
) {
   static MemoryState memoryState;

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

TEST_CASE_TEMPLATE("Reflected coverters to text", TestType,
   Stringifiable, StringifiableConst
) {
   static MemoryState memoryState;

   GIVEN("A stringifiable type") {
      const auto debugMeta = MetaOf<Text>();
      const auto meta = MetaOf<TestType>();
      TestType instance;

      WHEN("Converted") {
         // @attention calling static_cast<Text>(Stringifiable) won't   
         // work on MSVC due to a compiler bug with mutable cast        
         // operators                                                   
         const auto staticallyConverted = instance.operator Text();
         Text rttiConverted;
         meta.GetMorphism(debugMeta).convert(&instance, &rttiConverted);

         REQUIRE(staticallyConverted == rttiConverted);
         if constexpr (Akin<Stringifiable, TestType>)
            REQUIRE(staticallyConverted == "Stringifiable converted to Text");
         else
            REQUIRE(staticallyConverted == "StringifiableConst converted to Text");            
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
