///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "Main.hpp"
#include <Langulus/CT/Suffix.hpp>
#include <Langulus/Logger.hpp>
#include <string>

using namespace Langulus;

namespace
{

   struct TypeWithSuffix { using CTTI_Suffix = YesText<"yeah">; };
   struct TypeWithoutSuffix {};

}

SCENARIO("SuffixOf", "[ct]") {
   WHEN("Generating a suffix for uint8_t") {
      auto token = SuffixOf<uint8_t>();
      REQUIRE(token == "u8");
      STATIC_REQUIRE(SuffixOf<uint8_t>() == "u8");
   }

   WHEN("Generating a suffix for uint16_t") {
      auto token = SuffixOf<uint16_t>();
      REQUIRE(token == "u16");
      STATIC_REQUIRE(SuffixOf<uint16_t>() == "u16");
   }

   WHEN("Generating a suffix for uint32_t") {
      auto token = SuffixOf<uint32_t>();
      if constexpr (CT::Same<uint32_t, unsigned int>) {
         REQUIRE(token == "u");
         STATIC_REQUIRE(SuffixOf<uint32_t>() == "u");
      }
      else {
         REQUIRE(token == "u32");
         STATIC_REQUIRE(SuffixOf<uint32_t>() == "u32");
      }
   }
   
   WHEN("Generating a suffix for uint64_t") {
      auto token = SuffixOf<uint64_t>();
      if constexpr (CT::Same<uint64_t, unsigned int>) {
         REQUIRE(token == "u");
         STATIC_REQUIRE(SuffixOf<uint64_t>() == "u");
      }
      else {
         REQUIRE(token == "u64");
         STATIC_REQUIRE(SuffixOf<uint64_t>() == "u64");
      }
   }

   WHEN("Generating a suffix for int8_t") {
      auto token = SuffixOf<int8_t>();
      REQUIRE(token == "i8");
      STATIC_REQUIRE(SuffixOf<int8_t>() == "i8");
   }

   WHEN("Generating a suffix for int16_t") {
      auto token = SuffixOf<int16_t>();
      REQUIRE(token == "i16");
      STATIC_REQUIRE(SuffixOf<int16_t>() == "i16");
   }

   WHEN("Generating a suffix for int32_t") {
      auto token = SuffixOf<int32_t>();
      if constexpr (CT::Same<int32_t, signed int>) {
         REQUIRE(token == "i");
         STATIC_REQUIRE(SuffixOf<int32_t>() == "i");
      }
      else {
         REQUIRE(token == "i32");
         STATIC_REQUIRE(SuffixOf<int32_t>() == "i32");
      }
   }
   
   WHEN("Generating a suffix for int64_t") {
      auto token = SuffixOf<int64_t>();
      if constexpr (CT::Same<int64_t, signed int>) {
         REQUIRE(token == "i");
         STATIC_REQUIRE(SuffixOf<int64_t>() == "i");
      }
      else {
         REQUIRE(token == "i64");
         STATIC_REQUIRE(SuffixOf<int64_t>() == "i64");
      }
   }

   WHEN("Generating a suffix for float") {
      auto token = SuffixOf<float>();
      if constexpr (CT::Same<float, Real>) {
         REQUIRE(token == "");
         STATIC_REQUIRE(SuffixOf<float>() == "");
      }
      else {
         REQUIRE(token == "f");
         STATIC_REQUIRE(SuffixOf<float>() == "f");
      }
   }

   WHEN("Generating a suffix for double") {
      auto token = SuffixOf<double>();
      if constexpr (CT::Same<double, Real>) {
         REQUIRE(token == "");
         STATIC_REQUIRE(SuffixOf<double>() == "");
      }
      else {
         REQUIRE(token == "d");
         STATIC_REQUIRE(SuffixOf<double>() == "d");
      }
   }

   WHEN("Generating a suffix for bool") {
      auto token = SuffixOf<bool>();
      REQUIRE(token == "b");
      STATIC_REQUIRE(SuffixOf<bool>() == "b");
   }

   WHEN("Generating a suffix for a type with CTTI_Suffix") {
      auto token = SuffixOf<TypeWithSuffix>();
      REQUIRE(token == "yeah");
      STATIC_REQUIRE(SuffixOf<TypeWithSuffix>() == "yeah");
   }

   WHEN("Generating a suffix for a type without CTTI_Suffix") {
      auto token = SuffixOf<TypeWithoutSuffix>();
      REQUIRE(token == "");
      STATIC_REQUIRE(SuffixOf<TypeWithoutSuffix>() == "");
   }
}