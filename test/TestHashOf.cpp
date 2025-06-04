///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "Main.hpp"
#include <Langulus/HashOf.hpp>
#include <Langulus/CT/Nullable.hpp>
#include <Langulus/CT/POD.hpp>

using namespace Langulus;

static_assert(CT::Nullable<Hash>, "Hash needs to be batch-nullable");
static_assert(CT::POD<Hash>,      "Hash needs to be POD");

namespace Catch
{
   template <>
   struct StringMaker<Langulus::Hash> {
      static std::string convert(Langulus::Hash k) {
         return "Hash(" + std::to_string(k.mHash) + ")";
      }
   };
}

SCENARIO("Hashing standard text containers should result in the same hashes", "[hash]") {
   std::string_view same1 = "Same1";
   std::string_view same2 = "Same1";
   std::string same1str = "Same1";
   std::string same2str = "Same1";

   REQUIRE(HashOf(same1) == HashOf(same2));
   REQUIRE(HashOf(same1str) == HashOf(same2str));
   REQUIRE(HashOf(same1) == HashOf(same1str));
   REQUIRE(HashOf(same2) == HashOf(same2str));
}

SCENARIO("Hashing same values of differently sized types should result in different hashes", "[hash]") {
   auto init = GENERATE(0, 1, 100);
   bool b = init;
   char c = init;
   wchar_t wc = init;
   char8_t c8 = init;
   char16_t c16 = init;
   char32_t c32 = init;
   uint8_t u8 = init;
   uint16_t u16 = init;
   uint32_t u32 = init;
   uint64_t u64 = init;
   float f = init;
   double d = init;
   int8_t i8 = init;
   int16_t i16 = init;
   int32_t i32 = init;
   int64_t i64 = init;

   if (init <= 1) {
      REQUIRE(HashOf(b) == HashOf(c));
      REQUIRE(HashOf(b) == HashOf(c8));
      REQUIRE(HashOf(b) == HashOf(u8));
      REQUIRE(HashOf(b) == HashOf(i8));

      STATIC_REQUIRE(HashOf(b) == HashOf(c));
      STATIC_REQUIRE(HashOf(b) == HashOf(c8));
      STATIC_REQUIRE(HashOf(b) == HashOf(u8));
      STATIC_REQUIRE(HashOf(b) == HashOf(i8));
   }
   else {
      REQUIRE(HashOf(b) != HashOf(c));
      REQUIRE(HashOf(b) != HashOf(c8));
      REQUIRE(HashOf(b) != HashOf(u8));
      REQUIRE(HashOf(b) != HashOf(i8));

      STATIC_REQUIRE(HashOf(b) != HashOf(c));
      STATIC_REQUIRE(HashOf(b) != HashOf(c8));
      STATIC_REQUIRE(HashOf(b) != HashOf(u8));
      STATIC_REQUIRE(HashOf(b) != HashOf(i8));
   }

   REQUIRE(HashOf(b) != HashOf(wc));
   REQUIRE(HashOf(b) != HashOf(c16));
   REQUIRE(HashOf(b) != HashOf(c32));
   REQUIRE(HashOf(b) != HashOf(u16));
   REQUIRE(HashOf(b) != HashOf(u32));
   REQUIRE(HashOf(b) != HashOf(u64));
   REQUIRE(HashOf(b) != HashOf(i16));
   REQUIRE(HashOf(b) != HashOf(i32));
   REQUIRE(HashOf(b) != HashOf(i64));
   REQUIRE(HashOf(b) != HashOf(f));
   REQUIRE(HashOf(b) != HashOf(d));

   STATIC_REQUIRE(HashOf(b) != HashOf(wc));
   STATIC_REQUIRE(HashOf(b) != HashOf(c16));
   STATIC_REQUIRE(HashOf(b) != HashOf(c32));
   STATIC_REQUIRE(HashOf(b) != HashOf(u16));
   STATIC_REQUIRE(HashOf(b) != HashOf(u32));
   STATIC_REQUIRE(HashOf(b) != HashOf(u64));
   STATIC_REQUIRE(HashOf(b) != HashOf(i16));
   STATIC_REQUIRE(HashOf(b) != HashOf(i32));
   STATIC_REQUIRE(HashOf(b) != HashOf(i64));
   STATIC_REQUIRE(HashOf(b) != HashOf(f));
   STATIC_REQUIRE(HashOf(b) != HashOf(d));

   if constexpr (sizeof(wchar_t) == 2) {
      REQUIRE(HashOf(c16) == HashOf(wc));
      REQUIRE(HashOf(c16) == HashOf(wc));
   }

   REQUIRE(HashOf(c16) == HashOf(u16));
   REQUIRE(HashOf(c16) == HashOf(i16));

   REQUIRE(HashOf(c16) != HashOf(c32));
   REQUIRE(HashOf(c16) != HashOf(u32));
   REQUIRE(HashOf(c16) != HashOf(u64));
   REQUIRE(HashOf(c16) != HashOf(i32));
   REQUIRE(HashOf(c16) != HashOf(i64));
   REQUIRE(HashOf(c16) != HashOf(f));
   REQUIRE(HashOf(c16) != HashOf(d));

   STATIC_REQUIRE(HashOf(c16) == HashOf(u16));
   STATIC_REQUIRE(HashOf(c16) == HashOf(i16));

   STATIC_REQUIRE(HashOf(c16) != HashOf(c32));
   STATIC_REQUIRE(HashOf(c16) != HashOf(u32));
   STATIC_REQUIRE(HashOf(c16) != HashOf(u64));
   STATIC_REQUIRE(HashOf(c16) != HashOf(i32));
   STATIC_REQUIRE(HashOf(c16) != HashOf(i64));
   STATIC_REQUIRE(HashOf(c16) != HashOf(f));
   STATIC_REQUIRE(HashOf(c16) != HashOf(d));

   if constexpr (sizeof(wchar_t) == 4) {
      REQUIRE(HashOf(c32) == HashOf(wc));
      STATIC_REQUIRE(HashOf(c32) == HashOf(wc));
   }

   REQUIRE(HashOf(c32) == HashOf(u32));
   REQUIRE(HashOf(c32) == HashOf(i32));

   REQUIRE(HashOf(c32) != HashOf(c16));
   REQUIRE(HashOf(c32) != HashOf(u64));
   REQUIRE(HashOf(c32) != HashOf(i64));

   STATIC_REQUIRE(HashOf(c32) == HashOf(u32));
   STATIC_REQUIRE(HashOf(c32) == HashOf(i32));

   STATIC_REQUIRE(HashOf(c32) != HashOf(c16));
   STATIC_REQUIRE(HashOf(c32) != HashOf(u64));
   STATIC_REQUIRE(HashOf(c32) != HashOf(i64));

   if (init == 0 and sizeof(float) == 4)
      REQUIRE(HashOf(c32) == HashOf(f));
   else
      REQUIRE(HashOf(c32) != HashOf(f));

   REQUIRE(HashOf(c32) != HashOf(d));

   REQUIRE(HashOf(i64) == HashOf(u64));
   REQUIRE(HashOf(i64) != HashOf(c16));
   REQUIRE(HashOf(i64) != HashOf(c32));
   REQUIRE(HashOf(i64) != HashOf(u32));
   REQUIRE(HashOf(i64) != HashOf(i32));

   REQUIRE(HashOf(i64) != HashOf(f));

   STATIC_REQUIRE(HashOf(c32) != HashOf(d));

   STATIC_REQUIRE(HashOf(i64) == HashOf(u64));
   STATIC_REQUIRE(HashOf(i64) != HashOf(c16));
   STATIC_REQUIRE(HashOf(i64) != HashOf(c32));
   STATIC_REQUIRE(HashOf(i64) != HashOf(u32));
   STATIC_REQUIRE(HashOf(i64) != HashOf(i32));

   STATIC_REQUIRE(HashOf(i64) != HashOf(f));

   if (init == 0 and sizeof(double) == 8)
      REQUIRE(HashOf(i64) == HashOf(d));
   else
      REQUIRE(HashOf(i64) != HashOf(d));
}