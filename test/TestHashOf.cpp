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
#include <map>
#include <set>
#include <unordered_map>
#include <unordered_set>

using namespace Langulus;


///                                                                           
/// MARK: CT::Hashable                                                        
///                                                                           
namespace
{
   template<class T>
   struct SheddableType { using CTTI_Sheddable = T; using CTTI_Typed = T; };

   struct NonHashable {
      int blah = 5;
   };

   struct HashableViaConstMethod {
      constexpr Hash GetHash() const {
         return {666};
      }
   };

   struct HashableViaMutMethod {
      constexpr Hash GetHash() {
         return {666};
      }
   };

   struct HashableViaBeingPOD : NonHashable {
      using CTTI_POD = Yes<>;
   };

   struct HashableViaBoth : HashableViaConstMethod, HashableViaBeingPOD {};
}

TEST_CASE_TEMPLATE("Testing hashable types", TestType
   , SheddableType<HashableViaConstMethod>
   , SheddableType<HashableViaBeingPOD>
   , SheddableType<HashableViaBoth>
   , SheddableType<NonHashable*>
   , HashableViaConstMethod
   , HashableViaMutMethod
   , HashableViaBeingPOD
   , HashableViaBoth
   , HashableViaBoth&
   , NonHashable*
   , int, float, bool, nullptr_t, void*
   , int&
   , int*
) {
   static_assert(    CT::Hashable<TestType>);
   static_assert(not CT::NotHashable<TestType>);
}

TEST_CASE_TEMPLATE("Testing non-hashable types", TestType
   , SheddableType<NonHashable>
   , SheddableType<NonHashable&>
   , NonHashable
   , NonHashable&
   , void
) {
   static_assert(not CT::Hashable<TestType>);
   static_assert(    CT::NotHashable<TestType>);
}

//static_assert(CT::Hashable<>); // shouldn't compile at all
static_assert(    CT::Hashable<SheddableType<HashableViaConstMethod>, HashableViaConstMethod, NonHashable*>);
static_assert(not CT::Hashable<SheddableType<HashableViaConstMethod>, HashableViaConstMethod, NonHashable>);

//static_assert(CT::NotHashable<>); // shouldn't compile at all
static_assert(    CT::NotHashable<SheddableType<NonHashable>, NonHashable, void>);
static_assert(not CT::NotHashable<SheddableType<NonHashable>, NonHashable, HashableViaBoth>);


///                                                                           
/// MARK: CT::HasGetHashMethod                                                
///                                                                           
TEST_CASE_TEMPLATE("Testing types with GetHash() method", TestType
   , SheddableType<HashableViaConstMethod>
   , SheddableType<HashableViaBoth>
   , HashableViaConstMethod
   , HashableViaMutMethod
   , HashableViaBoth
) {
   static_assert(CT::HasGetHashMethod<TestType>);
}

TEST_CASE_TEMPLATE("Testing types without GetHash() method", TestType
   , SheddableType<HashableViaBeingPOD>
   , HashableViaBeingPOD
   , NonHashable
   , int, int&, int*, void
) {
   static_assert(not CT::HasGetHashMethod<TestType>);
}

//static_assert(CT::HasGetHashMethod<>); // shouldn't compile at all
static_assert(    CT::HasGetHashMethod<SheddableType<HashableViaConstMethod>, HashableViaConstMethod, HashableViaBoth>);
static_assert(not CT::HasGetHashMethod<SheddableType<HashableViaConstMethod>, HashableViaConstMethod, HashableViaBeingPOD>);


///                                                                           
/// MARK: CT::HasStdHasher                                                    
///                                                                           
TEST_CASE_TEMPLATE("Testing for if types are hashable by std", TestType
   , std::string
   , std::string_view, int
   , SheddableType<HashableViaConstMethod>
   , SheddableType<HashableViaBoth>
   , HashableViaConstMethod
   , HashableViaMutMethod
   , HashableViaBoth
) {
   static_assert(CT::HasStdHasher<TestType>);
}

TEST_CASE_TEMPLATE("Testing for if types are not hashable by std", TestType
   , std::unordered_map<int, bool>
   , std::unordered_set<int>
   , std::set<int>
   , std::array<int, 5>
   , std::map<int, bool>
   , SheddableType<std::map<int, bool>>
   , void
   , NonHashable
   , HashableViaBeingPOD
) {
   static_assert(not CT::HasStdHasher<TestType>);
}

//static_assert(CT::HasStdHasher<>); // shouldn't compile at all
static_assert(    CT::HasStdHasher<std::string, SheddableType<HashableViaConstMethod>, HashableViaBoth>);
static_assert(not CT::HasStdHasher<std::string, SheddableType<HashableViaConstMethod>, NonHashable>);


///                                                                           
/// MARK: Hash type tests                                                     
///                                                                           
static_assert(CT::Nullable<Hash>, "Hash needs to be batch-nullable");
static_assert(CT::POD<Hash>,      "Hash needs to be POD");

namespace doctest
{
   template<>
   struct StringMaker<Langulus::Hash> {
      static String convert(const Langulus::Hash& k) {
         return "Hash(" + toString(k.value) + ")";
      }
   };
}


///                                                                           
/// MARK: Hashing using standard containers                                   
///                                                                           
SCENARIO("Hashing standard containers should result in the same hashes") {
   std::string_view same1 = "Same1";
   std::string_view same2 = "Same1";
   std::string same1str = "Same1";
   std::string same2str = "Same1";
   std::array<char, 5> same1arr = {'S', 'a', 'm', 'e', '1'};

   REQUIRE(HashOf(same1) == HashOf(same2));
   REQUIRE(HashOf(same1str) == HashOf(same2str));
   REQUIRE(HashOf(same1) == HashOf(same1str));
   REQUIRE(HashOf(same2) == HashOf(same2str));

   static_assert(::std::ranges::range<decltype(same1arr)>);
   static_assert(CT::Hashable<TypeOf<decltype(same1arr)>>);
   REQUIRE(HashOf(same2) == HashOf(same1arr));

   constexpr std::array<Hash, 5> c_same2arr = {
      HashOf('S'),
      HashOf('a'),
      HashOf('m'),
      HashOf('e'),
      HashOf('2')
   };

   REQUIRE(HashOf(c_same2arr) == HashOf('S', 'a', 'm', 'e', '2'));
   static_assert(HashOf(c_same2arr) == HashOf('S', 'a', 'm', 'e', '2'));

   constexpr std::array<Hash, 1> c_wrappedHash = {Hash{666}};
   REQUIRE(HashOf(c_wrappedHash) == Hash {666});

   static_assert(HashOf(HashableViaConstMethod {}) == Hash {666});

   std::vector<HashableViaConstMethod> vec3;
   vec3.emplace_back();
   vec3.emplace_back();
   vec3.emplace_back();
   REQUIRE(HashOf(vec3) == HashOf(HashableViaConstMethod {}, HashableViaConstMethod {}, HashableViaConstMethod {}));

   HashableViaConstMethod vec3ca[3] = {
      HashableViaConstMethod {},
      HashableViaConstMethod {},
      HashableViaConstMethod {}
   };
   REQUIRE(HashOf(vec3ca) == HashOf(HashableViaConstMethod {}, HashableViaConstMethod {}, HashableViaConstMethod {}));
   static_assert(HashOf(vec3ca) == HashOf(HashableViaConstMethod {}, HashableViaConstMethod {}, HashableViaConstMethod {}));

   std::vector<HashableViaConstMethod> vec1;
   vec1.emplace_back();
   REQUIRE(HashOf(vec1) == Hash {666});

   HashableViaConstMethod vec1ca[1] = {HashableViaConstMethod {}};
   REQUIRE(HashOf(vec1ca) == Hash {666});
   static_assert(HashOf(vec1ca) == Hash {666});
}

template<int V>
struct TestValue {
   static constexpr int Value = V;
};


///                                                                           
/// MARK:: Hash similarities with fundamental types (and constexpr hashing)   
///                                                                           
TEST_CASE_TEMPLATE("Hashing same values of differently sized types should result in different hashes", TestType
   , TestValue<0>
   , TestValue<1>
   , TestValue<2>
) {
   constexpr int init = TestType::Value;
   bool b       = static_cast<bool>(init);
   char c       = static_cast<char>(init);
   wchar_t wc   = static_cast<wchar_t>(init);
   char8_t c8   = static_cast<char8_t>(init);
   char16_t c16 = static_cast<char16_t>(init);
   char32_t c32 = static_cast<char32_t>(init);
   uint8_t u8   = static_cast<uint8_t>(init);
   uint16_t u16 = static_cast<uint16_t>(init);
   uint32_t u32 = static_cast<uint32_t>(init);
   uint64_t u64 = static_cast<uint64_t>(init);
   float f      = static_cast<float>(init);
   double d     = static_cast<double>(init);
   int8_t i8    = static_cast<int8_t>(init);
   int16_t i16  = static_cast<int16_t>(init);
   int32_t i32  = static_cast<int32_t>(init);
   int64_t i64  = static_cast<int64_t>(init);
   void* ptr    = reinterpret_cast<void*>(static_cast<intptr_t>(init));

   REQUIRE(HashOf(b) != Hash {});
   REQUIRE(HashOf(c) != Hash {});
   REQUIRE(HashOf(wc) != Hash {});
   REQUIRE(HashOf(c8) != Hash {});
   REQUIRE(HashOf(c16) != Hash {});
   REQUIRE(HashOf(c32) != Hash {});
   REQUIRE(HashOf(u8) != Hash {});
   REQUIRE(HashOf(u16) != Hash {});
   REQUIRE(HashOf(u32) != Hash {});
   REQUIRE(HashOf(u64) != Hash {});
   REQUIRE(HashOf(f) != Hash {});
   REQUIRE(HashOf(d) != Hash {});
   REQUIRE(HashOf(i8) != Hash {});
   REQUIRE(HashOf(i16) != Hash {});
   REQUIRE(HashOf(i32) != Hash {});
   REQUIRE(HashOf(i64) != Hash {});
   REQUIRE(HashOf(ptr) != Hash {});

   if constexpr (init <= 1) {
      REQUIRE(HashOf(b) == HashOf(c));
      REQUIRE(HashOf(b) == HashOf(c8));
      REQUIRE(HashOf(b) == HashOf(u8));
      REQUIRE(HashOf(b) == HashOf(i8));

      static_assert(HashOf(static_cast<bool>(init)) == HashOf(static_cast<char>(init)));
      static_assert(HashOf(static_cast<bool>(init)) == HashOf(static_cast<char8_t>(init)));
      static_assert(HashOf(static_cast<bool>(init)) == HashOf(static_cast<uint8_t>(init)));
      static_assert(HashOf(static_cast<bool>(init)) == HashOf(static_cast<int8_t>(init)));
   }
   else {
      REQUIRE(HashOf(b) != HashOf(c));
      REQUIRE(HashOf(b) != HashOf(c8));
      REQUIRE(HashOf(b) != HashOf(u8));
      REQUIRE(HashOf(b) != HashOf(i8));

      static_assert(HashOf(static_cast<bool>(init)) != HashOf(static_cast<char>(init)));
      static_assert(HashOf(static_cast<bool>(init)) != HashOf(static_cast<char8_t>(init)));
      static_assert(HashOf(static_cast<bool>(init)) != HashOf(static_cast<uint8_t>(init)));
      static_assert(HashOf(static_cast<bool>(init)) != HashOf(static_cast<int8_t>(init)));
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

   static_assert(HashOf(static_cast<bool>(init)) != HashOf(static_cast<wchar_t>(init)));
   static_assert(HashOf(static_cast<bool>(init)) != HashOf(static_cast<char16_t>(init)));
   static_assert(HashOf(static_cast<bool>(init)) != HashOf(static_cast<char32_t>(init)));
   static_assert(HashOf(static_cast<bool>(init)) != HashOf(static_cast<uint16_t>(init)));
   static_assert(HashOf(static_cast<bool>(init)) != HashOf(static_cast<uint32_t>(init)));
   static_assert(HashOf(static_cast<bool>(init)) != HashOf(static_cast<uint64_t>(init)));
   static_assert(HashOf(static_cast<bool>(init)) != HashOf(static_cast<int16_t>(init)));
   static_assert(HashOf(static_cast<bool>(init)) != HashOf(static_cast<int32_t>(init)));
   static_assert(HashOf(static_cast<bool>(init)) != HashOf(static_cast<int64_t>(init)));
   static_assert(HashOf(static_cast<bool>(init)) != HashOf(static_cast<float>(init)));
   static_assert(HashOf(static_cast<bool>(init)) != HashOf(static_cast<double>(init)));

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

   static_assert(HashOf(static_cast<char16_t>(init)) == HashOf(static_cast<uint16_t>(init)));
   static_assert(HashOf(static_cast<char16_t>(init)) == HashOf(static_cast<int16_t>(init)));

   static_assert(HashOf(static_cast<char16_t>(init)) != HashOf(static_cast<char32_t>(init)));
   static_assert(HashOf(static_cast<char16_t>(init)) != HashOf(static_cast<uint32_t>(init)));
   static_assert(HashOf(static_cast<char16_t>(init)) != HashOf(static_cast<uint64_t>(init)));
   static_assert(HashOf(static_cast<char16_t>(init)) != HashOf(static_cast<int32_t>(init)));
   static_assert(HashOf(static_cast<char16_t>(init)) != HashOf(static_cast<int64_t>(init)));
   static_assert(HashOf(static_cast<char16_t>(init)) != HashOf(static_cast<float>(init)));
   static_assert(HashOf(static_cast<char16_t>(init)) != HashOf(static_cast<double>(init)));

   if constexpr (sizeof(wchar_t) == 4) {
      REQUIRE(HashOf(c32) == HashOf(wc));
      static_assert(HashOf(static_cast<char32_t>(init)) == HashOf(static_cast<wchar_t>(init)));
   }

   REQUIRE(HashOf(c32) == HashOf(u32));
   REQUIRE(HashOf(c32) == HashOf(i32));

   REQUIRE(HashOf(c32) != HashOf(c16));
   REQUIRE(HashOf(c32) != HashOf(u64));
   REQUIRE(HashOf(c32) != HashOf(i64));

   static_assert(HashOf(static_cast<char32_t>(init)) == HashOf(static_cast<uint32_t>(init)));
   static_assert(HashOf(static_cast<char32_t>(init)) == HashOf(static_cast<int32_t>(init)));

   static_assert(HashOf(static_cast<char32_t>(init)) != HashOf(static_cast<char16_t>(init)));
   static_assert(HashOf(static_cast<char32_t>(init)) != HashOf(static_cast<uint64_t>(init)));
   static_assert(HashOf(static_cast<char32_t>(init)) != HashOf(static_cast<int64_t>(init)));

   if constexpr (init == 0 and sizeof(float) == 4)
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

   static_assert(HashOf(static_cast<char32_t>(init)) != HashOf(static_cast<double>(init)));

   static_assert(HashOf(static_cast<int64_t>(init)) == HashOf(static_cast<uint64_t>(init)));
   static_assert(HashOf(static_cast<int64_t>(init)) != HashOf(static_cast<char16_t>(init)));
   static_assert(HashOf(static_cast<int64_t>(init)) != HashOf(static_cast<char32_t>(init)));
   static_assert(HashOf(static_cast<int64_t>(init)) != HashOf(static_cast<uint32_t>(init)));
   static_assert(HashOf(static_cast<int64_t>(init)) != HashOf(static_cast<int32_t>(init)));

   static_assert(HashOf(static_cast<int64_t>(init)) != HashOf(static_cast<float>(init)));

   if constexpr (init == 0 and sizeof(double) == 8)
      REQUIRE(HashOf(i64) == HashOf(d));
   else
      REQUIRE(HashOf(i64) != HashOf(d));
}
