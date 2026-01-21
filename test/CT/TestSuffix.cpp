///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "../Main.hpp"
#include <Langulus/CT/Suffix.hpp>
#include <Langulus/Logger.hpp>

using namespace Langulus;


namespace
{
   struct SuffixUsingMember {
      using CTTI_Suffix = Yes<"yeah">;
   };
   struct NastySuffixUsingMember {
      using CTTI_Suffix = Yes<"yeаh">; // contains cyrillic 'а'
   };
   struct SuffixBySpecialization {};
   struct NoSuffix {};
   struct IncompleteType;
}

namespace Langulus::CTTI
{
   template<>
   struct Suffix<SuffixBySpecialization> {
      static constexpr Literal Name = "yeah";
   };
   template<>
   struct Suffix<SuffixBySpecialization*> {
      static constexpr Literal Name = "yeahPtr";
   };
   template<>
   struct Suffix<SuffixBySpecialization const*> {
      static constexpr Literal Name = "yeahCptr";
   };
}

#define DEFINE_SUFFIXOF_TYPE_TEST(WHAT, RESULT) \
   WHEN("Taken the suffix of type " #WHAT) { \
      static_assert(SuffixOf<WHAT>() == RESULT); \
   }


SCENARIO("SuffixOf") {
   DEFINE_SUFFIXOF_TYPE_TEST(void, "")
   DEFINE_SUFFIXOF_TYPE_TEST(nullptr_t, "")
   DEFINE_SUFFIXOF_TYPE_TEST(int32_t(&)[5], "") 
   DEFINE_SUFFIXOF_TYPE_TEST(int32_t[5], "")    
   DEFINE_SUFFIXOF_TYPE_TEST(SuffixUsingMember, "yeah")         
   DEFINE_SUFFIXOF_TYPE_TEST(SuffixUsingMember&, "yeah")         
   DEFINE_SUFFIXOF_TYPE_TEST(SuffixUsingMember const, "yeah")
   DEFINE_SUFFIXOF_TYPE_TEST(SuffixUsingMember const*, "")
   DEFINE_SUFFIXOF_TYPE_TEST(SuffixBySpecialization, "yeah")         
   DEFINE_SUFFIXOF_TYPE_TEST(SuffixBySpecialization&, "yeah")         
   DEFINE_SUFFIXOF_TYPE_TEST(SuffixBySpecialization const, "yeah")
   DEFINE_SUFFIXOF_TYPE_TEST(SuffixBySpecialization*, "yeahPtr")
   DEFINE_SUFFIXOF_TYPE_TEST(SuffixBySpecialization const*, "yeahCptr")
   DEFINE_SUFFIXOF_TYPE_TEST(NoSuffix, "")

   DEFINE_SUFFIXOF_TYPE_TEST(bool,  "b")
   
   DEFINE_SUFFIXOF_TYPE_TEST(uint8_t,  "u8")
   DEFINE_SUFFIXOF_TYPE_TEST(uint16_t, "u16")
   DEFINE_SUFFIXOF_TYPE_TEST(uint32_t, (::std::same_as<uint32_t, unsigned int> ? "u" : "u32"))
   DEFINE_SUFFIXOF_TYPE_TEST(uint64_t, (::std::same_as<uint64_t, unsigned int> ? "u" : "u64"))
   
   DEFINE_SUFFIXOF_TYPE_TEST( int8_t,  "i8")
   DEFINE_SUFFIXOF_TYPE_TEST( int16_t, "i16")
   DEFINE_SUFFIXOF_TYPE_TEST( int32_t, (::std::same_as<int32_t, int> ? "i" : "i32"))
   DEFINE_SUFFIXOF_TYPE_TEST( int64_t, (::std::same_as<int64_t, int> ? "i" : "i64"))
   
   DEFINE_SUFFIXOF_TYPE_TEST( float,   (::std::same_as<float,  Real> ? "" : "f"))
   DEFINE_SUFFIXOF_TYPE_TEST( double,  (::std::same_as<double, Real> ? "" : "d"))
   
   WHEN("Taken the suffix of type NastySuffixUsingMember (with cyrillic 'a')") {
      //STATIC_REQUIRE(SuffixOf<NastySuffixUsingMember>()); // shouldn't compile at all
   }

   //DEFINE_SUFFIXOF_TYPE_TEST(IncompleteType, "") // shouldn't compile at all
}
