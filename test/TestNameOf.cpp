///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "Main.hpp"
#include "TestTypes/CommonTypes.hpp"
#include <Langulus/NameOf.hpp>
#include <Langulus/NameOf-Runtime.hpp>

#if LANGULUS_FEATURE(MANAGED_MEMORY)
   #include "TestTypes/PackedPointers.hpp"
#endif

#if 0
   #include <Langulus/Logger.hpp>
   #define VERBOSE(...) Logger::Verbose(__VA_ARGS__)
#else
   #define VERBOSE(...)
#endif

using namespace Langulus;

namespace Langulus::CTTI
{
   template<>
   struct Named<NamedBySpecialization> {
      static constexpr Literal Name = "NameOverridedBySpecializing_CTTI_Named";
   };
   template<>
   struct Named<NamedBySpecialization*> {
      static constexpr Literal Name = "NameOverridedBySpecializing_CTTI_Named_Ptr";
   };
   template<>
   struct Named<NamedBySpecialization const*> {
      static constexpr Literal Name = "NameOverridedBySpecializing_CTTI_Named_ConstPtr";
   };

   template<>
   struct NamedValue<PiButNamed::Number> {
      static constexpr Literal Name = "PiButNamedByCTTI";
   };
   template<>
   struct NamedValue<NumberButNamed> {
      static constexpr Literal Name = "NumberButNamedByCTTI";
   };
   template<>
   struct NamedValue<AnonymousNumberButNamed> {
      static constexpr Literal Name = "AnonymousNumberButNamedByCTTI";
   };
}


#define DEFINE_NAMEOF_TYPE_TEST(WHAT, RESULT) \
   WHEN("Taken the name of type " #WHAT) { \
      REQUIRE(NameOfRt<WHAT>() == RESULT); \
      REQUIRE(TokenOf<WHAT>() == RESULT); \
      static_assert(NameOf<WHAT>() == RESULT); \
   }

#define DEFINE_NAMEOF_CONST_TEST(WHAT, RESULT) \
   WHEN("Taken the name of constant " #WHAT) { \
      REQUIRE(NameOfRt<WHAT>() == RESULT); \
      REQUIRE(TokenOf<WHAT>() == RESULT); \
      static_assert(NameOf<WHAT>() == RESULT); \
   }


SCENARIO("NameOf") {
   DEFINE_NAMEOF_TYPE_TEST(void, "void")
   //DEFINE_NAMEOF_TYPE_TEST(nullptr_t, "null") // no longer valid
   DEFINE_NAMEOF_TYPE_TEST(int32_t(&)[5], "int32[5]&")
   DEFINE_NAMEOF_TYPE_TEST(int32_t[5], "int32[5]")
   
   DEFINE_NAMEOF_TYPE_TEST(int, "int32")
   DEFINE_NAMEOF_TYPE_TEST(int&, "int32&")
   DEFINE_NAMEOF_TYPE_TEST(const int, "int32 const")
   DEFINE_NAMEOF_TYPE_TEST(const int*, "int32 const*")
   DEFINE_NAMEOF_TYPE_TEST(const int**, "int32 const**")
   DEFINE_NAMEOF_TYPE_TEST(const int* const*, "int32 const* const*")
   DEFINE_NAMEOF_TYPE_TEST(const int* const* const, "int32 const* const* const")
   
   DEFINE_NAMEOF_TYPE_TEST(uint16_t, "uint16")
   DEFINE_NAMEOF_TYPE_TEST(uint16_t&, "uint16&")
   DEFINE_NAMEOF_TYPE_TEST(const uint16_t, "uint16 const")
   DEFINE_NAMEOF_TYPE_TEST(const uint16_t*, "uint16 const*")
   DEFINE_NAMEOF_TYPE_TEST(const uint16_t**, "uint16 const**")
   DEFINE_NAMEOF_TYPE_TEST(const uint16_t* const*, "uint16 const* const*")
   DEFINE_NAMEOF_TYPE_TEST(const uint16_t* const* const, "uint16 const* const* const")
   DEFINE_NAMEOF_TYPE_TEST(uint16_t*, "uint16*")

   DEFINE_NAMEOF_TYPE_TEST(A, "A")
   DEFINE_NAMEOF_TYPE_TEST(B, "B")
   DEFINE_NAMEOF_TYPE_TEST(C, "C")
   DEFINE_NAMEOF_TYPE_TEST(D, "D")
   DEFINE_NAMEOF_TYPE_TEST(E, "E")
   DEFINE_NAMEOF_TYPE_TEST(F, "F")
   DEFINE_NAMEOF_TYPE_TEST(G, "G")
   DEFINE_NAMEOF_TYPE_TEST(H, "H")
   DEFINE_NAMEOF_TYPE_TEST(I, "I")
   DEFINE_NAMEOF_TYPE_TEST(J, "J")
   DEFINE_NAMEOF_TYPE_TEST(K, "K")
   DEFINE_NAMEOF_TYPE_TEST(L, "L")
   DEFINE_NAMEOF_TYPE_TEST(M, "M")
   DEFINE_NAMEOF_TYPE_TEST(N, "N")
   DEFINE_NAMEOF_TYPE_TEST(O, "O")
   DEFINE_NAMEOF_TYPE_TEST(P, "P")
   DEFINE_NAMEOF_TYPE_TEST(Q, "Q")
   DEFINE_NAMEOF_TYPE_TEST(R, "R")
   DEFINE_NAMEOF_TYPE_TEST(S, "S")
   DEFINE_NAMEOF_TYPE_TEST(T, "T")
   DEFINE_NAMEOF_TYPE_TEST(U, "U")
   DEFINE_NAMEOF_TYPE_TEST(V, "V")
   DEFINE_NAMEOF_TYPE_TEST(W, "W")
   DEFINE_NAMEOF_TYPE_TEST(X, "X")
   DEFINE_NAMEOF_TYPE_TEST(Y, "Y")
   DEFINE_NAMEOF_TYPE_TEST(Z, "Z")
   DEFINE_NAMEOF_TYPE_TEST(_, "_")

   DEFINE_NAMEOF_TYPE_TEST(a, "a")
   DEFINE_NAMEOF_TYPE_TEST(b, "b")
   DEFINE_NAMEOF_TYPE_TEST(c, "c")
   DEFINE_NAMEOF_TYPE_TEST(d, "d")
   DEFINE_NAMEOF_TYPE_TEST(e, "e")
   DEFINE_NAMEOF_TYPE_TEST(f, "f")
   DEFINE_NAMEOF_TYPE_TEST(g, "g")
   DEFINE_NAMEOF_TYPE_TEST(h, "h")
   DEFINE_NAMEOF_TYPE_TEST(i, "i")
   DEFINE_NAMEOF_TYPE_TEST(j, "j")
   DEFINE_NAMEOF_TYPE_TEST(k, "k")
   DEFINE_NAMEOF_TYPE_TEST(l, "l")
   DEFINE_NAMEOF_TYPE_TEST(m, "m")
   DEFINE_NAMEOF_TYPE_TEST(n, "n")
   DEFINE_NAMEOF_TYPE_TEST(o, "o")
   DEFINE_NAMEOF_TYPE_TEST(p, "p")
   DEFINE_NAMEOF_TYPE_TEST(q, "q")
   DEFINE_NAMEOF_TYPE_TEST(r, "r")
   DEFINE_NAMEOF_TYPE_TEST(s, "s")
   DEFINE_NAMEOF_TYPE_TEST(t, "t")
   DEFINE_NAMEOF_TYPE_TEST(u, "u")
   DEFINE_NAMEOF_TYPE_TEST(v, "v")
   DEFINE_NAMEOF_TYPE_TEST(w, "w")
   DEFINE_NAMEOF_TYPE_TEST(x, "x")
   DEFINE_NAMEOF_TYPE_TEST(y, "y")
   DEFINE_NAMEOF_TYPE_TEST(z, "z")

   DEFINE_NAMEOF_TYPE_TEST(s_struct, "s_struct")
   DEFINE_NAMEOF_TYPE_TEST(t_struct, "t_struct")
   DEFINE_NAMEOF_TYPE_TEST(u_struct, "u_struct")
   DEFINE_NAMEOF_TYPE_TEST(v_struct, "v_struct")
   DEFINE_NAMEOF_TYPE_TEST(w_struct, "w_struct")
   DEFINE_NAMEOF_TYPE_TEST(x_struct, "x_struct")
   DEFINE_NAMEOF_TYPE_TEST(y_struct, "y_struct")
   DEFINE_NAMEOF_TYPE_TEST(z_struct, "z_struct")

   DEFINE_NAMEOF_TYPE_TEST(z, "z")

   #if LANGULUS_FEATURE(MANAGED_MEMORY)
      DEFINE_NAMEOF_TYPE_TEST(pptr8, "Langulus::Fractalloc::PackedPointer<char, 2, 6, 0>")
      DEFINE_NAMEOF_TYPE_TEST(pptr16, "Langulus::Fractalloc::PackedPointer<char, 4, 4, 8>")
      DEFINE_NAMEOF_TYPE_TEST(pptr32, "Langulus::Fractalloc::PackedPointer<char>")

      DEFINE_NAMEOF_TYPE_TEST(pptr8*, "Langulus::Fractalloc::PackedPointer<char, 2, 6, 0>*")
      DEFINE_NAMEOF_TYPE_TEST(pptr16*, "Langulus::Fractalloc::PackedPointer<char, 4, 4, 8>*")
      DEFINE_NAMEOF_TYPE_TEST(pptr32*, "Langulus::Fractalloc::PackedPointer<char>*")
   #endif

   WHEN("Taken the name of type Nаsty (with cyrillic 'a')") {
      //static_assert(NameOf<Nаsty>()); // shouldn't compile at all
      REQUIRE_THROWS(NameOfRt<Nаsty>());
   }

   DEFINE_NAMEOF_TYPE_TEST(IncompleteType, "IncompleteType")
   DEFINE_NAMEOF_TYPE_TEST(IncompleteType*, "IncompleteType*")
   DEFINE_NAMEOF_TYPE_TEST(IncompleteType**, "IncompleteType**")
   DEFINE_NAMEOF_TYPE_TEST(const IncompleteType*, "IncompleteType const*")
   DEFINE_NAMEOF_TYPE_TEST(const IncompleteType**, "IncompleteType const**")
   DEFINE_NAMEOF_TYPE_TEST(const IncompleteType* const*, "IncompleteType const* const*")
   DEFINE_NAMEOF_TYPE_TEST(const IncompleteType, "IncompleteType const")

   DEFINE_NAMEOF_TYPE_TEST(
       One::Two::Three::TypeDeepIntoNamespaces, 
      "One::Two::Three::TypeDeepIntoNamespaces"
   )

   DEFINE_NAMEOF_TYPE_TEST(
       One::Two::Three::TemplatedTypeDeepIntoNamespaces<char>,
      "One::Two::Three::TemplatedTypeDeepIntoNamespaces<char>"
   )

   DEFINE_NAMEOF_TYPE_TEST(
       One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16_t>, 
      "One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16>"
   )

   DEFINE_NAMEOF_TYPE_TEST(TypeDeepAlias, "One::Two::Three::TypeDeepIntoNamespaces")
   DEFINE_NAMEOF_TYPE_TEST(TypeDeepAlias*, "One::Two::Three::TypeDeepIntoNamespaces*")
   DEFINE_NAMEOF_TYPE_TEST(const TypeDeepAlias, "One::Two::Three::TypeDeepIntoNamespaces const")
   DEFINE_NAMEOF_TYPE_TEST(const TypeDeepAlias*, "One::Two::Three::TypeDeepIntoNamespaces const*")
   DEFINE_NAMEOF_TYPE_TEST(
       One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16_t>::Nested<uint16_t>,
      "One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16>::Nested<uint16>"
   )

   DEFINE_NAMEOF_TYPE_TEST(TemplatedAlias, "One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16>")
   DEFINE_NAMEOF_TYPE_TEST(VeryComplexTemplatedAlias, "One::Two::Three::VeryComplexTemplate<One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16>>")

   DEFINE_NAMEOF_TYPE_TEST(
       One::Two::Three::VeryComplexTemplate<TemplatedAlias>,
      "One::Two::Three::VeryComplexTemplate<One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16>>"
   )

   DEFINE_NAMEOF_TYPE_TEST(Langulus::Flow::Construct, "Langulus::Flow::Construct")
   DEFINE_NAMEOF_TYPE_TEST(Langulus::Flow::Constructconst, "Langulus::Flow::Constructconst")
   DEFINE_NAMEOF_TYPE_TEST(Langulus::Flow::constConstructconst, "Langulus::Flow::constConstructconst")
   DEFINE_NAMEOF_TYPE_TEST(Langulus::Flow::constconst, "Langulus::Flow::constconst")
   DEFINE_NAMEOF_TYPE_TEST(Signature, "<void(void*)>*")

   DEFINE_NAMEOF_CONST_TEST(EnumInsideAnonimousNamespace, "<unnamed namespace>::EnumInsideAnonimousNamespace")




   DEFINE_NAMEOF_CONST_TEST(Pi::Number, "Pi::Number")
   DEFINE_NAMEOF_CONST_TEST(PiNonClass::Number, "Number")
   DEFINE_NAMEOF_CONST_TEST(AnonymousNumber, "AnonymousNumber")

   DEFINE_NAMEOF_CONST_TEST(
       One::Two::Three::TemplatedTypeDeepIntoNamespaces<char>::VeryDeeplyTemplatedEnum::YesYouGotThatRight,
      "One::Two::Three::TemplatedTypeDeepIntoNamespaces<char>::YesYouGotThatRight"
   )

   DEFINE_NAMEOF_CONST_TEST(
       One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16_t>::VeryDeeplyTemplatedEnum::YesYouGotThatRight,
      "One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16>::YesYouGotThatRight"
   )

   DEFINE_NAMEOF_TYPE_TEST(NamedUsingMember, "NameOverrideUsingMember")
   DEFINE_NAMEOF_TYPE_TEST(NamedUsingMember*, "NameOverrideUsingMember*")
   DEFINE_NAMEOF_TYPE_TEST(const NamedUsingMember*, "NameOverrideUsingMember const*")

   DEFINE_NAMEOF_TYPE_TEST(NamedBySpecialization, "NameOverridedBySpecializing_CTTI_Named")
   DEFINE_NAMEOF_TYPE_TEST(NamedBySpecialization*, "NameOverridedBySpecializing_CTTI_Named_Ptr")
   DEFINE_NAMEOF_TYPE_TEST(const NamedBySpecialization*, "NameOverridedBySpecializing_CTTI_Named_ConstPtr")
   DEFINE_NAMEOF_TYPE_TEST(const NamedBySpecialization**, "NameOverridedBySpecializing_CTTI_Named_ConstPtr*")

   DEFINE_NAMEOF_CONST_TEST(PiButNamed::Number, "PiButNamedByCTTI")
   DEFINE_NAMEOF_CONST_TEST(PiNonClassButNamed::NumberButNamed, "NumberButNamedByCTTI")
   DEFINE_NAMEOF_CONST_TEST(AnonymousNumberButNamed, "AnonymousNumberButNamedByCTTI")
   DEFINE_NAMEOF_CONST_TEST(AnonymousNumberButNotNamed, "AnonymousNumberButNotNamed")

   DEFINE_NAMEOF_CONST_TEST(ConstantInsideAnonimousNamespace, "<unnamed namespace>::ConstantInsideAnonimousNamespace")
}

#define DEFINE_CPPNAMEOF_TYPE_TEST(WHAT, RESULT) \
   WHEN("Taken the raw name of type " #WHAT) { \
      REQUIRE(CppNameOfRt<WHAT>() == RESULT); \
      static_assert(CppNameOf<WHAT>() == RESULT); \
   }

#define DEFINE_CPPNAMEOF_CONST_TEST(WHAT, RESULT) \
   WHEN("Taken the raw name of constant " #WHAT) { \
      REQUIRE(CppNameOfRt<WHAT>() == RESULT); \
      static_assert(CppNameOf<WHAT>() == RESULT); \
   }


/// These names may differ for each compiler, so it's pretty hard to write a  
/// comprehensive test for each compiler, compiler version, etc. combinations.
/// That's why some of the tests are commented out.                           
SCENARIO("CppNameOf") {
   DEFINE_CPPNAMEOF_TYPE_TEST(void, "void")
   DEFINE_CPPNAMEOF_TYPE_TEST(nullptr_t, "std::nullptr_t")
   /*DEFINE_CPPNAMEOF_TYPE_TEST(int32_t(&)[5], "int32(&)[5]")
   DEFINE_CPPNAMEOF_TYPE_TEST(int32_t[5], "int32[5]")
   
   DEFINE_CPPNAMEOF_TYPE_TEST(int, "int32")
   DEFINE_CPPNAMEOF_TYPE_TEST(int&, "int32&")
   DEFINE_CPPNAMEOF_TYPE_TEST(const int, "int32 const")
   DEFINE_CPPNAMEOF_TYPE_TEST(const int*, "int32 const*")
   DEFINE_CPPNAMEOF_TYPE_TEST(const int**, "int32 const**")
   DEFINE_CPPNAMEOF_TYPE_TEST(const int* const*, "int32 const* const*")
   DEFINE_CPPNAMEOF_TYPE_TEST(const int* const* const, "int32 const* const* const")
   
   DEFINE_CPPNAMEOF_TYPE_TEST(uint16_t, "uint16")
   DEFINE_CPPNAMEOF_TYPE_TEST(uint16_t&, "uint16&")
   DEFINE_CPPNAMEOF_TYPE_TEST(const uint16_t, "uint16 const")
   DEFINE_CPPNAMEOF_TYPE_TEST(const uint16_t*, "uint16 const*")
   DEFINE_CPPNAMEOF_TYPE_TEST(const uint16_t**, "uint16 const**")
   DEFINE_CPPNAMEOF_TYPE_TEST(const uint16_t* const*, "uint16 const* const*")
   DEFINE_CPPNAMEOF_TYPE_TEST(const uint16_t* const* const, "uint16 const* const* const")
   DEFINE_CPPNAMEOF_TYPE_TEST(uint16_t*, "uint16*")*/

   DEFINE_CPPNAMEOF_TYPE_TEST(A, "A")
   DEFINE_CPPNAMEOF_TYPE_TEST(B, "B")
   DEFINE_CPPNAMEOF_TYPE_TEST(C, "C")
   DEFINE_CPPNAMEOF_TYPE_TEST(D, "D")
   DEFINE_CPPNAMEOF_TYPE_TEST(E, "E")
   DEFINE_CPPNAMEOF_TYPE_TEST(F, "F")
   DEFINE_CPPNAMEOF_TYPE_TEST(G, "G")
   DEFINE_CPPNAMEOF_TYPE_TEST(H, "H")
   DEFINE_CPPNAMEOF_TYPE_TEST(I, "I")
   DEFINE_CPPNAMEOF_TYPE_TEST(J, "J")
   DEFINE_CPPNAMEOF_TYPE_TEST(K, "K")
   DEFINE_CPPNAMEOF_TYPE_TEST(L, "L")
   DEFINE_CPPNAMEOF_TYPE_TEST(M, "M")
   DEFINE_CPPNAMEOF_TYPE_TEST(N, "N")
   DEFINE_CPPNAMEOF_TYPE_TEST(O, "O")
   DEFINE_CPPNAMEOF_TYPE_TEST(P, "P")
   DEFINE_CPPNAMEOF_TYPE_TEST(Q, "Q")
   DEFINE_CPPNAMEOF_TYPE_TEST(R, "R")
   DEFINE_CPPNAMEOF_TYPE_TEST(S, "S")
   DEFINE_CPPNAMEOF_TYPE_TEST(T, "T")
   DEFINE_CPPNAMEOF_TYPE_TEST(U, "U")
   DEFINE_CPPNAMEOF_TYPE_TEST(V, "V")
   DEFINE_CPPNAMEOF_TYPE_TEST(W, "W")
   DEFINE_CPPNAMEOF_TYPE_TEST(X, "X")
   DEFINE_CPPNAMEOF_TYPE_TEST(Y, "Y")
   DEFINE_CPPNAMEOF_TYPE_TEST(Z, "Z")
   DEFINE_CPPNAMEOF_TYPE_TEST(_, "_")

   DEFINE_CPPNAMEOF_TYPE_TEST(a, "a")
   DEFINE_CPPNAMEOF_TYPE_TEST(b, "b")
   DEFINE_CPPNAMEOF_TYPE_TEST(c, "c")
   DEFINE_CPPNAMEOF_TYPE_TEST(d, "d")
   DEFINE_CPPNAMEOF_TYPE_TEST(e, "e")
   DEFINE_CPPNAMEOF_TYPE_TEST(f, "f")
   DEFINE_CPPNAMEOF_TYPE_TEST(g, "g")
   DEFINE_CPPNAMEOF_TYPE_TEST(h, "h")
   DEFINE_CPPNAMEOF_TYPE_TEST(i, "i")
   DEFINE_CPPNAMEOF_TYPE_TEST(j, "j")
   DEFINE_CPPNAMEOF_TYPE_TEST(k, "k")
   DEFINE_CPPNAMEOF_TYPE_TEST(l, "l")
   DEFINE_CPPNAMEOF_TYPE_TEST(m, "m")
   DEFINE_CPPNAMEOF_TYPE_TEST(n, "n")
   DEFINE_CPPNAMEOF_TYPE_TEST(o, "o")
   DEFINE_CPPNAMEOF_TYPE_TEST(p, "p")
   DEFINE_CPPNAMEOF_TYPE_TEST(q, "q")
   DEFINE_CPPNAMEOF_TYPE_TEST(r, "r")
   DEFINE_CPPNAMEOF_TYPE_TEST(s, "s")
   DEFINE_CPPNAMEOF_TYPE_TEST(t, "t")
   DEFINE_CPPNAMEOF_TYPE_TEST(u, "u")
   DEFINE_CPPNAMEOF_TYPE_TEST(v, "v")
   DEFINE_CPPNAMEOF_TYPE_TEST(w, "w")
   DEFINE_CPPNAMEOF_TYPE_TEST(x, "x")
   DEFINE_CPPNAMEOF_TYPE_TEST(y, "y")
   DEFINE_CPPNAMEOF_TYPE_TEST(z, "z")

   DEFINE_CPPNAMEOF_TYPE_TEST(s_struct, "s_struct")
   DEFINE_CPPNAMEOF_TYPE_TEST(t_struct, "t_struct")
   DEFINE_CPPNAMEOF_TYPE_TEST(u_struct, "u_struct")
   DEFINE_CPPNAMEOF_TYPE_TEST(v_struct, "v_struct")
   DEFINE_CPPNAMEOF_TYPE_TEST(w_struct, "w_struct")
   DEFINE_CPPNAMEOF_TYPE_TEST(x_struct, "x_struct")
   DEFINE_CPPNAMEOF_TYPE_TEST(y_struct, "y_struct")
   DEFINE_CPPNAMEOF_TYPE_TEST(z_struct, "z_struct")

   DEFINE_CPPNAMEOF_TYPE_TEST(z, "z")

   WHEN("Taken the name of type Nаsty (with cyrillic 'a')") {
      //REQUIRE_STATIC(NameOf<Nаsty>()); // shouldn't compile at all
      REQUIRE_THROWS(NameOfRt<Nаsty>());
   }

   DEFINE_CPPNAMEOF_TYPE_TEST(IncompleteType, "IncompleteType")
   DEFINE_CPPNAMEOF_TYPE_TEST(IncompleteType*, "IncompleteType*")
   DEFINE_CPPNAMEOF_TYPE_TEST(IncompleteType**, "IncompleteType**")
   DEFINE_CPPNAMEOF_TYPE_TEST(const IncompleteType*, "IncompleteType const*")
   DEFINE_CPPNAMEOF_TYPE_TEST(const IncompleteType**, "IncompleteType const**")
   DEFINE_CPPNAMEOF_TYPE_TEST(const IncompleteType* const*, "IncompleteType const* const*")
   DEFINE_CPPNAMEOF_TYPE_TEST(const IncompleteType, "IncompleteType const")

   DEFINE_CPPNAMEOF_TYPE_TEST(
       One::Two::Three::TypeDeepIntoNamespaces, 
      "One::Two::Three::TypeDeepIntoNamespaces"
   )

   DEFINE_CPPNAMEOF_TYPE_TEST(
       One::Two::Three::TemplatedTypeDeepIntoNamespaces<char>,
      "One::Two::Three::TemplatedTypeDeepIntoNamespaces<char>"
   )

   /*DEFINE_CPPNAMEOF_TYPE_TEST(
       One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16_t>, 
      "One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16>"
   )*/

   DEFINE_CPPNAMEOF_TYPE_TEST(TypeDeepAlias, "One::Two::Three::TypeDeepIntoNamespaces")
   DEFINE_CPPNAMEOF_TYPE_TEST(TypeDeepAlias*, "One::Two::Three::TypeDeepIntoNamespaces*")
   DEFINE_CPPNAMEOF_TYPE_TEST(const TypeDeepAlias, "One::Two::Three::TypeDeepIntoNamespaces const")
   DEFINE_CPPNAMEOF_TYPE_TEST(const TypeDeepAlias*, "One::Two::Three::TypeDeepIntoNamespaces const*")
   /*DEFINE_CPPNAMEOF_TYPE_TEST(
       One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16_t>::Nested<uint16_t>,
      "One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16>::Nested<uint16>"
   )*/

   /*DEFINE_CPPNAMEOF_TYPE_TEST(TemplatedAlias, "One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16>")
   DEFINE_CPPNAMEOF_TYPE_TEST(VeryComplexTemplatedAlias, "One::Two::Three::VeryComplexTemplate<One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16>>")

   DEFINE_CPPNAMEOF_TYPE_TEST(
       One::Two::Three::VeryComplexTemplate<TemplatedAlias>,
      "One::Two::Three::VeryComplexTemplate<One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16>>"
   )*/

   DEFINE_CPPNAMEOF_TYPE_TEST(Langulus::Flow::Construct, "Langulus::Flow::Construct")
   DEFINE_CPPNAMEOF_TYPE_TEST(Langulus::Flow::Constructconst, "Langulus::Flow::Constructconst")
   DEFINE_CPPNAMEOF_TYPE_TEST(Langulus::Flow::constConstructconst, "Langulus::Flow::constConstructconst")
   DEFINE_CPPNAMEOF_TYPE_TEST(Langulus::Flow::constconst, "Langulus::Flow::constconst")
   //DEFINE_CPPNAMEOF_TYPE_TEST(Signature, "<void(void*)>*")

   //DEFINE_CPPNAMEOF_TYPE_TEST(EnumInsideAnonimousNamespace, "<unnamed namespace>::EnumInsideAnonimousNamespace")




   DEFINE_CPPNAMEOF_CONST_TEST(Pi::Number, "Pi::Number")
   DEFINE_CPPNAMEOF_CONST_TEST(PiNonClass::Number, "Number")
   DEFINE_CPPNAMEOF_CONST_TEST(AnonymousNumber, "AnonymousNumber")

   DEFINE_CPPNAMEOF_CONST_TEST(
       One::Two::Three::TemplatedTypeDeepIntoNamespaces<char>::VeryDeeplyTemplatedEnum::YesYouGotThatRight,
      "One::Two::Three::TemplatedTypeDeepIntoNamespaces<char>::YesYouGotThatRight"
   )

   /*DEFINE_CPPNAMEOF_CONST_TEST(
       One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16_t>::VeryDeeplyTemplatedEnum::YesYouGotThatRight,
      "One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16>::YesYouGotThatRight"
   )*/

   DEFINE_CPPNAMEOF_TYPE_TEST(NamedUsingMember, "NamedUsingMember")
   DEFINE_CPPNAMEOF_TYPE_TEST(NamedUsingMember*, "NamedUsingMember*")
   DEFINE_CPPNAMEOF_TYPE_TEST(const NamedUsingMember*, "NamedUsingMember const*")

   DEFINE_CPPNAMEOF_TYPE_TEST(NamedBySpecialization, "NamedBySpecialization")
   DEFINE_CPPNAMEOF_TYPE_TEST(NamedBySpecialization*, "NamedBySpecialization*")
   DEFINE_CPPNAMEOF_TYPE_TEST(const NamedBySpecialization*, "NamedBySpecialization const*")
   DEFINE_CPPNAMEOF_TYPE_TEST(const NamedBySpecialization**, "NamedBySpecialization const**")

   DEFINE_CPPNAMEOF_CONST_TEST(PiButNamed::Number, "PiButNamed::Number")
   DEFINE_CPPNAMEOF_CONST_TEST(PiNonClassButNamed::NumberButNamed, "NumberButNamed")
   DEFINE_CPPNAMEOF_CONST_TEST(AnonymousNumberButNamed, "AnonymousNumberButNamed")
   DEFINE_CPPNAMEOF_CONST_TEST(AnonymousNumberButNotNamed, "AnonymousNumberButNotNamed")

   //DEFINE_CPPNAMEOF_CONST_TEST(ConstantInsideAnonimousNamespace, "<unnamed namespace>::ConstantInsideAnonimousNamespace")
}
