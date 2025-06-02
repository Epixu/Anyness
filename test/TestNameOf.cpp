///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#include "Main.hpp"
#include <Langulus/NameOf.hpp>
#include <Langulus/Logger.hpp>
#include <string>

using namespace Langulus;

namespace One::Two::Three
{
   struct TypeDeepIntoNamespaces;

   template<class T>
   struct TemplatedTypeDeepIntoNamespaces {
      enum VeryDeeplyTemplatedEnum { YesYouGotThatRight };

      template<class MORE>
      struct Nested;
   };

   template<class T>
   struct VeryComplexTemplate;
}

namespace Langulus::Flow
{
   struct Construct;
   struct Constructconst;
   struct constConstructconst;
   struct constconst;
}

namespace
{
   struct IncompleteType;

   using TypeDeepAlias =
      One::Two::Three::TypeDeepIntoNamespaces;
   using TemplatedAlias =
      One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16_t>;
   using VeryComplexTemplatedAlias =
      One::Two::Three::VeryComplexTemplate<
      One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16_t>>;

   class A; class B; class C; class D; class E; class F; class G; class H; class I;
   class J; class K; class L; class M; class N; class O; class P; class Q; class R;
   class S; class T; class U; class V; class W; class X; class Y; class Z; class _;

   class a; class b; class c; class d; class e; class f; class g; class h; class i;
   class j; class k; class l; class m; class n; class o; class p; class q; class r;
   class s; class t; class u; class v; class w; class x; class y; class z;

   struct TypeWithSuffix { using CTTI_Suffix = YesText<"yeah">; };
   struct TypeWithoutSuffix {};

   using Signature = void(*)(void*);
   
   template<class T>
   ::std::string IsolateTypenameAtRuntime();

   ::std::string NormalizeTypenameAtRuntime(const ::std::string& SRC) {
      // Replace these patterns when normalizing names               
      // @attention when having similar tokens to replace, order     
      //    them correctly, with longer ones replaced first          
      // @attention replacement will not commence, if IsTransition   
      //    isn't satisifed                                          
      ::std::string a01 = Replace(SRC, Literal {"*const "     },    Literal {"* const"});
      ::std::string a02 = Replace(a01, Literal {" *const"     },    Literal {"* const"});
      ::std::string a03 = Replace(a02, Literal {" *"          },    Literal {"*"      });
      ::std::string a04 = Replace(a03, Literal {" &"          },    Literal {"&"      });
      ::std::string a05 = Replace(a04, Literal {" >"          },    Literal {">"      });
      ::std::string a06 = Replace(a05, IsolateTypenameAtRuntime<int8_t>(),   Literal {"int8"   });
      ::std::string a07 = Replace(a06, IsolateTypenameAtRuntime<int16_t>(),  Literal {"int16"  });
      ::std::string a08 = Replace(a07, IsolateTypenameAtRuntime<int32_t>(),  Literal {"int32"  });
      ::std::string a09 = Replace(a08, IsolateTypenameAtRuntime<int64_t>(),  Literal {"int64"  });
      ::std::string a10 = Replace(a09, IsolateTypenameAtRuntime<uint8_t>(),  Literal {"uint8"  });
      ::std::string a11 = Replace(a10, IsolateTypenameAtRuntime<uint16_t>(), Literal {"uint16" });
      ::std::string a12 = Replace(a11, IsolateTypenameAtRuntime<uint32_t>(), Literal {"uint32" });
      ::std::string a13 = Replace(a12, IsolateTypenameAtRuntime<uint64_t>(), Literal {"uint64" });
      ::std::string a14 = Replace(a13, Literal {"class "      },    Literal {""       });
      ::std::string a15 = Replace(a14, Literal {"struct "     },    Literal {""       });
      ::std::string a16 = Replace(a15, Literal {"enum "       },    Literal {""       });
      ::std::string a17 = Replace(a16, Literal {"Langulus::"  },    Literal {""       });
      ::std::string a18 = Replace(a17, Literal {"(__cdecl *)" },    Literal {""       });
      ::std::string a19 = Replace(a18, Literal {" (*)"        },    Literal {""       });
      return a19;
   }

   template<class T>
   ::std::string IsolateTypenameAtRuntime() {
      ::std::string name = static_cast<::std::string>(RTTI::Inner::WrappedTypeName<T>());
      size_t size = name.size();
      size_t left = RTTI::Inner::CalibratedTypeLeftOffset;
      size_t right = RTTI::Inner::CalibratedTypeRightOffset;
      REQUIRE(size > left + right);

      ::std::string isolated = name.substr(left, size - right - left);
      for (char c : isolated) {
         if (RTTI::Inner::IsAlphabetical(c)
         or RTTI::Inner::IsOperator(c)
         or RTTI::Inner::IsNumerical(c)
         or RTTI::Inner::IsSpace(c))
            continue;

         Logger::ErrorRaw("Disallowed symbol: ", c);
         FAIL();
      }

      Logger::InfoRaw("IsolateTypenameAtRuntime: ", name, " -> ", isolated);
      ::std::string normalized = NormalizeTypenameAtRuntime(isolated);
      Logger::InfoRaw("NormalizeTypenameAtRuntime: ", isolated, " -> ", normalized);
      return normalized;
   }

}

#define DEFINE_NAMEOF_TEST(WHAT, RESULT) \
   WHEN("Taken the name of class " #WHAT) { \
      auto name_runtime = IsolateTypenameAtRuntime<WHAT>(); \
      REQUIRE(name_runtime == RESULT); \
   }

/*#define DEFINE_NAMEOF_TEST(WHAT, RESULT) \
   WHEN("Taken the name of class " #WHAT) { \
      auto name = NameOf<::WHAT>(); \
      auto name_runtime = IsolateTypenameAtRuntime<::WHAT>(); \
      REQUIRE(name == RESULT); \
      REQUIRE(name_runtime == RESULT); \
      STATIC_REQUIRE(NameOf<::WHAT>() == RESULT); \
   }*/

SCENARIO("NameOf", "[nameof]") {
   DEFINE_NAMEOF_TEST(uint16_t, "uint16")
   DEFINE_NAMEOF_TEST(uint16_t&, "uint16&")
   DEFINE_NAMEOF_TEST(const uint16_t, "const uint16")
   DEFINE_NAMEOF_TEST(const uint16_t*, "uint16 const*")
   DEFINE_NAMEOF_TEST(const uint16_t**, "uint16 const**")
   DEFINE_NAMEOF_TEST(const uint16_t* const*, "uint16 const* const*")
   DEFINE_NAMEOF_TEST(const uint16_t* const* const, "uint16 const* const* const")
   DEFINE_NAMEOF_TEST(uint16_t*, "uint16*")
   //DEFINE_NAMEOF_TEST(Pi::Number, "Pi::Number")

   DEFINE_NAMEOF_TEST(A, "A")
   DEFINE_NAMEOF_TEST(B, "B")
   DEFINE_NAMEOF_TEST(C, "C")
   DEFINE_NAMEOF_TEST(D, "D")
   DEFINE_NAMEOF_TEST(E, "E")
   DEFINE_NAMEOF_TEST(F, "F")
   DEFINE_NAMEOF_TEST(G, "G")
   DEFINE_NAMEOF_TEST(H, "H")
   DEFINE_NAMEOF_TEST(I, "I")
   DEFINE_NAMEOF_TEST(J, "J")
   DEFINE_NAMEOF_TEST(K, "K")
   DEFINE_NAMEOF_TEST(L, "L")
   DEFINE_NAMEOF_TEST(M, "M")
   DEFINE_NAMEOF_TEST(N, "N")
   DEFINE_NAMEOF_TEST(O, "O")
   DEFINE_NAMEOF_TEST(P, "P")
   DEFINE_NAMEOF_TEST(Q, "Q")
   DEFINE_NAMEOF_TEST(R, "R")
   DEFINE_NAMEOF_TEST(S, "S")
   DEFINE_NAMEOF_TEST(T, "T")
   DEFINE_NAMEOF_TEST(U, "U")
   DEFINE_NAMEOF_TEST(V, "V")
   DEFINE_NAMEOF_TEST(W, "W")
   DEFINE_NAMEOF_TEST(X, "X")
   DEFINE_NAMEOF_TEST(Y, "Y")
   DEFINE_NAMEOF_TEST(Z, "Z")
   DEFINE_NAMEOF_TEST(_, "_")

   DEFINE_NAMEOF_TEST(a, "a")
   DEFINE_NAMEOF_TEST(b, "b")
   DEFINE_NAMEOF_TEST(c, "c")
   DEFINE_NAMEOF_TEST(d, "d")
   DEFINE_NAMEOF_TEST(e, "e")
   DEFINE_NAMEOF_TEST(f, "f")
   DEFINE_NAMEOF_TEST(g, "g")
   DEFINE_NAMEOF_TEST(h, "h")
   DEFINE_NAMEOF_TEST(i, "i")
   DEFINE_NAMEOF_TEST(j, "j")
   DEFINE_NAMEOF_TEST(k, "k")
   DEFINE_NAMEOF_TEST(l, "l")
   DEFINE_NAMEOF_TEST(m, "m")
   DEFINE_NAMEOF_TEST(n, "n")
   DEFINE_NAMEOF_TEST(o, "o")
   DEFINE_NAMEOF_TEST(p, "p")
   DEFINE_NAMEOF_TEST(q, "q")
   DEFINE_NAMEOF_TEST(r, "r")
   DEFINE_NAMEOF_TEST(s, "s")
   DEFINE_NAMEOF_TEST(t, "t")
   DEFINE_NAMEOF_TEST(u, "u")
   DEFINE_NAMEOF_TEST(v, "v")
   DEFINE_NAMEOF_TEST(w, "w")
   DEFINE_NAMEOF_TEST(x, "x")
   DEFINE_NAMEOF_TEST(y, "y")
   DEFINE_NAMEOF_TEST(z, "z")

   DEFINE_NAMEOF_TEST(IncompleteType, "IncompleteType")
   DEFINE_NAMEOF_TEST(IncompleteType*, "IncompleteType*")
   DEFINE_NAMEOF_TEST(const IncompleteType*, "IncompleteType const*")
   DEFINE_NAMEOF_TEST(const IncompleteType, "IncompleteType const")

   DEFINE_NAMEOF_TEST(One::Two::Three::TypeDeepIntoNamespaces, "One::Two::Three::TypeDeepIntoNamespaces")
   DEFINE_NAMEOF_TEST(One::Two::Three::TemplatedTypeDeepIntoNamespaces<char>, "One::Two::Three::TemplatedTypeDeepIntoNamespaces<char>")
   DEFINE_NAMEOF_TEST(One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16_t>, "One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16>")

   //DEFINE_NAMEOF_TEST(One::Two::Three::TemplatedTypeDeepIntoNamespaces<char>::VeryDeeplyTemplatedEnum::YesYouGotThatRight, "One::Two::Three::TemplatedTypeDeepIntoNamespaces<char>::VeryDeeplyTemplatedEnum::YesYouGotThatRight")
   //DEFINE_NAMEOF_TEST(One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16_t>::VeryDeeplyTemplatedEnum::YesYouGotThatRight, "One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16>::VeryDeeplyTemplatedEnum::YesYouGotThatRight")

   DEFINE_NAMEOF_TEST(TypeDeepAlias, "One::Two::Three::TypeDeepIntoNamespaces")
   DEFINE_NAMEOF_TEST(TypeDeepAlias*, "One::Two::Three::TypeDeepIntoNamespaces*")
   DEFINE_NAMEOF_TEST(const TypeDeepAlias, "One::Two::Three::TypeDeepIntoNamespaces const")
   DEFINE_NAMEOF_TEST(const TypeDeepAlias*, "One::Two::Three::TypeDeepIntoNamespaces const*")
   DEFINE_NAMEOF_TEST(One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16_t>::Nested<uint16_t>, "One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16>::Nested<uint16>")

   DEFINE_NAMEOF_TEST(TemplatedAlias, "One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16>")
   DEFINE_NAMEOF_TEST(VeryComplexTemplatedAlias, "One::Two::Three::VeryComplexTemplate<One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16>>")
   DEFINE_NAMEOF_TEST(One::Two::Three::VeryComplexTemplate<TemplatedAlias>, "One::Two::Three::VeryComplexTemplate<One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16>>")

   DEFINE_NAMEOF_TEST(Langulus::Flow::Construct, "Flow::Construct")
   DEFINE_NAMEOF_TEST(Langulus::Flow::Constructconst, "Flow::Constructconst")
   DEFINE_NAMEOF_TEST(Langulus::Flow::constConstructconst, "Flow::constConstructconst")
   DEFINE_NAMEOF_TEST(Langulus::Flow::constconst, "Flow::constconst")
   DEFINE_NAMEOF_TEST(Signature, "Function<void(void*)>*")
}


/*SCENARIO("SuffixOf", "[metadata]") {
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
}*/