///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "Main.hpp"
#include <Langulus/NameOf.hpp>

#include <string>
#if 0
   #include <Langulus/Logger.hpp>
   #define VERBOSE(...) Logger::Verbose(__VA_ARGS__)
#else
   #define VERBOSE(...)
#endif

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
   struct NamedUsingMember {
      using CTTI_Named = Yes<"NameOverrideUsingMember">;
   };
   struct NamedBySpecialization {};

   struct Nаsty {
      int this_type_name_contains_a_cyrillic_letter;
   };

   enum class Pi {
      Number = 314
   };

   enum PiNonClass {
      Number = 314
   };

   enum {
      AnonymousNumber = 314
   };

   enum class PiButNamed {
      Number = 314
   };

   enum PiNonClassButNamed {
      NumberButNamed = 314
   };

   enum {
      AnonymousNumberButNamed = 314,
      AnonymousNumberButNotNamed = 315
   };

   struct IncompleteType;

   using TypeDeepAlias             = One::Two::Three::TypeDeepIntoNamespaces;
   using TemplatedAlias            = One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16_t>;
   using VeryComplexTemplatedAlias = One::Two::Three::VeryComplexTemplate<
      One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16_t>
   >;

   class A; class B; class C; class D; class E; class F; class G; class H; class I;
   class J; class K; class L; class M; class N; class O; class P; class Q; class R;
   class S; class T; class U; class V; class W; class X; class Y; class Z; class _;

   class a; class b; class c; class d; class e; class f; class g; class h; class i;
   class j; class k; class l; class m; class n; class o; class p; class q; class r;
   class s; class t; class u; class v; class w; class x; class y; class z;

   struct s_struct; struct t_struct; struct u_struct; struct v_struct; struct w_struct; struct x_struct; struct y_struct; struct z_struct;

   using Signature = void(*)(void*);
   
   template<class,bool,bool>
   ::std::string IsolateTypenameAtRuntime();

   std::size_t CountOccurencesAtRuntime(const ::std::string& LHS, const ::std::string& RHS) {
      if (RHS.size() > LHS.size() or RHS.size() == 0)
         return 0;

      ::std::size_t occurences = 0;
      ::std::size_t cookie = 0;
      while (cookie + RHS.size() <= LHS.size()) {
         ::std::size_t scan = 0;
         while (scan < RHS.size()) {
            if (LHS[cookie + scan] == RHS[scan]) {
               ++scan;
               continue;
            }

            break;
         }

         if (scan == RHS.size() and IsTransition(LHS, cookie, cookie + RHS.size())) {
            cookie += RHS.size();
            ++occurences;
         }
         else ++cookie;
      }
      return occurences;
   }

   ::std::string ReplaceAtRuntime(const ::std::string& SOURCE, const ::std::string& WHAT, const ::std::string& WITH) {
      auto found = CountOccurencesAtRuntime(SOURCE, WHAT);
      if (not found)
         return SOURCE;

      ::std::string result;
      std::size_t prev = 0;
      std::size_t curr = 0;
      while ((curr = SOURCE.find(WHAT, prev)) != SOURCE.npos) {
         while (curr > prev)
            result += SOURCE[prev++];

         if (IsTransition(SOURCE, curr, curr + WHAT.size())) {
            for (auto& c : WITH)
               result += c;
            prev += WHAT.size();
         }
      }

      while (prev < SOURCE.size())
         result += SOURCE[prev++];

      return result;
   }

   ::std::string NormalizeTypenameAtRuntime(const ::std::string& SRC) {
      #if LANGULUS_COMPILER(MSVC)
         ::std::string a00 = ReplaceAtRuntime(SRC, "`anonymous-namespace'::", "");
      #elif LANGULUS_COMPILER(CLANG)
         ::std::string a00 = ReplaceAtRuntime(SRC, "(anonymous namespace)::", "");
      #else
         ::std::string b00 = ReplaceAtRuntime(SRC, "<unnamed>::", "");
         ::std::string a00 = ReplaceAtRuntime(b00, "{anonymous}::", "");
      #endif

      ::std::string a01 = ReplaceAtRuntime(a00, " *", "*");
      ::std::string a02 = ReplaceAtRuntime(a01, " &", "&");
      ::std::string a03 = ReplaceAtRuntime(a02, " >", ">");
      ::std::string a04 = ReplaceAtRuntime(a03, " (", "(");
      ::std::string a05 = ReplaceAtRuntime(a04, " )", ")");
      ::std::string a06 = ReplaceAtRuntime(a05, " [", "[");
      ::std::string a07 = ReplaceAtRuntime(a06, " ]", "]");
      ::std::string a08 = ReplaceAtRuntime(a07, "class ", "");
      ::std::string a09 = ReplaceAtRuntime(a08, "struct ", "");
      ::std::string a10 = ReplaceAtRuntime(a09, "enum ", "");
      ::std::string a11 = ReplaceAtRuntime(a10, "(__cdecl *)", "");

      ::std::string a12 = ReplaceAtRuntime(a11, IsolateTypenameAtRuntime<uint8_t,  false, false>(), "uint8" );
      ::std::string a13 = ReplaceAtRuntime(a12, IsolateTypenameAtRuntime<uint16_t, false, false>(), "uint16");
      ::std::string a14 = ReplaceAtRuntime(a13, IsolateTypenameAtRuntime<uint32_t, false, false>(), "uint32");
      ::std::string a15 = ReplaceAtRuntime(a14, IsolateTypenameAtRuntime<uint64_t, false, false>(), "uint64");

      ::std::string a16 = ReplaceAtRuntime(a15, IsolateTypenameAtRuntime<int8_t,   false, false>(), "int8"  );
      ::std::string a17 = ReplaceAtRuntime(a16, IsolateTypenameAtRuntime<int16_t,  false, false>(), "int16" );
      ::std::string a18 = ReplaceAtRuntime(a17, IsolateTypenameAtRuntime<int32_t,  false, false>(), "int32" );
      ::std::string a19 = ReplaceAtRuntime(a18, IsolateTypenameAtRuntime<int64_t,  false, false>(), "int64" );

      for (char c : a19) {
         if (IsAlphabetical(c) or IsOperator(c) or IsNumerical(c) or IsSpace(c))
            continue;

         VERBOSE(Logger::Red, "Disallowed symbol: `", c, "` in `", a19, "`");
         throw "";
      }

      return a19;
   }

   template<class T, bool NORMALIZE, bool NAMED>
   ::std::string IsolateTypenameAtRuntime() {
      if constexpr (NAMED and CTTI::Named<T>::Enabled)
         return static_cast<::std::string>(CTTI::Named<T>::Name);
      // Move `const` next to pointers/references at the end of type 
      // Discards `volatile` - it shouldn't matter outside compiler  
      // Helps with better sorting reflected types                   
      else if constexpr (::std::is_const_v<T> or ::std::is_volatile_v<T>) {
         auto deptr = IsolateTypenameAtRuntime<Decvq<T>, NORMALIZE, NAMED>();
         if constexpr (not ::std::is_const_v<T>)
            return deptr;
         else
            return deptr + " const";
      }
      else if constexpr (::std::is_reference_v<T>) {
         auto deptr = IsolateTypenameAtRuntime<Decvq<Deref<T>>, NORMALIZE, NAMED>();
         if constexpr (not ::std::is_const_v<Deref<T>>)
            return deptr + "&";
         else
            return deptr + " const&";
      }
      else if constexpr (::std::is_bounded_array_v<T>) {
         auto deext = IsolateTypenameAtRuntime<Deext<T>, NORMALIZE, NAMED>();
         constexpr auto ext = ::std::extent_v<T>;
         static_assert(ext < 1000000, "Extent is too big");
         if constexpr (ext > 99999) {
            return deext + "[" + static_cast<char>('0' + ext/100000)
                               + static_cast<char>('0' + ext/10000)
                               + static_cast<char>('0' + ext/1000)
                               + static_cast<char>('0' + ext/100)
                               + static_cast<char>('0' + ext/10)
                               + static_cast<char>('0' + ext) + "]";
         }
         else if constexpr (ext > 9999) {
            return deext + "[" + static_cast<char>('0' + ext/10000)
                               + static_cast<char>('0' + ext/1000)
                               + static_cast<char>('0' + ext/100)
                               + static_cast<char>('0' + ext/10)
                               + static_cast<char>('0' + ext) + "]";
         }
         else if constexpr (ext > 999) {
            return deext + "[" + static_cast<char>('0' + ext/1000)
                               + static_cast<char>('0' + ext/100)
                               + static_cast<char>('0' + ext/10)
                               + static_cast<char>('0' + ext) + "]";               
         }
         else if constexpr (ext > 99) {
            return deext + "[" + static_cast<char>('0' + ext/100)
                               + static_cast<char>('0' + ext/10)
                               + static_cast<char>('0' + ext) + "]";               
         }
         else if constexpr (ext > 9) {
            return deext + "[" + static_cast<char>('0' + ext/10)
                               + static_cast<char>('0' + ext) + "]";
         }
         else return deext + "[" + static_cast<char>('0' + ext) + "]";
      }
      else if constexpr (::std::is_pointer_v<T>) {
         auto deptr = IsolateTypenameAtRuntime<Decvq<Deptr<T>>, NORMALIZE, NAMED>();
         if constexpr (not ::std::is_const_v<Deptr<T>>)
            return deptr + "*";
         else
            return deptr + " const*";
      }
      else if constexpr (NAMED and requires { T::CTTI_Named::Constant; })
         return static_cast<::std::string>(T::CTTI_Named::Constant);
      else {
         ::std::string name = static_cast<::std::string>(RTTI::Inner::WrappedTypeName<T>());
         size_t size = name.size();
         size_t left = RTTI::Inner::CalculateTypeLeftOffset();
         size_t right = RTTI::Inner::CalculateTypeRightOffset();
         REQUIRE(size > left + right);
         ::std::string isolated = name.substr(left, size - right - left);

         if constexpr (not NORMALIZE) {
            if constexpr (::std::is_function_v<T>)
               return "<" + isolated + ">";
            else
               return isolated;
         }
         else {
            VERBOSE("IsolateTypenameAtRuntime: ", name, " -> ", isolated);
            ::std::string normalized = NormalizeTypenameAtRuntime(isolated);
            VERBOSE("NormalizeTypenameAtRuntime: ", isolated, " -> ", normalized);
            if constexpr (::std::is_function_v<T>)
               return "<" + normalized + ">";
            else
               return normalized;
         }
      }
   }

   template<auto T, bool NORMALIZE, bool NAMED>
   ::std::string IsolateConstantAtRuntime() {
      if constexpr (NAMED and CT::NamedValue<T>)
         return static_cast<::std::string>(CTTI::NamedValue<T>::Name);
      else {
         ::std::string name = static_cast<::std::string>(RTTI::Inner::WrappedEnumName<T>());
         size_t size = name.size();
         size_t left = RTTI::Inner::CalculateEnumLeftOffset();
         size_t right = RTTI::Inner::CalculateEnumRightOffset();
         REQUIRE(size > left + right);
         ::std::string isolated = name.substr(left, size - right - left);

         VERBOSE("IsolateConstantAtRuntime: ", name, " -> ", isolated);

         if constexpr (NORMALIZE) {
            ::std::string normalized = NormalizeTypenameAtRuntime(isolated);
            VERBOSE("Normalized constant: ", isolated, " -> ", normalized);
            return normalized;
         }
         else return isolated;
      }
   }
}

namespace Langulus::CTTI
{

   template<>
   struct Named<NamedBySpecialization> {
      static constexpr Literal Name = "NameOverridedBySpecializing_CTTI_Named";
      static constexpr bool Enabled = true;
   };
   template<>
   struct Named<NamedBySpecialization*> {
      static constexpr Literal Name = "NameOverridedBySpecializing_CTTI_Named_Ptr";
      static constexpr bool Enabled = true;
   };
   template<>
   struct Named<NamedBySpecialization const*> {
      static constexpr Literal Name = "NameOverridedBySpecializing_CTTI_Named_ConstPtr";
      static constexpr bool Enabled = true;
   };
   template<>
   struct Named<NamedUsingMember> {
      //static constexpr Literal Name = "<should be disabled>";
      static constexpr bool Enabled = false;
   };

   template<>
   struct NamedValue<PiButNamed::Number> {
      static constexpr Literal Name = "PiButNamedByCTTI";
      static constexpr bool Enabled = true;
   };
   template<>
   struct NamedValue<NumberButNamed> {
      static constexpr Literal Name = "NumberButNamedByCTTI";
      static constexpr bool Enabled = true;
   };
   template<>
   struct NamedValue<AnonymousNumberButNamed> {
      static constexpr Literal Name = "AnonymousNumberButNamedByCTTI";
      static constexpr bool Enabled = true;
   };
   template<>
   struct NamedValue<AnonymousNumberButNotNamed> {
      //static constexpr Literal Name = "<not actually named>";
      static constexpr bool Enabled = false;
   };

} // namespace Langulus::CTTI


#define DEFINE_NAMEOF_TYPE_TEST(WHAT, RESULT) \
   WHEN("Taken the name of type " #WHAT) { \
      auto name_runtime = IsolateTypenameAtRuntime<WHAT, true, true>(); \
      REQUIRE(name_runtime == RESULT); \
      /*STATIC_REQUIRE(NameOf<WHAT>() == RESULT);*/ \
   }

#define DEFINE_NAMEOF_CONST_TEST(WHAT, RESULT) \
   WHEN("Taken the name of constant " #WHAT) { \
      auto name_runtime = IsolateConstantAtRuntime<WHAT, true, true>(); \
      REQUIRE(name_runtime == RESULT); \
      /*STATIC_REQUIRE(NameOf<WHAT>() == RESULT);*/ \
   }


SCENARIO("NameOf", "[nameof]") {
   DEFINE_NAMEOF_TYPE_TEST(void, "void")
   DEFINE_NAMEOF_TYPE_TEST(nullptr_t, "null")
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

   WHEN("Taken the name of type Nаsty (with cyrillic 'a')") {
      //REQUIRE_STATIC(NameOf<Nаsty>()); // shouldn't compile at all
      REQUIRE_THROWS(IsolateTypenameAtRuntime<Nаsty, true, true>());
   }

   DEFINE_NAMEOF_TYPE_TEST(IncompleteType, "IncompleteType")
   DEFINE_NAMEOF_TYPE_TEST(IncompleteType*, "IncompleteType*")
   DEFINE_NAMEOF_TYPE_TEST(const IncompleteType*, "IncompleteType const*")
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
}

#define DEFINE_CPPNAMEOF_TYPE_TEST(WHAT, RESULT) \
   WHEN("Taken the raw name of type " #WHAT) { \
      auto name_runtime = IsolateTypenameAtRuntime<WHAT, true, false>(); \
      REQUIRE(name_runtime == RESULT); \
      /*STATIC_REQUIRE(CppNameOf<WHAT>() == RESULT);*/ \
   }

#define DEFINE_CPPNAMEOF_CONST_TEST(WHAT, RESULT) \
   WHEN("Taken the raw name of constant " #WHAT) { \
      auto name_runtime = IsolateConstantAtRuntime<WHAT, true, false>(); \
      REQUIRE(name_runtime == RESULT); \
      /*STATIC_REQUIRE(CppNameOf<WHAT>() == RESULT);*/ \
   }


SCENARIO("CppNameOf", "[nameof]") {
   DEFINE_CPPNAMEOF_TYPE_TEST(void, "void")
   DEFINE_CPPNAMEOF_TYPE_TEST(nullptr_t, "std::nullptr_t")
   DEFINE_CPPNAMEOF_TYPE_TEST(int32_t(&)[5], "int32[5]&")
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
   DEFINE_CPPNAMEOF_TYPE_TEST(uint16_t*, "uint16*")

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
      REQUIRE_THROWS(IsolateTypenameAtRuntime<Nаsty, true, false>());
   }

   DEFINE_CPPNAMEOF_TYPE_TEST(IncompleteType, "IncompleteType")
   DEFINE_CPPNAMEOF_TYPE_TEST(IncompleteType*, "IncompleteType*")
   DEFINE_CPPNAMEOF_TYPE_TEST(const IncompleteType*, "IncompleteType const*")
   DEFINE_CPPNAMEOF_TYPE_TEST(const IncompleteType, "IncompleteType const")

   DEFINE_CPPNAMEOF_TYPE_TEST(
       One::Two::Three::TypeDeepIntoNamespaces, 
      "One::Two::Three::TypeDeepIntoNamespaces"
   )

   DEFINE_CPPNAMEOF_TYPE_TEST(
       One::Two::Three::TemplatedTypeDeepIntoNamespaces<char>,
      "One::Two::Three::TemplatedTypeDeepIntoNamespaces<char>"
   )

   DEFINE_CPPNAMEOF_TYPE_TEST(
       One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16_t>, 
      "One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16>"
   )

   DEFINE_CPPNAMEOF_TYPE_TEST(TypeDeepAlias, "One::Two::Three::TypeDeepIntoNamespaces")
   DEFINE_CPPNAMEOF_TYPE_TEST(TypeDeepAlias*, "One::Two::Three::TypeDeepIntoNamespaces*")
   DEFINE_CPPNAMEOF_TYPE_TEST(const TypeDeepAlias, "One::Two::Three::TypeDeepIntoNamespaces const")
   DEFINE_CPPNAMEOF_TYPE_TEST(const TypeDeepAlias*, "One::Two::Three::TypeDeepIntoNamespaces const*")
   DEFINE_CPPNAMEOF_TYPE_TEST(
       One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16_t>::Nested<uint16_t>,
      "One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16>::Nested<uint16>"
   )

   DEFINE_CPPNAMEOF_TYPE_TEST(TemplatedAlias, "One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16>")
   DEFINE_CPPNAMEOF_TYPE_TEST(VeryComplexTemplatedAlias, "One::Two::Three::VeryComplexTemplate<One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16>>")

   DEFINE_CPPNAMEOF_TYPE_TEST(
       One::Two::Three::VeryComplexTemplate<TemplatedAlias>,
      "One::Two::Three::VeryComplexTemplate<One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16>>"
   )

   DEFINE_CPPNAMEOF_TYPE_TEST(Langulus::Flow::Construct, "Langulus::Flow::Construct")
   DEFINE_CPPNAMEOF_TYPE_TEST(Langulus::Flow::Constructconst, "Langulus::Flow::Constructconst")
   DEFINE_CPPNAMEOF_TYPE_TEST(Langulus::Flow::constConstructconst, "Langulus::Flow::constConstructconst")
   DEFINE_CPPNAMEOF_TYPE_TEST(Langulus::Flow::constconst, "Langulus::Flow::constconst")
   DEFINE_CPPNAMEOF_TYPE_TEST(Signature, "<void(void*)>*")

   DEFINE_CPPNAMEOF_CONST_TEST(Pi::Number, "Pi::Number")
   DEFINE_CPPNAMEOF_CONST_TEST(PiNonClass::Number, "Number")
   DEFINE_CPPNAMEOF_CONST_TEST(AnonymousNumber, "AnonymousNumber")

   DEFINE_CPPNAMEOF_CONST_TEST(
       One::Two::Three::TemplatedTypeDeepIntoNamespaces<char>::VeryDeeplyTemplatedEnum::YesYouGotThatRight,
      "One::Two::Three::TemplatedTypeDeepIntoNamespaces<char>::YesYouGotThatRight"
   )

   DEFINE_CPPNAMEOF_CONST_TEST(
       One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16_t>::VeryDeeplyTemplatedEnum::YesYouGotThatRight,
      "One::Two::Three::TemplatedTypeDeepIntoNamespaces<uint16>::YesYouGotThatRight"
   )

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
}
