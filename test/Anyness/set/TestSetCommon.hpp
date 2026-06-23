///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../many/TestManyCommon.hpp"
#include <Langulus/Anyness/Set.hpp>
#include <Langulus/Anyness/TSet.hpp>
#include <unordered_set>
#include <set>

#if LANGULUS(BENCHMARK)
   /// Perform a persistent benchmark across build and verify performance     
   #define BenchmarkSet(func, tolerance, my_init, my) { \
      const auto token = ::std::string("Test/") + func + "(" + NameOf<E>() ") |" + NameOf<T>() + "|"; \
      volatile int i = 0; \
      for (; i < BenchmarkWarmupCycles; i += 1) { \
         my_init; \
         my; \
      } \
      for (; i < BenchmarkWarmupCycles + BenchmarkMeasureCycles; i += 1) { \
         my_init; \
         { \
            CTRACK_NAME_PERSIST(token.c_str()); \
            my; \
         } \
      } \
      auto results = ctrack::result_get_detail_table(); \
      results.check_highscore(tolerance); \
   }

   /// Perform two persistent benchmarks across builds - one for Set and      
   /// one for std::unordered_set. Make sure they don't deviate in a bad way. 
   #define BenchmarkSetStd(func, tolerance_highscore, tolerance, my_init, my, theirs_init, theirs) { \
      const auto token = ::std::string("Test/") + func + "(" + NameOf<E>() ") |" + NameOf<T>() + "|"; \
      volatile int i = 0; \
      for (; i < BenchmarkWarmupCycles; i += 1) { \
         my_init; \
         my; \
      } \
      for (; i < BenchmarkWarmupCycles + BenchmarkMeasureCycles; i += 1) { \
         my_init; \
         { \
            CTRACK_NAME_PERSIST(token.c_str()); \
            my; \
         } \
      } \
      i = 0; \
      const auto token_std = ::std::string("Test/") + func + "(" + NameOf<E>() ") |std::unordered_set|"; \
      for (; i < BenchmarkWarmupCycles; i += 1) { \
         theirs_init; \
         theirs; \
      } \
      for (; i < BenchmarkWarmupCycles + BenchmarkMeasureCycles; i += 1) { \
         theirs_init; \
         { \
            CTRACK_NAME(token_std.c_str()); \
            theirs; \
         } \
      } \
      auto results = ctrack::result_get_detail_table(); \
      results.check_highscore(tolerance_highscore); \
      REQUIRE(results.check_same(token.c_str(), token_std.c_str(), tolerance)); \
   }
#else
   #define BenchmarkSet(func, tolerance, my_init, my)
   #define BenchmarkSetStd(func, tolerance_highscore, tolerance, my_init, my, theirs_init, theirs)
#endif

namespace doctest
{
   template<Anyness::StateValue SORT>
   struct StringMaker<Anyness::Inner::Set<SORT>> {
      static String convert(Anyness::Inner::Set<SORT> const& value) {
         return toString(static_cast<::std::string>(
            NameOf<Anyness::Inner::Set<SORT>>() + "(" + Convert<Text>(value) + ")"
         ));
      }
   };

   template<CT::NotVoid T, Anyness::StateValue SORT>
   struct StringMaker<TSet<T, SORT>> {
      static String convert(TSet<T, SORT> const& value) {
         return toString(static_cast<::std::string>(
            NameOf<TSet<T, SORT>>() + "(" + Convert<Text>(value) + ")"
         ));
      }
   };
}

template<class E, CT::Container C> requires CT::NoIntent<C>
void Set_Helper_TestType(const C& set) {
   Many_Helper_TestType<E>(set);
}

template<class LHS, class RHS> requires (CT::Container<LHS, RHS> and CT::NoIntent<LHS, RHS>)
void Set_Helper_TestSame(const LHS& lhs, const RHS& rhs, bool match_constness = true) {
   Many_Helper_TestSame(lhs, rhs, match_constness);
}

///                                                                           
/// Possible state test implementations                                       
template<class E, CT::Container C> requires CT::NoIntent<C>
void Set_CheckState_Default(const C& set, bool typed = false) {
   Common_CheckState_Default<E>(set, typed);

   REQUIRE_FALSE(set.IsCompressed());
   REQUIRE_FALSE(set.IsEncrypted());
   REQUIRE_FALSE(set.IsSorted());
}

template<class E, CT::Container C> requires CT::NoIntent<C>
void Set_CheckState_OwnedEmpty(const C& set) {
   Many_CheckState_OwnedEmpty<E>(set);
}

template<class E, CT::Container C> requires CT::NoIntent<C>
void Set_CheckState_OwnedFull(const C& set) {
   Many_CheckState_OwnedFull<E>(set);
}

template<class E, CT::Container C> requires CT::NoIntent<C>
void Set_CheckState_DisownedFull(const C& set) {
   Many_CheckState_DisownedFull<E>(set);
}

template<class E, CT::Container C> requires CT::NoIntent<C>
void Set_CheckState_Abandoned(const C& set) {
   Many_CheckState_Abandoned<E>(set);
}

template<CT::Container T, CT::Intent I> requires CT::NoIntent<T>
void Set_CheckState_ContainsOne(T const& set, I&& e_with_intent, int uses = 1) {
   Many_VerifyAccessorInterface(set, LglsFwd(e_with_intent));

   auto& e = e_with_intent.what;
   using E = typename Decay<Deint<I>>::Type;

   if constexpr (CT::DeepDense<E>)
      REQUIRE(set.template AsAt<E*>(0)->template IsSame<int>());

   REQUIRE(set.GetCount() == 1);
   REQUIRE(set.GetUses() == uses);
   REQUIRE(set.GetReserved() >= (uses ? 1 : 0));

   if constexpr (not CT::CustomPointer<E>)
      REQUIRE(set.template AsAt<Decay<E>>(0) == DenseCast(*e));

   if constexpr (CT::Cloned<I> and CT::Sparse<E>) {
      REQUIRE(set.template AsAt<E>(0) != *e);
      REQUIRE((*set.template AsAt<E*>(0)) != *e);
   }
   else {
      REQUIRE(set.template AsAt<E>(0) == *e);
      REQUIRE((*set.template AsAt<E*>(0)) == *e);
   }

   if constexpr (CT::OwnedDeep<T>) {
      if constexpr (CT::Dense<E>)
         REQUIRE(set.GetEntries() == nullptr);
      else if (uses) {
         REQUIRE(set.GetEntries() != nullptr);

         if constexpr (not CT::Disowned<I>) {
            for (size_t i = 0; i < IndirectsOf<E>; ++i) {
               if constexpr (CT::Cloned<I>)
                  REQUIRE(set.GetEntriesAt(0)[i] != e.entries[i + 1]);
               else
                  REQUIRE(set.GetEntriesAt(0)[i] == e.entries[i + 1]);
            }
         }
         else {
            for (size_t i = 0; i < IndirectsOf<E>; ++i)
               REQUIRE(set.GetEntriesAt(0)[i] == nullptr);
         }
      }
   }

   if constexpr (CT::TypeErased<T>) {
      REQUIRE_THROWS(set.template AsAt<float>(0) == 0.0f);
      REQUIRE_THROWS(set.template AsAt<float*>(0) == nullptr);
   }

   if constexpr (CT::Cloned<I> and CT::Sparse<E>) {
      //TODO test all kinds of ranged modifiers??
      for (auto& it : set) {
         if constexpr (CT::TypeErased<T>)
            REQUIRE(not it.CompareOneEqual(*e));
         else
            REQUIRE(it != *e);
      }
   }
   else {
      //TODO test all kinds of ranged modifiers??
      for (auto& it : set) {
         if constexpr (CT::TypeErased<T>)
            REQUIRE(it.CompareOneEqual(*e));
         else
            REQUIRE(it == *e);
      }
   }
}

template<CT::Container T, CT::Intent I> requires CT::NoIntent<T>
void Set_CheckState_ContainsN(size_t n, const T& set, I&& e_scoped_with_intent, int uses = 1) {
   auto& e = e_scoped_with_intent.what;
   //using E = typename Decay<Deint<I>>::Type;

   REQUIRE(set.GetCount() == n);
   REQUIRE(set.GetUses() == uses);
   REQUIRE(set.GetReserved() >= n);

   for (auto& it : set)
      REQUIRE(it == e);

   //TODO other kinds of iterations
}

template<CT::Container T, CT::Intent I> requires (CT::NoIntent<T> and CT::Array<I>)
void Set_CheckState_ContainsArray(const T& set, I&& e_scoped_array_with_intent) {
   auto  e = e_scoped_array_with_intent.what;
   //using E = typename Decay<Deint<I>>::Type;
   constexpr size_t n = ExtentOf<decltype(e_scoped_array_with_intent.what)>;

   REQUIRE(set.GetCount() == n);
   REQUIRE(set.GetUses() == 1);
   REQUIRE(set.GetReserved() >= n);

   //TODO
}

template<CT::Container T, class E> requires CT::NoIntent<T>
void Set_Helper_CompareOne(const T& set, const E& e) {
   Many_Helper_CompareOne(set, e);
}
