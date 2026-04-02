///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../set/TestSetCommon.hpp"
#include <Langulus/Anyness/Map.hpp>
#include <Langulus/Anyness/TMap.hpp>
#include <Langulus/Anyness/Pair.hpp>
#include <Langulus/Anyness/TPair.hpp>
#include <unordered_map>

#if LANGULUS(BENCHMARK)
   /// Perform a persistent benchmark across build and verify performance     
   #define BenchmarkMap(func, tolerance, my_init, my) { \
      const auto token = ::std::string("Test/") + static_cast<::std::string>(func) + " |" + static_cast<::std::string>(NameOf<T>()) + "|"; \
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

   /// Perform two persistent benchmarks across builds - one for Map and      
   /// one for std::unordered_map. Make sure they don't deviate in a bad way. 
   #define BenchmarkMapStd(func, tolerance_highscore, tolerance, my_init, my, theirs_init, theirs) { \
      const auto token = ::std::string("Test/") + static_cast<::std::string>(func) + " |" + static_cast<::std::string>(NameOf<T>()) + "|"; \
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
      const auto token_std = ::std::string("Test/") + static_cast<::std::string>(func) + " |std::unordered_map|"; \
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
   #define BenchmarkMap(func, tolerance, my_init, my)
   #define BenchmarkMapStd(func, tolerance_highscore, tolerance, my_init, my, theirs_init, theirs)
#endif

namespace doctest
{
   template<Anyness::State::StateValue SORT>
   struct StringMaker<Anyness::Inner::Map<SORT>> {
      static String convert(Anyness::Inner::Map<SORT> const& value) {
         return toString(static_cast<::std::string>(
            NameOf<Anyness::Inner::Map<SORT>>() + "(" + Convert<Text>(value) + ")"
         ));
      }
   };

   template<CT::NotVoid K, CT::NotVoid V, Anyness::State::StateValue SORT>
   struct StringMaker<TMap<K, V, SORT>> {
      static String convert(TMap<K, V, SORT> const& value) {
         return toString(static_cast<::std::string>(
            NameOf<TMap<K, V, SORT>>() + "(" + Convert<Text>(value) + ")"
         ));
      }
   };
}




template<class K, class V, CT::Container C> requires CT::NoIntent<C>
void Map_Helper_TestType(C const& map) {
   if constexpr (CT::Void<K>) {
      REQUIRE(    map.template IsKeySame<int>());
      REQUIRE(    map.template IsKeyExact<int>());
      REQUIRE(    map.template IsKey<int>());
      REQUIRE(not map.IsKeySparse());
      REQUIRE(not map.IsKeyDeep());
      REQUIRE(    map.GetKeyType() == MetaDataOf<int>());
   }
   else {
      REQUIRE(    map.template IsKeySame<K>());
      REQUIRE(    map.template IsKeyExact<K>());
      REQUIRE(    map.template IsKey<K>());
      REQUIRE(    map.IsKeySparse() == CT::Sparse<K>);
      REQUIRE(    map.IsKeyDeep() == CT::Deep<K>);
      REQUIRE(    map.GetKeyType() == MetaDataOf<K>());      
   }
   
   if constexpr (CT::Void<V>) {
      REQUIRE(    map.template IsValSame<int>());
      REQUIRE(    map.template IsValExact<int>());
      REQUIRE(    map.template IsVal<int>());
      REQUIRE(not map.IsValSparse());
      REQUIRE(not map.IsValDeep());
      REQUIRE(    map.GetValType() == MetaDataOf<int>());
   }
   else {
      REQUIRE(    map.template IsValSame<V>());
      REQUIRE(    map.template IsValExact<V>());
      REQUIRE(    map.template IsVal<V>());
      REQUIRE(    map.IsValSparse() == CT::Sparse<V>);
      REQUIRE(    map.IsValDeep() == CT::Deep<V>);
      REQUIRE(    map.GetValType() == MetaDataOf<V>());      
   }
   
   REQUIRE(map.IsKeyTyped());
   REQUIRE(map.IsValTyped());
}

template<class LHS, class RHS> requires (CT::Container<LHS, RHS> and CT::NoIntent<LHS, RHS>)
void Map_Helper_TestSame(const LHS& lhs, const RHS& rhs) {
   REQUIRE(lhs.GetCount() == rhs.GetCount());
   if (not lhs.IsEmpty())
      REQUIRE(lhs.GetRaw() == rhs.GetRaw()); // not really a requirement when containers are empty
   REQUIRE(lhs.IsKeyExact(rhs.GetKeyType()));
   REQUIRE(lhs.IsValExact(rhs.GetValType()));
   REQUIRE(lhs == rhs);
   REQUIRE(lhs.IsKeyDeep() == rhs.IsKeyDeep());
   REQUIRE(lhs.IsValDeep() == rhs.IsValDeep());
   REQUIRE(lhs.IsConstant() == rhs.IsConstant());
   REQUIRE(lhs.GetUnconstrainedState() == rhs.GetUnconstrainedState());
}

///                                                                           
/// Possible state test implementations                                       
template<class K, class V, CT::Container C> requires CT::NoIntent<C>
void Map_CheckState_Default(C const& map, bool typed = false) {
   if constexpr (CT::Typed<C>) {
      static_assert(Exact<typename TypeOf<C>::First,  K>);
      static_assert(Exact<typename TypeOf<C>::Second, V>);
      static_assert(Exact<typename C::Key,  K>);
      static_assert(Exact<typename C::Val,  V>);
      Map_Helper_TestType<K, V>(map);

      if constexpr (requires { map.GetState(); })
         REQUIRE(map.GetState() == State::Typed);
   }
   else if (not typed) {
      REQUIRE_FALSE(map.IsKeyTyped());
      REQUIRE      (map.GetKeyType() == nullptr);
      REQUIRE_FALSE(map.IsKeySparse());
      REQUIRE_FALSE(map.IsKeyDeep());

      REQUIRE_FALSE(map.IsValTyped());
      REQUIRE      (map.GetValType() == nullptr);
      REQUIRE_FALSE(map.IsValSparse());
      REQUIRE_FALSE(map.IsValDeep());

      if constexpr (requires { map.GetState(); })
         REQUIRE(map.GetState() == State::Default);
   }
   else {
      Map_Helper_TestType<K, V>(map);

      if constexpr (requires { map.GetState(); })
         REQUIRE(map.GetState() == State::Default);
   }

   REQUIRE      (map.IsTypeConstrained() == CT::Typed<C>);
   REQUIRE      (map.IsConstant());
   REQUIRE_FALSE(map.IsValid());
   REQUIRE_FALSE(map.GetAllocation());
   REQUIRE      (map.IsEmpty());
   REQUIRE      (map.GetCount() == 0);
   REQUIRE      (map.GetReserved() == 0);
   REQUIRE      (map.GetUses() == 0);
   //REQUIRE      (map.GetRaw() == nullptr); // not really a requirement for the default state. Count being 0 is enough in most cases
   REQUIRE_FALSE(map);
   REQUIRE      (not map);

   Many_CheckState_Default<K>(map.GetKeys());
   Many_CheckState_Default<V>(map.GetVals());

   REQUIRE_FALSE(map.IsCompressed());
   REQUIRE_FALSE(map.IsEncrypted());
   REQUIRE_FALSE(map.IsSorted());
}

template<class K, class V, CT::Container C> requires CT::NoIntent<C>
void Map_CheckState_OwnedEmpty(C const& map) {
   Map_Helper_TestType<K, V>(map);

   REQUIRE      (map.IsTypeConstrained() == CT::Typed<C>);
   REQUIRE      (map.IsConstant() == CT::Constant<V>);
   REQUIRE_FALSE(map.IsValid());
   REQUIRE      (map.GetAllocation());
   REQUIRE      (map.IsEmpty());
   REQUIRE      (map.GetCount() == 0);
   REQUIRE      (map.GetReserved() > 0);
   REQUIRE      (map.GetUses() == 1);
   //REQUIRE      (map.GetRaw() == nullptr); // not really a requirement for the owned-empty state. Count being 0 is enough in most cases
   REQUIRE_FALSE(map);
   REQUIRE      (not map);

   Many_CheckState_OwnedEmpty<K>(map.GetKeys());
   Many_CheckState_OwnedEmpty<V>(map.GetVals());
}

template<class K, class V, CT::Container C> requires CT::NoIntent<C>
void Map_CheckState_OwnedFull(C const& map) {
   Map_Helper_TestType<K, V>(map);

   REQUIRE      (map.IsTypeConstrained() == CT::Typed<C>);
   REQUIRE      (map.IsConstant() == CT::Constant<V>);
   REQUIRE      (map.IsValid());
   REQUIRE      (map.GetAllocation());
   REQUIRE_FALSE(map.IsEmpty());
   REQUIRE      (map.GetCount() > 0);
   REQUIRE      (map.GetReserved() > 0);
   REQUIRE      (map.GetUses() > 0);
   REQUIRE      (map.GetRaw());
   REQUIRE      (map);
   REQUIRE_FALSE(not map);

   Many_CheckState_OwnedFull<K>(map.GetKeys());
   Many_CheckState_OwnedFull<V>(map.GetVals());
}

template<class K, class V, CT::Container C> requires CT::NoIntent<C>
void Map_CheckState_DisownedFull(C const& map) {
   Map_Helper_TestType<K, V>(map);

   REQUIRE      (map.IsTypeConstrained() == CT::Typed<C>);
   REQUIRE      (map.IsConstant());
   REQUIRE      (map.IsValid());
   REQUIRE_FALSE(map.GetAllocation());
   REQUIRE_FALSE(map.IsEmpty());
   REQUIRE      (map.GetCount() > 0);
   REQUIRE      (map.GetReserved() > 0); // Many keeps its reserved count as a member, so it's allowed to be absorbed and passed around
   REQUIRE      (map.GetUses() == 0);
   REQUIRE      (map.GetRaw());
   REQUIRE      (map);
   REQUIRE_FALSE(not map);

   Many_CheckState_DisownedFull<K>(map.GetKeys());
   Many_CheckState_DisownedFull<V>(map.GetVals());
}

template<class K, class V, CT::Container C> requires CT::NoIntent<C>
void Map_CheckState_Abandoned(C const& map) {
   REQUIRE_FALSE(map.GetAllocation());

   Many_CheckState_Abandoned<K>(map.GetKeys());
   Many_CheckState_Abandoned<V>(map.GetVals());
}

template<CT::Container T, CT::Intent I> requires CT::NoIntent<T>
void Map_CheckState_ContainsOne(T const& map, I&& e_with_intent, int uses = 1) {
   Map_VerifyAccessorInterface(map, LglsFwd(e_with_intent));

   auto& e = e_with_intent.what;
   using E = typename Decay<Deint<I>>::Type;

   if constexpr (CT::Deep<E> and CT::Dense<E>)
      REQUIRE(map.template AsAt<E*>(0)->template IsSame<int>());

   REQUIRE(map.GetCount() == 1);
   REQUIRE(map.GetUses() == uses);
   REQUIRE(map.GetReserved() >= (uses ? 1 : 0));

   if constexpr (not CT::CustomPointer<E>)
      REQUIRE(map.template AsAt<Decay<E>>(0) == DenseCast(*e));

   if constexpr (CT::Cloned<I> and CT::Sparse<E>) {
      REQUIRE(map.template AsAt<E>(0) != *e);
      REQUIRE((*map.template AsAt<E*>(0)) != *e);
   }
   else {
      REQUIRE(map.template AsAt<E>(0) == *e);
      REQUIRE((*map.template AsAt<E*>(0)) == *e);
   }

   if constexpr (CT::Dense<E>)
      REQUIRE(map.GetEntries() == nullptr);
   else if (uses) {
      REQUIRE(map.GetEntries() != nullptr);

      if constexpr (not CT::Disowned<I>) {
         for (size_t i = 0; i < IndirectsOf<E>; ++i) {
            if constexpr (CT::Cloned<I>)
               REQUIRE(map.GetEntriesAt(0)[i] != e.entries[i + 1]);
            else
               REQUIRE(map.GetEntriesAt(0)[i] == e.entries[i + 1]);
         }
      }
      else {
         for (size_t i = 0; i < IndirectsOf<E>; ++i)
            REQUIRE(map.GetEntriesAt(0)[i] == nullptr);
      }
   }

   if constexpr (CT::TypeErased<T>) {
      REQUIRE_THROWS(map.template AsAt<float>(0) == 0.0f);
      REQUIRE_THROWS(map.template AsAt<float*>(0) == nullptr);
   }

   if constexpr (CT::Cloned<I> and CT::Sparse<E>) {
      //TODO test all kinds of ranged modifiers??
      for (auto& it : map) {
         if constexpr (CT::TypeErased<T>)
            REQUIRE(not it.CompareOneEqual(*e));
         else
            REQUIRE(it != *e);
      }
   }
   else {
      //TODO test all kinds of ranged modifiers??
      for (auto& it : map) {
         if constexpr (CT::TypeErased<T>)
            REQUIRE(it.CompareOneEqual(*e));
         else
            REQUIRE(it == *e);
      }
   }
}

template<CT::Container T, CT::Intent I> requires CT::NoIntent<T>
void Map_CheckState_ContainsN(size_t n, const T& map, I&& e_scoped_with_intent, int uses = 1) {
   auto& e = e_scoped_with_intent.what;
   //using E = typename Decay<Deint<I>>::Type;

   REQUIRE(map.GetCount() == n);
   REQUIRE(map.GetUses() == uses);
   REQUIRE(map.GetReserved() >= n);

   for (auto& it : map)
      REQUIRE(it == e);

   //TODO other kinds of iterations
}

template<CT::Container T, CT::Intent I> requires (CT::NoIntent<T> and CT::Array<I>)
void Map_CheckState_ContainsArray(const T& map, I&& e_scoped_array_with_intent) {
   auto  e = e_scoped_array_with_intent.what;
   //using E = typename Decay<Deint<I>>::Type;
   constexpr size_t n = ExtentOf<decltype(e_scoped_array_with_intent.what)>;

   REQUIRE(map.GetCount() == n);
   REQUIRE(map.GetUses() == 1);
   REQUIRE(map.GetReserved() >= n);

   //TODO
}

template<CT::Container T, class E> requires CT::NoIntent<T>
void Map_Helper_CompareOne(const T& map, const E& e) {
   //TODO Many_Helper_CompareOne(map, e);
}
