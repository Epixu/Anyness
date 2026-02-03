///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../any/TestAnyCommon.hpp"
#include <Langulus/Anyness/Many.hpp>
#include <Langulus/Anyness/TMany.hpp>
#include <vector>

namespace doctest
{
   template<>
   struct StringMaker<Many> {
      static String convert(Many const& value) {
         return toString(static_cast<::std::string>(
            NameOf<Many>() + "(" + Convert<Text>(value) + ")"
         ));
      }
   };

   template<class T>
   struct StringMaker<TMany<T>> {
      static String convert(TMany<T> const& value) {
         return toString(static_cast<::std::string>(
            NameOf<TMany<T>>() + "(" + Convert<Text>(value) + ")"
         ));
      }
   };
}

template<class E, CT::Container C> requires CT::NoIntent<C>
void Many_Helper_TestType(const C& many) {
   Any_Helper_TestType<E>(many);
}

template<class LHS, class RHS> requires (CT::Container<LHS, RHS> and CT::NoIntent<LHS, RHS>)
void Many_Helper_TestSame(const LHS& lhs, const RHS& rhs) {
   Any_Helper_TestSame(lhs, rhs);
}

///                                                                           
/// Possible state test implementations                                       
template<class E, CT::Container C> requires CT::NoIntent<C>
void Many_CheckState_Default(const C& many, bool typed = false) {
   Common_CheckState_Default<E>(many, typed);

   REQUIRE_FALSE(many.IsCompressed());
   REQUIRE_FALSE(many.IsEncrypted());
   REQUIRE_FALSE(many.IsMissing());
   REQUIRE_FALSE(many.IsOr());
   REQUIRE_FALSE(many.IsFuture());
   REQUIRE_FALSE(many.IsPast());
}

template<class E, CT::Container C> requires CT::NoIntent<C>
void Many_CheckState_OwnedEmpty(const C& many) {
   Any_CheckState_OwnedEmpty<E>(many);
}

template<class E, CT::Container C> requires CT::NoIntent<C>
void Many_CheckState_OwnedFull(const C& many) {
   Any_CheckState_OwnedFull<E>(many);
}

template<class E, CT::Container C> requires CT::NoIntent<C>
void Many_CheckState_DisownedFull(const C& many) {
   Any_CheckState_DisownedFull<E>(many);
}

template<class E, CT::Container C> requires CT::NoIntent<C>
void Many_CheckState_Abandoned(const C& many) {
   Any_CheckState_Abandoned<E>(many);
}

template<CT::Container T, CT::Intent I> requires CT::NoIntent<T>
void Many_CheckState_ContainsOne(T const& many, I&& e_with_intent, int uses = 1) {
   Any_CheckState_ContainsOne(many, LglsFwd(e_with_intent), uses);

   auto& e = e_with_intent.what;
   using E = typename Decay<Deint<I>>::Type;

   if constexpr (CT::Cloned<I> and CT::Sparse<E>) {
      //TODO test all kinds of ranged modifiers??
      for (auto& it : many)
         REQUIRE(it != *e);
   }
   else {
      //TODO test all kinds of ranged modifiers??
      for (auto& it : many)
         REQUIRE(it == *e);
   }
}

template<CT::Container T, CT::Intent I> requires CT::NoIntent<T>
void Many_CheckState_ContainsN(size_t n, const T& many, I&& e_scoped_with_intent, int uses = 1) {
   auto& e = e_scoped_with_intent.what;
   using E = typename Decay<Deint<I>>::Type;

   REQUIRE(many.GetCount() == n);
   REQUIRE(many.GetUses() == 1);
   REQUIRE(many.GetReserved() >= n);

   for (auto& it : many)
      REQUIRE(it == e);

   for (size_t i = 0; i < n; ++i) {
      REQUIRE(&many.template As<Deptr<E>>(i) ==  e);
      REQUIRE( many.template As<Deptr<E>>(i) == *e);
      REQUIRE(*many.template As<E>(i) == *e);
      REQUIRE( many.template GetRaw<E>()[i] == e);

      if constexpr (CT::Dense<E>)
         REQUIRE(many.GetEntries() == nullptr);
      else if (uses) {
         REQUIRE(many.GetEntries() != nullptr);

         if constexpr (not CT::Disowned<I>) {
            for (size_t indirection = 0; indirection < IndirectsOf<E>; ++indirection) {
               size_t entry_idx = indirection + i * IndirectsOf<E>;
               if constexpr (CT::Cloned<I>)
                  REQUIRE(many.GetEntries()[entry_idx] != e.entries[indirection + 1]);
               else
                  REQUIRE(many.GetEntries()[entry_idx] == e.entries[indirection + 1]);
            }
         }
         else {
            for (size_t indirection = 0; indirection < IndirectsOf<E>; ++indirection) {
               size_t entry_idx = indirection + i * IndirectsOf<E>;
               REQUIRE(many.GetEntries()[entry_idx] == nullptr);
            }
         }
      }

      if constexpr (CT::TypeErased<T>) {
         REQUIRE_THROWS(many.template As<float>(i) == 0.0f);
         REQUIRE_THROWS(many.template As<float*>(i) == nullptr);
      }
   }
}

template<CT::Container T, CT::Intent I> requires (CT::NoIntent<T> and CT::Array<I>)
void Many_CheckState_ContainsArray(const T& many, I&& e_scoped_array_with_intent, int uses = 1) {
   auto  e = e_scoped_array_with_intent.what;
   using E = typename Decay<Deint<I>>::Type;
   constexpr size_t n = ExtentOf<decltype(e_scoped_array_with_intent.what)>;

   REQUIRE(many.GetCount() == n);
   REQUIRE(many.GetUses() == 1);
   REQUIRE(many.GetReserved() >= n);

   int index = 0;
   for (auto& it : many)
      REQUIRE(it == *(e[index++]));
   REQUIRE(index == n);

   for (int i = 0; i < n; ++i) {
      REQUIRE(&many.template As<Deptr<E>>(i) == e[i]);
      REQUIRE( many.template As<Deptr<E>>(i) == *e[i]);
      REQUIRE(*many.template As<E>(i) == *e[i]);
      REQUIRE( many.template GetRaw<E>()[i] == e[i]);

      if constexpr (not CT::Disowned<I>) {
         for (size_t indirection = 0; indirection < IndirectsOf<E>; ++indirection) {
            size_t entry_idx = indirection + i * IndirectsOf<E>;
            if constexpr (CT::Cloned<I>)
               REQUIRE(many.GetEntries()[entry_idx] != e[i].entries[indirection + 1]);
            else
               REQUIRE(many.GetEntries()[entry_idx] == e[i].entries[indirection + 1]);
         }
      }
      else {
         for (size_t indirection = 0; indirection < IndirectsOf<E>; ++indirection) {
            size_t entry_idx = indirection + i * IndirectsOf<E>;
            REQUIRE(many.GetEntries()[entry_idx] == nullptr);
         }
      }

      if constexpr (T::TypeErased) {
         REQUIRE_THROWS(many.template As<float>(i) == 0.0f);
         REQUIRE_THROWS(many.template As<float*>(i) == nullptr);
      }
   }
}

template<CT::Container T, class E> requires CT::NoIntent<T>
void Many_Helper_CompareOne(const T& many, const E& e) {
   Any_Helper_CompareOne(many, e);
}
