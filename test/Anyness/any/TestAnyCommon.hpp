///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../../Main.hpp"
#include "../../TestTypes/ScopedElement.hpp"
#include <Langulus/Anyness/Any.hpp>
#include <Langulus/Anyness/TAny.hpp>
#include <Langulus/Anyness/Text.hpp>

using namespace Langulus;
using namespace Anyness;

/*template<class T, class E>
decltype(auto) FromHelper() {
   if constexpr (not CT::Typed<T>) {
      if constexpr (CT::Tag<T>) {
         if constexpr (CT::DefineTag<T>)
            return T::template OfType<E>();
         else
            return T::template From<Tags::Count, E>();
      }
      else return T::template From<E>();
   }
   else return T {};
}*/


///                                                                           
/// Possible states                                                           
template<class E>
void Any_CheckState_Default(const auto&);
template<class E>
void Any_CheckState_Invariant(const auto&);
template<class E>
void Any_CheckState_OwnedFull(const auto&);
template<class E>
void Any_CheckState_OwnedFullConst(const auto&);
template<class E>
void Any_CheckState_OwnedEmpty(const auto&);
template<class E>
void Any_CheckState_DisownedFull(const auto&);
template<class E>
void Any_CheckState_DisownedFullConst(const auto&);
template<class E>
void Any_CheckState_Abandoned(const auto&);

template<class E>
void Any_Helper_TestType(const auto& many) {
   REQUIRE(many.IsTyped());
   REQUIRE(many.GetType() == MetaDataOf<E>());
   REQUIRE(many.template IsSimilar<E>());
   REQUIRE(many.template IsExact<E>());
   REQUIRE(many.template Is<E>());
   REQUIRE(many.IsSparse() == CT::Sparse<E>);
   REQUIRE(many.IsDeep() == CT::Deep<Decay<E>>);
}

template<CT::Container LHS, CT::Container RHS>
void Any_Helper_TestSame(const LHS& lhs, const RHS& rhs) {
   REQUIRE(lhs.GetRaw() == rhs.GetRaw());
   REQUIRE(lhs.IsExact(rhs.GetType()));
   REQUIRE(lhs == rhs);
   REQUIRE(lhs.IsDeep() == rhs.IsDeep());
   REQUIRE(lhs.IsConstant() == rhs.IsConstant());
   REQUIRE(lhs.GetUnconstrainedState() == rhs.GetUnconstrainedState());
}


///                                                                           
/// Possible state test implementations                                       
template<class E>
void Any_CheckState_Default(const auto& many) {
   using T = Decay<decltype(many)>;

   if constexpr (CT::Typed<T>) {
      static_assert(CT::Exact<TypeOf<T>, E>);
      Any_Helper_TestType<E>(many);
      REQUIRE      (many.GetState() == State::Typed);
   }
   else {
      REQUIRE_FALSE(many.IsTyped());
      REQUIRE      (many.GetType() == nullptr);
      REQUIRE_FALSE(many.IsSparse());
      REQUIRE      (many.GetState() == State::Default);
      REQUIRE_FALSE(many.IsDeep());
   }

   REQUIRE      (many.IsTypeConstrained() == CT::Typed<T>);
   REQUIRE      (many.IsConstant() == CT::Constant<E>);
   REQUIRE_FALSE(many.IsMissing());
   REQUIRE_FALSE(many.IsValid());
   REQUIRE_FALSE(many.GetAllocation());
   REQUIRE_FALSE(many.IsFuture());
   REQUIRE_FALSE(many.IsPast());
   REQUIRE      (many.IsEmpty());
   REQUIRE      (many.GetCount() == 0);
   REQUIRE      (many.GetReserved() == 0);
   REQUIRE      (many.GetUses() == 0);
   REQUIRE      (many.GetRaw() == nullptr);
   REQUIRE_FALSE(many);
   REQUIRE      (not many);
}

template<class E>
void Any_CheckState_OwnedEmpty(const auto& many) {
   using T = Decay<decltype(many)>;

   Any_Helper_TestType<E>(many);

   REQUIRE      (many.IsTypeConstrained() == CT::Typed<T>);
   REQUIRE      (many.IsConstant() == CT::Constant<E>);
   REQUIRE_FALSE(many.IsValid());
   REQUIRE      (many.GetAllocation());
   REQUIRE      (many.IsEmpty());
   REQUIRE      (many.GetCount() == 0);
   REQUIRE      (many.GetReserved() > 0);
   REQUIRE      (many.GetUses() == 1);
   REQUIRE      (many.GetRaw());
   REQUIRE_FALSE(many);
   REQUIRE      (not many);
}

template<class E>
void Any_CheckState_OwnedFull(const auto& many) {
   using T = Decay<decltype(many)>;

   Any_Helper_TestType<E>(many);

   REQUIRE      (many.IsTypeConstrained() == CT::Typed<T>);
   REQUIRE      (many.IsConstant() == CT::Constant<E>);
   REQUIRE      (many.IsValid());
   REQUIRE      (many.GetAllocation());
   REQUIRE_FALSE(many.IsEmpty());
   REQUIRE      (many.GetCount() > 0);
   REQUIRE      (many.GetReserved() > 0);
   REQUIRE      (many.GetUses() > 0);
   REQUIRE      (many.GetRaw());
   REQUIRE      (many);
   REQUIRE_FALSE(not many);
}

template<class E>
void Any_CheckState_DisownedFull(const auto& many) {
   using T = Decay<decltype(many)>;

   Any_Helper_TestType<E>(many);

   REQUIRE      (many.IsTypeConstrained() == CT::Typed<T>);
   REQUIRE      (many.IsConstant() == CT::Constant<E>);
   REQUIRE      (many.IsValid());
   REQUIRE_FALSE(many.GetAllocation());
   REQUIRE_FALSE(many.IsEmpty());
   REQUIRE      (many.GetCount() > 0);
   REQUIRE      (many.GetReserved() > 0);
   REQUIRE      (many.GetUses() == 0);
   REQUIRE      (many.GetRaw());
   REQUIRE      (many);
   REQUIRE_FALSE(not many);
}

template<class E>
void Any_CheckState_DisownedFullConst(const auto& many) {
   using T = Decay<decltype(many)>;

   Any_Helper_TestType<E>(many);

   REQUIRE      (many.IsTypeConstrained() == CT::Typed<T>);
   REQUIRE      (many.IsConstant());
   REQUIRE      (many.IsValid());
   REQUIRE_FALSE(many.GetAllocation());
   REQUIRE_FALSE(many.IsEmpty());
   REQUIRE      (many.GetCount() > 0);
   REQUIRE      (many.GetReserved() > 0);
   REQUIRE      (many.GetUses() == 0);
   REQUIRE      (many.GetRaw());
   REQUIRE      (many);
   REQUIRE_FALSE(not many);
}

template<class E>
void Any_CheckState_Abandoned(const auto& many) {
   REQUIRE_FALSE(many.GetAllocation());
}

void Any_CheckState_ContainsOne(const auto& pack, const auto& e, Allocation* entry = nullptr) {
   using T = Deref<decltype(pack)>;
   using E = Deref<decltype(e)>;
   (void) entry;

   REQUIRE(pack.GetCount() == 1);
   REQUIRE(pack.GetUses() == 1);
   REQUIRE(pack.GetReserved() >= 1);

   for (auto& it : pack)
      REQUIRE(it == e);

   if constexpr (CT::Sparse<E>) {
      REQUIRE(&pack.template As<Deptr<E>>() ==  e);
      REQUIRE( pack.template As<Deptr<E>>() == *e);
      REQUIRE(*pack.template As<E>() == *e);
      REQUIRE(*pack.template GetRaw<E>() == e);
   }
   else if constexpr (T::TypeErased or CT::Same<TypeOf<T>, E>) {
      REQUIRE(pack.template As<E>() == e);
   }

   IF_LANGULUS_MANAGED_MEMORY(REQUIRE(*pack.GetEntries() == entry));

   if constexpr (T::TypeErased) {
      REQUIRE_THROWS(pack.template As<float>() == 0.0f);
      REQUIRE_THROWS(pack.template As<float*>() == nullptr);
   }
}
