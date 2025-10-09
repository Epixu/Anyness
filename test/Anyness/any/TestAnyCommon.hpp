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

namespace Catch
{
   template<>
   struct is_range<Any> { static const bool value = false; };
   template<class T>
   struct is_range<TAny<T>> { static const bool value = false; };

   template<>
   struct StringMaker<Any> {
      static std::string convert(Any const& value) {
         return NameOf<Any>() + "(" + static_cast<::std::string>(value) + ")";
      }
   };
   template<class T>
   struct StringMaker<TAny<T>> {
      static std::string convert(TAny<T> const& value) {
         return NameOf<TAny<T>>() + "(" + static_cast<::std::string>(value) + ")";
      }
   };
}


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
   REQUIRE(many.template IsSame<E>());
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
void Any_CheckState_Default(const auto& any) {
   using T = Decay<decltype(any)>;

   if constexpr (CT::Typed<T>) {
      static_assert(Exact<TypeOf<T>, E>);
      Any_Helper_TestType<E>(any);

      if constexpr (requires { any.GetState(); })
         REQUIRE(any.GetState() == State::Typed);
   }
   else {
      REQUIRE_FALSE(any.IsTyped());
      REQUIRE      (any.GetType() == nullptr);
      REQUIRE_FALSE(any.IsSparse());
      REQUIRE_FALSE(any.IsDeep());
      
      if constexpr (requires { any.GetState(); })
         REQUIRE(any.GetState() == State::Default);
   }

   REQUIRE      (any.IsTypeConstrained() == CT::Typed<T>);
   REQUIRE      (any.IsConstant() /*== CT::Constant<E>*/);
   REQUIRE_FALSE(any.IsValid());
   REQUIRE_FALSE(any.GetAllocation());
   REQUIRE      (any.IsEmpty());
   REQUIRE      (any.GetCount() == 0);
   REQUIRE      (any.GetReserved() == 0);
   REQUIRE      (any.GetUses() == 0);
   REQUIRE      (any.GetRaw() == nullptr);
   REQUIRE_FALSE(any);
   REQUIRE      (not any);

   if constexpr (requires { any.GetState(); }) {
      REQUIRE_FALSE(any.IsMissing());
      REQUIRE_FALSE(any.IsFuture());
      REQUIRE_FALSE(any.IsPast());      
   }
}

template<class E>
void Any_CheckState_OwnedEmpty(const auto& any) {
   using T = Decay<decltype(any)>;

   Any_Helper_TestType<E>(any);

   REQUIRE      (any.IsTypeConstrained() == CT::Typed<T>);
   REQUIRE      (any.IsConstant() == CT::Constant<E>);
   REQUIRE_FALSE(any.IsValid());
   REQUIRE      (any.GetAllocation());
   REQUIRE      (any.IsEmpty());
   REQUIRE      (any.GetCount() == 0);
   REQUIRE      (any.GetReserved() > 0);
   REQUIRE      (any.GetUses() == 1);
   REQUIRE      (any.GetRaw() == nullptr);
   REQUIRE_FALSE(any);
   REQUIRE      (not any);
}

template<class E>
void Any_CheckState_OwnedFull(const auto& any) {
   using T = Decay<decltype(any)>;

   Any_Helper_TestType<E>(any);

   REQUIRE      (any.IsTypeConstrained() == CT::Typed<T>);
   REQUIRE      (any.IsConstant() == CT::Constant<E>);
   REQUIRE      (any.IsValid());
   REQUIRE      (any.GetAllocation());
   REQUIRE_FALSE(any.IsEmpty());
   REQUIRE      (any.GetCount() > 0);
   REQUIRE      (any.GetReserved() > 0);
   REQUIRE      (any.GetUses() > 0);
   REQUIRE      (any.GetRaw());
   REQUIRE      (any);
   REQUIRE_FALSE(not any);
}

template<class E>
void Any_CheckState_DisownedFull(const auto& any) {
   using T = Decay<decltype(any)>;

   Any_Helper_TestType<E>(any);

   REQUIRE      (any.IsTypeConstrained() == CT::Typed<T>);
   REQUIRE      (any.IsConstant() /*== CT::Constant<E>*/);
   REQUIRE      (any.IsValid());
   REQUIRE_FALSE(any.GetAllocation());
   REQUIRE_FALSE(any.IsEmpty());
   REQUIRE      (any.GetCount() > 0);
   REQUIRE      (any.GetReserved() == 0 /*> 0*/);
   REQUIRE      (any.GetUses() == 0);
   REQUIRE      (any.GetRaw());
   REQUIRE      (any);
   REQUIRE_FALSE(not any);
}

template<class E>
void Any_CheckState_DisownedFullConst(const auto& any) {
   using T = Decay<decltype(any)>;

   Any_Helper_TestType<E>(any);

   REQUIRE      (any.IsTypeConstrained() == CT::Typed<T>);
   REQUIRE      (any.IsConstant());
   REQUIRE      (any.IsValid());
   REQUIRE_FALSE(any.GetAllocation());
   REQUIRE_FALSE(any.IsEmpty());
   REQUIRE      (any.GetCount() > 0);
   REQUIRE      (any.GetReserved() == 0 /*> 0*/);
   REQUIRE      (any.GetUses() == 0);
   REQUIRE      (any.GetRaw());
   REQUIRE      (any);
   REQUIRE_FALSE(not any);
}

template<class E>
void Any_CheckState_Abandoned(const auto& any) {
   REQUIRE_FALSE(any.GetAllocation());
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
   else if constexpr (T::TypeErased or Akin<TypeOf<T>, E>) {
      REQUIRE(pack.template As<E>() == e);
   }

   IF_LANGULUS_MANAGED_MEMORY(REQUIRE(*pack.GetEntries() == entry));

   if constexpr (T::TypeErased) {
      REQUIRE_THROWS(pack.template As<float>() == 0.0f);
      REQUIRE_THROWS(pack.template As<float*>() == nullptr);
   }
}

template<class T, class E>
void Any_Helper_CompareOne(const T& pack, const E& e) {
   if constexpr (CT::TypeErased<T>) {
      REQUIRE(pack.CompareOne(e) == Compared::Equal);
      REQUIRE(pack.CompareOneEqual(e) == true);
   }
   else {
      REQUIRE(pack.CompareOne(e) == ::std::partial_ordering::equivalent);
      REQUIRE(pack.CompareOneEqual(e) == true);
   }

   if constexpr (CT::Deep<E> and LANGULUS(SAFE))
      REQUIRE_THROWS(pack == e);
   else
      REQUIRE_NOTHROW(pack == e);
}
