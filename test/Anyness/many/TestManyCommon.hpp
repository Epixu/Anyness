///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           

/// INTENTIONALLY NOT GUARDED                                                 
/// Include this file once in each cpp file, after all other headers          
#include <Langulus/Anyness/Text.hpp>
#include <Langulus/Anyness/Many.hpp>
#include <Langulus/CT/Deep.hpp>
#include <Langulus/Tag.hpp>
#include "../Common.hpp"


template<class T, class E>
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
}


///                                                                           
/// Possible states:                                                          
///   - uninitialized                                                         
///   - default                                                               
template<class E>
void Many_CheckState_Default(const auto&);
///   - invariant                                                             
template<class E>
void Many_CheckState_Invariant(const auto&);
///   - owned-full                                                            
template<class E>
void Many_CheckState_OwnedFull(const auto&);
///   - owned-full-const                                                      
template<class E>
void Many_CheckState_OwnedFullConst(const auto&);
///   - owned-empty                                                           
template<class E>
void Many_CheckState_OwnedEmpty(const auto&);
///   - disowned-full                                                         
template<class E>
void Many_CheckState_DisownedFull(const auto&);
///   - disowned-full-const                                                   
template<class E>
void Many_CheckState_DisownedFullConst(const auto&);
///   - abandoned                                                             
template<class E>
void Many_CheckState_Abandoned(const auto&);

template<class E>
void Many_Helper_TestType(const auto& many) {
   REQUIRE      (many.IsTyped());
   REQUIRE_FALSE(many.IsUntyped());
   REQUIRE      (many.GetType() == MetaDataOf<E>());
   REQUIRE      (many.GetType()->template IsSimilar<const E>());
   REQUIRE      (many.GetType()->template IsExact<E>());
   REQUIRE      (many.GetType()->template Is<E*>());
   REQUIRE      (many.IsDense() == CT::Dense<E>);
   REQUIRE      (many.IsSparse() == CT::Sparse<E>);
   REQUIRE      (many.IsDeep() == CT::Deep<Decay<E>>);
}

template<CT::Container LHS, CT::Container RHS>
void Many_Helper_TestSame(const LHS& lhs, const RHS& rhs) {
   REQUIRE(lhs.GetRaw() == rhs.GetRaw());
   REQUIRE(lhs.IsExact(rhs.GetType()));
   REQUIRE(lhs == rhs);
   REQUIRE(lhs.IsDeep() == rhs.IsDeep());
   REQUIRE(lhs.IsConstant() == rhs.IsConstant());
   REQUIRE(lhs.GetUnconstrainedState() == rhs.GetUnconstrainedState());
}


///                                                                           
/// Possible actions for each state:                                          
///   - uninitialized                                                         
///      - constexpr-default-initialized                                      
///      - runtime-default-initialized                                        
///      - intent-initialized from container                                  
///      - intent-initialized from single dense element                       
///      - intent-initialized from multiple dense elements                    
///      - intent-initialized from dense element bounded array                

template<class E>
void Many_CheckState_Default(const auto& many) {
   using T = Decay<decltype(many)>;

   if constexpr (CT::Typed<T>) {
      static_assert(CT::Exact<TypeOf<T>, E>);
      Many_Helper_TestType<E>(many);
      REQUIRE      (many.GetState() == State::Typed);
   }
   else {
      REQUIRE_FALSE(many.IsTyped());
      REQUIRE      (many.IsUntyped());
      REQUIRE      (many.GetType() == nullptr);
      REQUIRE      (many.IsDense());
      REQUIRE_FALSE(many.IsSparse());
      REQUIRE      (many.GetState() == State::Default);
      REQUIRE_FALSE(many.IsDeep());
   }

   REQUIRE      (many.IsTypeConstrained() == CT::Typed<T>);
   REQUIRE_FALSE(many.IsCompressed());
   REQUIRE      (many.IsConstant() == CT::Constant<E>);
   REQUIRE_FALSE(many.IsEncrypted());
   REQUIRE_FALSE(many.IsMissing());
   REQUIRE_FALSE(many.IsOr());
   REQUIRE_FALSE(many.IsStatic());
   REQUIRE_FALSE(many.IsValid());
   REQUIRE      (many.IsInvalid());
   REQUIRE_FALSE(many.GetAllocation());
   REQUIRE      (many.IsNow());
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
void Many_CheckState_OwnedEmpty(const auto& many) {
   using T = Decay<decltype(many)>;

   Any_Helper_TestType<E>(many);

   REQUIRE      (many.IsTypeConstrained() == CT::Typed<T>);
   REQUIRE_FALSE(many.IsCompressed());
   REQUIRE      (many.IsConstant() == CT::Constant<E>);
   REQUIRE_FALSE(many.IsEncrypted());
   REQUIRE_FALSE(many.IsStatic());
   REQUIRE_FALSE(many.IsValid());
   REQUIRE      (many.IsInvalid());
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
void Many_CheckState_OwnedFull(const auto& many) {
   using T = Decay<decltype(many)>;

   Many_Helper_TestType<E>(many);

   REQUIRE      (many.IsTypeConstrained() == CT::Typed<T>);
   REQUIRE_FALSE(many.IsCompressed());
   REQUIRE      (many.IsConstant() == CT::Constant<E>);
   REQUIRE_FALSE(many.IsEncrypted());
   REQUIRE      (many.IsValid());
   REQUIRE_FALSE(many.IsInvalid());
   REQUIRE_FALSE(many.IsStatic());
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
void Many_CheckState_DisownedFull(const auto& many) {
   using T = Decay<decltype(many)>;

   Many_Helper_TestType<E>(many);

   REQUIRE      (many.IsTypeConstrained() == CT::Typed<T>);
   REQUIRE_FALSE(many.IsCompressed());
   REQUIRE      (many.IsConstant() == CT::Constant<E>);
   REQUIRE_FALSE(many.IsEncrypted());
   REQUIRE      (many.IsValid());
   REQUIRE_FALSE(many.IsInvalid());
   REQUIRE      (many.IsStatic());
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
void Many_CheckState_DisownedFullConst(const auto& many) {
   using T = Decay<decltype(many)>;

   Many_Helper_TestType<E>(many);

   REQUIRE      (many.IsTypeConstrained() == CT::Typed<T>);
   REQUIRE_FALSE(many.IsCompressed());
   REQUIRE      (many.IsConstant());
   REQUIRE_FALSE(many.IsEncrypted());
   REQUIRE      (many.IsValid());
   REQUIRE_FALSE(many.IsInvalid());
   REQUIRE      (many.IsStatic());
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
void Many_CheckState_Abandoned(const auto& many) {
   REQUIRE_FALSE(many.GetAllocation());
}


void Many_CheckState_ContainsOne(const auto& pack, const auto& e, Allocation* entry = nullptr) {
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

void Many_CheckState_ContainsN(auto n, const auto& pack, const CT::Sparse auto& e, Allocation* entry = nullptr) {
   using T = Deref<decltype(pack)>;
   using E = Deref<decltype(e)>;
   using Count = decltype(n);
   (void)entry;

   REQUIRE(pack.GetCount() == n);
   REQUIRE(pack.GetUses() == 1);
   REQUIRE(pack.GetReserved() >= n);

   for (auto& it : pack)
      REQUIRE(it == e);

   for (Count i = 0; i < n; ++i) {
      REQUIRE(&pack.template As<Deptr<E>>(i) ==  e);
      REQUIRE( pack.template As<Deptr<E>>(i) == *e);
      REQUIRE(*pack.template As<E>(i) == *e);
      REQUIRE( pack.template GetRaw<E>()[i] == e);
      IF_LANGULUS_MANAGED_MEMORY(REQUIRE(pack.GetEntries()[i] == entry));

      if constexpr (T::TypeErased) {
         REQUIRE_THROWS(pack.template As<float>(i) == 0.0f);
         REQUIRE_THROWS(pack.template As<float*>(i) == nullptr);
      }
   }
}

void Many_CheckState_ContainsArray(const auto& pack, const CT::Array auto& e, Allocation* entry = nullptr) {
   using T = Deref<decltype(pack)>;
   using E = Deext<decltype(e)>;
   (void)entry;
   constexpr int n = ExtentOf<decltype(e)>;

   REQUIRE(pack.GetCount() == n);
   REQUIRE(pack.GetUses() == 1);
   REQUIRE(pack.GetReserved() >= n);

   int index = 0;
   for (auto& it : pack)
      REQUIRE(it == e[index++]);
   REQUIRE(index == n);

   for (int i = 0; i < n; ++i) {
      REQUIRE(&pack.template As<Deptr<E>>(i) == e[i]);
      REQUIRE( pack.template As<Deptr<E>>(i) == *e[i]);
      REQUIRE(*pack.template As<E>(i) == *e[i]);
      REQUIRE( pack.template GetRaw<E>()[i] == e[i]);
      IF_LANGULUS_MANAGED_MEMORY(REQUIRE(pack.GetEntries()[i] == entry));

      if constexpr (T::TypeErased) {
         REQUIRE_THROWS(pack.template As<float>(i) == 0.0f);
         REQUIRE_THROWS(pack.template As<float*>(i) == nullptr);
      }
   }
}