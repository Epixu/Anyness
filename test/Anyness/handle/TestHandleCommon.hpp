///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../any/TestAnyCommon.hpp"


namespace doctest
{
   /// MARK: {doctest}                                                        
   /// doctest stringifiers for handles                                       
   template<>
   struct StringMaker<Handle> {
      static String convert(Handle const& value) {
         return toString(static_cast<::std::string>(
            NameOf<Handle>() + "(" + Convert<Text>(value) + ")"
         ));
      }
   };

   template<class T>
   struct StringMaker<THandle<T>> {
      static String convert(THandle<T> const& value) {
         return toString(static_cast<::std::string>(
            NameOf<THandle<T>>() + "(" + Convert<Text>(value) + ")"
         ));
      }
   };
}

/// MARK: TestType                                                            
/// Tests if a container is of a particular type                              
template<class E, CT::Container C> requires CT::NoIntent<C>
void Handle_Helper_TestType(const C& h) {
   Any_Helper_TestType<E>(h);
}

/// MARK: TestSame                                                            
/// Tests if two containers point to the same memory the same way             
template<class LHS, class RHS> requires (CT::Container<LHS, RHS> and CT::NoIntent<LHS, RHS>)
void Handle_Helper_TestSame(const LHS& lhs, const RHS& rhs, bool match_constness = true) {
   Any_Helper_TestSame(lhs, rhs, match_constness);
}

/// MARK: Default                                                             
template<class E, CT::Container C> requires CT::NoIntent<C>
void Handle_CheckState_Default(const C& any/*, bool typed = false*/) {
   if constexpr (CT::Typed<C>) {
      static_assert(Exact<TypeOf<C>, E>);
      Handle_Helper_TestType<E>(any);
   }
   /*else if (not typed) {
      REQUIRE_FALSE(any.IsTyped());
      REQUIRE      (any.GetType() == nullptr);
      REQUIRE_FALSE(any.IsSparse());
      REQUIRE_FALSE(any.IsDeep());
   }*/
   else Handle_Helper_TestType<E>(any);

   REQUIRE      (any.IsDefaultState());
   REQUIRE      (any.IsTypeConstrained()/* == CT::Typed<C>*/);
   REQUIRE_FALSE(any.IsConstant());
   REQUIRE_FALSE(any.IsDisowned());
   REQUIRE_FALSE(any.IsValid());

   if constexpr (requires { any.GetAllocation(); }) {
      REQUIRE   (any.GetAllocation());
      REQUIRE   (any.GetUses() > 0);
   }
   
   if constexpr (requires { any.GetEntries(); }) {
      REQUIRE   (any.GetEntries());
      REQUIRE   (any.GetEntries()[0] == nullptr);
   }

   REQUIRE      (any.IsEmpty());
   REQUIRE      (any.GetCount() == 0);
   REQUIRE_FALSE(any);
   REQUIRE      (not any);
   REQUIRE      (any == C{});

   if constexpr (requires { any.IsMissing(); }
   or            requires { any.IsFuture();  }
   or            requires { any.IsPast();    }) {
      REQUIRE_FALSE(any.IsMissing());
      REQUIRE_FALSE(any.IsFuture());
      REQUIRE_FALSE(any.IsPast());
   }
}

/// MARK: OwnedEmpty                                                          
template<class E, CT::Container C> requires CT::NoIntent<C>
void Handle_CheckState_OwnedEmpty(const C& h) {
   Any_CheckState_OwnedEmpty<E>(h);
}

/// MARK: OwnedFull                                                           
template<class E, CT::Container C> requires CT::NoIntent<C>
void Handle_CheckState_OwnedFull(const C& any) {
   Handle_Helper_TestType<E>(any);

   REQUIRE      (any.IsTypeConstrained()/* == CT::Typed<C>*/);
   REQUIRE      (any.IsConstant() == CT::Constant<E>);
   REQUIRE      (any.IsValid());

   if constexpr (requires { any.GetAllocation(); }) {
      REQUIRE   (any.GetAllocation());
      REQUIRE   (any.GetUses() > 0);
   }
   
   if constexpr (requires { any.GetEntries(); }) {
      if constexpr (CT::Sparse<E>)
         REQUIRE(any.GetEntries());
      else
         REQUIRE(any.GetEntries() == nullptr);
   }

   REQUIRE_FALSE(any.IsDisowned());
   REQUIRE_FALSE(any.IsEmpty());
   REQUIRE      (any.GetCount() > 0);

   REQUIRE      (any.GetRaw());
   REQUIRE      (any);
   REQUIRE_FALSE(not any);
   REQUIRE      (any != C{});
}

/// MARK: DisownedFull                                                        
template<class E, CT::Container C> requires CT::NoIntent<C>
void Handle_CheckState_DisownedFull(const C& h) {
   Any_CheckState_DisownedFull<E>(h);
}

/// MARK: Abandoned                                                           
template<class E, CT::Container C> requires CT::NoIntent<C>
void Handle_CheckState_Abandoned(const C& h) {
   Any_CheckState_Abandoned<E>(h);
}

/// MARK: ContainsOne                                                         
template<CT::Container T, CT::Intent I> requires CT::NoIntent<T>
void Handle_CheckState_ContainsOne(T const& h, I&& e_with_intent, int uses = 1) {
   Any_CheckState_ContainsOne(h, e_with_intent, uses);
}

/// MARK: CompareOne                                                          
template<CT::Container T, class E> requires CT::NoIntent<T>
void Handle_Helper_CompareOne(const T& h, const E& e) {
   Any_Helper_CompareOne(h, e);
}
