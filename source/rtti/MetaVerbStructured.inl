///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "MetaVerb.hpp"


namespace Langulus::RTTI::Inner
{
   template<unsigned ID_SIZE>
   constexpr MetaVerbStructured_X8<ID_SIZE>::MetaVerbStructured_X8(::std::nullptr_t) noexcept
      : Base {0} {}

   template<unsigned ID_SIZE>
   constexpr MetaVerbStructured_X8<ID_SIZE>::MetaVerbStructured_X8(DefinitionVerb const* definition) noexcept
      : Base {definition ? definition->mID : 0} {}

   template<unsigned ID_SIZE>
   constexpr MetaVerbStructured_X8<ID_SIZE>& MetaVerbStructured_X8<ID_SIZE>::operator = (::std::nullptr_t) noexcept {
      Base::operator = (0);
      return *this;
   }

   template<unsigned ID_SIZE>
   constexpr MetaVerbStructured_X8<ID_SIZE>& MetaVerbStructured_X8<ID_SIZE>::operator = (DefinitionVerb const* definition) noexcept {
      Base::operator = (definition ? definition->mID : 0);
      return *this;
   }

   template<unsigned ID_SIZE>
   auto MetaVerbStructured_X8<ID_SIZE>::GetPositiveName() const noexcept -> Token {
      return Instance.GetMetaVerbByID(*this)->mToken;
   }

   template<unsigned ID_SIZE>
   auto MetaVerbStructured_X8<ID_SIZE>::GetNegativeName() const noexcept -> Token {
      return Instance.GetMetaVerbByID(*this)->mTokenReverse;
   }

   template<unsigned ID_SIZE>
   auto MetaVerbStructured_X8<ID_SIZE>::GetPositiveOperator() const noexcept -> Token {
      return Instance.GetMetaVerbByID(*this)->mOperator;
   }

   template<unsigned ID_SIZE>
   auto MetaVerbStructured_X8<ID_SIZE>::GetNegativeOperator() const noexcept -> Token {
      return Instance.GetMetaVerbByID(*this)->mOperatorReverse;
   }

   template<unsigned ID_SIZE>
   constexpr bool MetaVerbStructured_X8<ID_SIZE>::IsReversible() const noexcept {
      return reversible;
   }

   template<unsigned ID_SIZE>
   constexpr bool MetaVerbStructured_X8<ID_SIZE>::IsConstant() const noexcept {
      return constant;
   }

   template<unsigned ID_SIZE>
   constexpr bool MetaVerbStructured_X8<ID_SIZE>::IsMutable() const noexcept {
      return not constant;
   }

   template<unsigned ID_SIZE>
   constexpr bool MetaVerbStructured_X8<ID_SIZE>::IsDefaultable() const noexcept {
      return defaultable;
   }

   template<unsigned ID_SIZE>
   constexpr bool MetaVerbStructured_X8<ID_SIZE>::IsStateless() const noexcept {
      return stateless;
   }

} // namespace Langulus::RTTI::Inner