///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once


namespace Langulus::RTTI::Inner
{
   template<unsigned ID_SIZE>
   constexpr MetaVerbStructured_X8<ID_SIZE>::MetaVerbStructured_X8(nullptr_t) noexcept
      : Base {0} {}

   template<unsigned ID_SIZE>
   constexpr MetaVerbStructured_X8<ID_SIZE>::MetaVerbStructured_X8(DefinitionVerb const* d) noexcept
      : Base {d ? d->mID : 0} {}

   template<unsigned ID_SIZE>
   constexpr auto MetaVerbStructured_X8<ID_SIZE>::operator = (nullptr_t)
   noexcept -> MetaVerbStructured_X8& {
      Base::operator = (0);
      return *this;
   }

   template<unsigned ID_SIZE>
   constexpr auto MetaVerbStructured_X8<ID_SIZE>::operator = (DefinitionVerb const* d)
   noexcept -> MetaVerbStructured_X8& {
      Base::operator = (d ? d->mID : 0);
      return *this;
   }

   template<unsigned ID_SIZE>
   auto MetaVerbStructured_X8<ID_SIZE>::GetInfo() const noexcept -> Token {
      return Instance.GetMetaVerbByID(*this)->mInfoOf;
   }
   
   template<unsigned ID_SIZE>
   auto MetaVerbStructured_X8<ID_SIZE>::GetPositiveName() const noexcept -> Token {
      return Instance.GetMetaVerbByID(*this)->mNameOf;
   }

   template<unsigned ID_SIZE>
   auto MetaVerbStructured_X8<ID_SIZE>::GetNegativeName() const noexcept -> Token {
      return Instance.GetMetaVerbByID(*this)->mNameOfReverse;
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
   auto MetaVerbStructured_X8<ID_SIZE>::GetVersionMajor()  const noexcept -> unsigned {
      return Instance.GetMetaVerbByID(*this)->mVersionMajor;
   }

   template<unsigned ID_SIZE>
   auto MetaVerbStructured_X8<ID_SIZE>::GetVersionMinor()  const noexcept -> unsigned {
      return Instance.GetMetaVerbByID(*this)->mVersionMinor;
   }

   template<unsigned ID_SIZE> auto MetaVerbStructured_X8<ID_SIZE>::GetBoundaries()
   const noexcept -> Definition::BoundarySet const& {
      return Instance.GetMetaVerbByID(*this)->mBoundaries;
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
