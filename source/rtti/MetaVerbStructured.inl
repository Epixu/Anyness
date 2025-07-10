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
   constexpr bool MetaVerbStructured_X8<ID_SIZE>::operator == (const MetaVerbStructured_X8& rhs) const noexcept {
      return Base::operator == (rhs);
   }

   /// Get the tag definition                                                 
   template<unsigned ID_SIZE>
   auto MetaVerbStructured_X8<ID_SIZE>::GetDefinition() const noexcept -> DefinitionVerb const* {
      return Instance.GetMetaVerbByID(Base::GetID());
   }
   
   template<unsigned ID_SIZE>
   auto MetaVerbStructured_X8<ID_SIZE>::GetInfo() const noexcept -> Token {
      return GetDefinition()->mInfoOf;
   }
   
   template<unsigned ID_SIZE>
   auto MetaVerbStructured_X8<ID_SIZE>::GetVersionMajor()  const noexcept -> unsigned {
      return GetDefinition()->mVersionMajor;
   }

   template<unsigned ID_SIZE>
   auto MetaVerbStructured_X8<ID_SIZE>::GetVersionMinor()  const noexcept -> unsigned {
      return GetDefinition()->mVersionMinor;
   }
   
   template<unsigned ID_SIZE> auto MetaVerbStructured_X8<ID_SIZE>::GetBoundaries()
   const noexcept -> Definition::BoundarySet const& {
      return GetDefinition()->mBoundaries;
   }


   template<unsigned ID_SIZE>
   auto MetaVerbStructured_X8<ID_SIZE>::GetPositiveName() const noexcept -> Token {
      return GetDefinition()->mNameOf;
   }

   template<unsigned ID_SIZE>
   auto MetaVerbStructured_X8<ID_SIZE>::GetNegativeName() const noexcept -> Token {
      return GetDefinition()->mNameOfReverse;
   }

   template<unsigned ID_SIZE>
   auto MetaVerbStructured_X8<ID_SIZE>::GetPositiveOperator() const noexcept -> Token {
      return GetDefinition()->mOperator;
   }

   template<unsigned ID_SIZE>
   auto MetaVerbStructured_X8<ID_SIZE>::GetNegativeOperator() const noexcept -> Token {
      return GetDefinition()->mOperatorReverse;
   }

   template<unsigned ID_SIZE>
   auto MetaVerbStructured_X8<ID_SIZE>::GetPrecedence() const noexcept -> float {
      return GetDefinition()->mPrecedence;
   }

   template<unsigned ID_SIZE>
   auto MetaVerbStructured_X8<ID_SIZE>::GetContextless() const noexcept -> DefinitionVerb::FContextless {
      return contextless ? GetDefinition()->mCurrentBoundary.mContextless : nullptr;
   }

   
   template<unsigned ID_SIZE>
   constexpr bool MetaVerbStructured_X8<ID_SIZE>::IsReversible() const noexcept {
      return reversible;
   }

   template<unsigned ID_SIZE>
   constexpr bool MetaVerbStructured_X8<ID_SIZE>::IsContextless() const noexcept {
      return contextless;
   }
}
