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

   auto MetaVerbNaked::GetPositiveName() const noexcept -> Token {
      return mDefinition->mNameOf;
   }

   auto MetaVerbNaked::GetNegativeName() const noexcept -> Token {
      return mDefinition->mNameOfReverse;
   }

   auto MetaVerbNaked::GetPositiveOperator() const noexcept -> Token {
      return mDefinition->mOperator;
   }

   auto MetaVerbNaked::GetNegativeOperator() const noexcept -> Token {
      return mDefinition->mOperatorReverse;
   }

   constexpr bool MetaVerbNaked::IsReversible() const noexcept {
      return not mDefinition->mNameOfReverse.empty();
   }

   constexpr bool MetaVerbNaked::IsConstant() const noexcept {
      return mDefinition->mDefaultInvocationMutable == nullptr;
   }

   constexpr bool MetaVerbNaked::IsMutable() const noexcept {
      return mDefinition->mDefaultInvocationMutable != nullptr;
   }

   constexpr bool MetaVerbNaked::IsDefaultable() const noexcept {
      return mDefinition->mDefaultInvocationMutable  != nullptr
          or mDefinition->mDefaultInvocationConstant != nullptr;
   }

   constexpr bool MetaVerbNaked::IsStateless() const noexcept {
      return mDefinition->mStatelessInvocation != nullptr;
   }

} // namespace Langulus::RTTI::Inner