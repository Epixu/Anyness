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

   inline auto MetaVerbNaked::GetPositiveName() const noexcept -> Token {
      return mDefinition->mNameOf;
   }

   inline auto MetaVerbNaked::GetNegativeName() const noexcept -> Token {
      return mDefinition->mNameOfReverse;
   }

   inline auto MetaVerbNaked::GetPositiveOperator() const noexcept -> Token {
      return mDefinition->mOperator;
   }

   inline auto MetaVerbNaked::GetNegativeOperator() const noexcept -> Token {
      return mDefinition->mOperatorReverse;
   }

   constexpr bool MetaVerbNaked::IsReversible() const noexcept {
      return not mDefinition->mNameOfReverse.empty();
   }

   constexpr bool MetaVerbNaked::IsContextless() const noexcept {
      return mDefinition->mCurrentBoundary.mContextless;
   }

} // namespace Langulus::RTTI::Inner
