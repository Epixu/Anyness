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

#if LANGULUS_FEATURE(MANAGED_REFLECTION)
   /// Get the active boundaries of the verb                                  
   inline auto MetaVerbNaked::GetBoundaries()
   const noexcept -> Definition::BoundarySet const& {
      return mDefinition->mBoundaries;
   }
#endif

   constexpr bool MetaVerbNaked::IsReversible() const noexcept {
      return not mDefinition->mNameOfReverse.empty();
   }

   constexpr bool MetaVerbNaked::IsConstant() const noexcept {
      return not mDefinition->mCurrentBoundary.mDefaultMut;
   }

   constexpr bool MetaVerbNaked::IsMutable() const noexcept {
      return mDefinition->mCurrentBoundary.mDefaultMut;
   }

   constexpr bool MetaVerbNaked::IsDefaultable() const noexcept {
      return mDefinition->mCurrentBoundary.mDefaultMut
          or mDefinition->mCurrentBoundary.mDefault;
   }

   constexpr bool MetaVerbNaked::IsStateless() const noexcept {
      return mDefinition->mCurrentBoundary.mStateless;
   }

} // namespace Langulus::RTTI::Inner
