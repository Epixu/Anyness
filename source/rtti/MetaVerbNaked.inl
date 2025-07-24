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
   /// Get the positive verb token                                            
   inline auto MetaVerbNaked::GetPositiveName() const noexcept -> Token {
      return mDefinition->mNameOf;
   }

   /// Get the negative verb token, a.k.a. the antonym                        
   inline auto MetaVerbNaked::GetNegativeName() const noexcept -> Token {
      return mDefinition->mNameOfReverse;
   }

   /// Get the positive reflected operator token                              
   inline auto MetaVerbNaked::GetPositiveOperator() const noexcept -> Token {
      return mDefinition->mOperator;
   }

   /// Get the negative reflected operator token                              
   inline auto MetaVerbNaked::GetNegativeOperator() const noexcept -> Token {
      return mDefinition->mOperatorReverse;
   }

   /// Get the default reflected precedence for the verb                      
   inline auto MetaVerbNaked::GetPrecedence() const noexcept -> float {
      return mDefinition->mPrecedence;
   }

   /// Get the contextless execution routine if such was defined              
   inline auto MetaVerbNaked::GetContextless() const noexcept -> DefinitionVerb::FContextless {
      return mDefinition->mCurrentBoundary.mContextless;
   }

   /// Check if the verb has a negative token defined                         
   constexpr bool MetaVerbNaked::IsReversible() const noexcept {
      return not mDefinition->mNameOfReverse.empty();
   }

   /// Check if the verb provides a contextless execution routine             
   constexpr bool MetaVerbNaked::IsContextless() const noexcept {
      return mDefinition->mCurrentBoundary.mContextless;
   }
}
