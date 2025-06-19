///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "MetaTag.hpp"


namespace Langulus::RTTI::Inner
{

   /// Check if type origins match                                            
   /// Disregards all cv-qualifiers, pointers, array extents, etc.            
   ///   @param other - the type to compare against                           
   ///   @return true if types match                                          
   bool MetaTagNaked::Is(const MetaTagNaked& other) const noexcept {
      return mDefinition->mOrigin and other
         and mDefinition->mOrigin == other.mDefinition->mOrigin;
   }
   
   /// Get the name of the type, the result of NameOf                         
   auto MetaTagNaked::GetName() const noexcept -> Token {
      return mDefinition->mToken;
   }

} // namespace Langulus::RTTI::Inner