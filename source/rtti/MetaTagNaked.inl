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

   /// Get the name of the tag, the result of NameOf                          
   inline auto MetaTagNaked::GetName() const noexcept -> Token {
      return mDefinition->mNameOf;
   }

} // namespace Langulus::RTTI::Inner