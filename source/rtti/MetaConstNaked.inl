///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "MetaConst.hpp"


namespace Langulus::RTTI::Inner
{

   /// Get the name of the constant, the result of NameOf                     
   inline auto MetaConstNaked::GetName() const noexcept -> Token {
      return mDefinition->mToken;
   }

} // namespace Langulus::RTTI::Inner