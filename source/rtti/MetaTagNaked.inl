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

#if LANGULUS_FEATURE(MANAGED_REFLECTION)
   /// Get the active boundaries of the tag                                   
   inline auto MetaTagNaked::GetBoundaries()
   const noexcept -> Definition::BoundarySet const& {
      return mDefinition->mBoundaries;
   }
#endif

} // namespace Langulus::RTTI::Inner