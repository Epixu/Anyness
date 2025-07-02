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

   /// Get the name of the constant, the result of NameOf                     
   inline auto MetaConstNaked::GetName() const noexcept -> Token {
      return mDefinition->mNameOf;
   }
   
#if LANGULUS_FEATURE(MANAGED_REFLECTION)
   /// Get the active boundaries of the constant                              
   inline auto MetaConstNaked::GetBoundaries()
   const noexcept -> Definition::BoundarySet const& {
      return mDefinition->mBoundaries;
   }
#endif

} // namespace Langulus::RTTI::Inner
