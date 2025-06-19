///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Definition.hpp"


namespace Langulus::RTTI
{

   ///                                                                        
   /// A constant value definition                                            
   ///                                                                        
   class DefinitionConst : public Inner::Definition {
   protected:
      friend struct Inner::MetaConstNaked;
      friend struct Inner::MetaConstPacked_16;

      DefinitionConst(const Token& cppname) : Definition {cppname} {}

   public:
      template<auto>
      static auto Reflect() -> DefinitionConst const*;
   };

} // namespace Langulus::RTTI

#include "DefinitionConst.inl"