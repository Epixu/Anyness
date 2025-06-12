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

   struct MetaTag;
   

   ///                                                                        
   /// Tag definition                                                         
   ///                                                                        
   class DefinitionTag : public Inner::Definition {
   protected:
      friend struct MetaTag;
      DefinitionTag(const Token& cppname) : Definition {cppname} {}

   public:
      template<CT::Decayed>
      static auto Reflect() -> DefinitionTag const*;
   };

} // namespace Langulus::RTTI

#include "DefinitionTag.inl"