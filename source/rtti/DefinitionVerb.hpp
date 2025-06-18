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
   /// A Verb definition                                                      
   ///                                                                        
   class DefinitionVerb : public Inner::Definition {
   protected:
      friend struct MetaVerb;
      DefinitionVerb(const Token& cppname) : Definition {cppname} {}

   public:
      template<CT::Decayed>
      static auto Reflect() -> DefinitionVerb const*;
   };

} // namespace Langulus::RTTI

#include "DefinitionVerb.inl"