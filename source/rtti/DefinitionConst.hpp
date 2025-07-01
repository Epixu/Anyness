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
   class DefinitionConst final : public Inner::Definition {
   protected:
      friend class Registry;
      friend struct Inner::MetaConstNaked;
      friend struct Inner::MetaConstPacked_16;

      // The type of the constant                                       
      DefinitionData const* mType IF_SAFE(= nullptr);
      // A pointer to an instance of the constant on the heap           
      void const* mData IF_SAFE(= nullptr);
      
      DefinitionConst(const Token& cppname, const Token& boundary)
         : Definition {cppname, boundary} {}

      ~DefinitionConst() override {
         free(const_cast<void*>(mData));
      }
      
   public:
      template<auto>
      static auto Reflect() -> DefinitionConst const*;
   };

} // namespace Langulus::RTTI

#include "DefinitionConst.inl"
