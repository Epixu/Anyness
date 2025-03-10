#pragma once
#include "Definition.hpp"
#include "MetaConst.hpp"


namespace Langulus::RTTI
{

   ///                                                                        
   /// A constant value definition                                            
   ///                                                                        
   class DefinitionConst : public Inner::Definition {
   protected:
      friend struct MetaConst;
      DefinitionConst(const Token& cppname) : Definition {cppname} {}

      // A unique handle that may or may not be compressed              
      MetaConst mHandle;

   public:
      template<auto>
      static CMeta Reflect();
   };

} // namespace Langulus::RTTI

#include "DefinitionConst.inl"