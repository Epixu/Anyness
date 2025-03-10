#pragma once
#include "Definition.hpp"
#include "MetaVerb.hpp"


namespace Langulus::RTTI
{  

   ///                                                                        
   /// A Verb definition                                                      
   ///                                                                        
   class DefinitionVerb : public Inner::Definition {
   protected:
      friend struct MetaVerb;
      DefinitionVerb(const Token& cppname) : Definition {cppname} {}

      // A unique handle that may or may not be compressed              
      VMeta mHandle;

   public:
      template<CT::Decayed>
      static VMeta Reflect();
   };

} // namespace Langulus::RTTI

#include "DefinitionVerb.inl"