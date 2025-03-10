#pragma once
#include "Definition.hpp"
#include "MetaTag.hpp"


namespace Langulus::RTTI
{

   /// Type used as a handle for a tag definition                             
   struct MetaTag;
   using TMeta = MetaTag;
   

   ///                                                                        
   /// Tag definition                                                         
   ///                                                                        
   class DefinitionTag : public Inner::Definition {
   protected:
      friend struct MetaTag;
      DefinitionTag(const Token& cppname) : Definition {cppname} {}

      // A unique handle that may or may not be compressed              
      TMeta mHandle;

   public:
      template<CT::Decayed>
      static TMeta Reflect();
   };

} // namespace Langulus::RTTI

#include "DefinitionTag.inl"