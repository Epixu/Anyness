#pragma once
#include "Definition.hpp"
#include "Meta.hpp"


namespace Langulus::RTTI
{

   struct MetaVerb;
   using VMeta = MetaVerb;
   

   ///                                                                        
   /// A Verb definition                                                      
   ///                                                                        
   class DefinitionVerb : public Inner::Definition {
   public:
   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      using Handle = Inner::MetaPacked<1>;
   #else
      using Handle = Inner::MetaNaked;
   #endif

   protected:
      friend struct MetaVerb;
      DefinitionVerb(const Token& cppname) : Definition {cppname} {}

      // A unique handle that may or may not be compressed              
      Handle mHandle;

   public:
      template<CT::Decayed>
      static VMeta Reflect();
   };

} // namespace Langulus::RTTI

#include "DefinitionVerb.inl"