#pragma once
#include "Definition.hpp"
#include "Meta.hpp"


namespace Langulus::RTTI
{

   /// Type used as a handle for a trait definition                           
   struct MetaTrait;
   using TMeta = MetaTrait;
   

   ///                                                                        
   /// Trait definition                                                       
   ///                                                                        
   class DefinitionTrait : public Inner::Definition {
   public:
   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      using Handle = Inner::MetaPacked<2>;
   #else
      using Handle = Inner::MetaNaked;
   #endif

   protected:
      friend struct MetaTrait;
      DefinitionTrait(const Token& cppname) : Definition {cppname} {}

      // A unique handle that may or may not be compressed              
      Handle mHandle;

   public:
      template<CT::Decayed>
      static TMeta Reflect();
   };

} // namespace Langulus::RTTI

#include "DefinitionTrait.inl"