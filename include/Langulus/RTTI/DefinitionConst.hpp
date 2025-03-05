#pragma once
#include "Definition.hpp"
#include "Meta.hpp"


namespace Langulus::RTTI
{

   struct MetaConst;
   using CMeta = MetaConst;
   

   ///                                                                        
   /// A constant value definition                                            
   ///                                                                        
   class DefinitionConst : public Inner::Definition {
   public:
   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      using Handle = Inner::MetaPacked<2>;
   #else
      using Handle = Inner::MetaNaked;
   #endif

   protected:
      friend struct MetaConst;
      DefinitionConst(const Token& cppname) : Definition {cppname} {}

      // A unique handle that may or may not be compressed              
      Handle mHandle;

   public:
      template<auto>
      static CMeta Reflect();
   };

} // namespace Langulus::RTTI

#include "DefinitionConst.inl"