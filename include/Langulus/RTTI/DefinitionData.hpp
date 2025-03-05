#pragma once
#include "Definition.hpp"
#include "Meta.hpp"


namespace Langulus::RTTI
{

   struct MetaData;
   using DMeta = MetaData;
   

   ///                                                                        
   /// A data definition                                                      
   ///                                                                        
   class DefinitionData : public Inner::Definition {
   public:
   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      using Handle = Inner::MetaPacked<2>;
   #else
      using Handle = Inner::MetaNaked;
   #endif

   protected:
      friend struct MetaData;
      DefinitionData(const Token& cppname) : Definition {cppname} {}

      // A unique handle that may or may not be compressed              
      Handle mHandle;

   public:
      template<CT::Decayed>
      static DMeta Reflect();
   };

} // namespace Langulus::RTTI

#include "DefinitionData.inl"