#pragma once
#include "Definition.hpp"
#include "MetaData.hpp"


namespace Langulus::RTTI
{

   ///                                                                        
   /// A data definition                                                      
   ///                                                                        
   class DefinitionData : public Inner::Definition {
   protected:
      friend struct MetaData;
      DefinitionData(const Token& cppname) : Definition {cppname} {}

      // A unique handle that may or may not be compressed              
      MetaData mHandle;

   public:
      template<CT::Decayed>
      static DMeta Reflect();
   };

} // namespace Langulus::RTTI

#include "DefinitionData.inl"