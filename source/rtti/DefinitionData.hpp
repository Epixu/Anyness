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
      // A unique handle that may or may not be compressed              
      MetaData mHandle;

   public:
      friend struct MetaData;
      DefinitionData(const Token& cppname) : Definition {cppname} {}

      template<CT::Decayed>
      static DMeta Reflect();
      template<CT::NotDecayed>
      static DMeta Reflect();
   };

} // namespace Langulus::RTTI

#include "DefinitionData.inl"