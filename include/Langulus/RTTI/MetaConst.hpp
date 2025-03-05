#pragma once
#include "DefinitionConst.hpp"


namespace Langulus::RTTI
{

   ///                                                                        
   ///   Constant ID                                                          
   ///                                                                        
   /// Can be a naked pointer to a definition, or packed to a smaller size    
   /// - all this is configurable.                                            
   ///                                                                        
   struct MetaConst : DefinitionConst::Handle {
      constexpr MetaConst() noexcept = default;
      constexpr MetaConst(::std::nullptr_t) noexcept {}

      LANGULUS(ALWAYS_INLINED)
      constexpr MetaConst(const DefinitionConst* definition) noexcept {
         if (not definition)
            return;
         DefinitionData::Handle::operator = (definition->mHandle);
      }
   };

} // namespace Langulus::RTTI