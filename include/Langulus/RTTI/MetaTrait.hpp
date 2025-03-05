#pragma once
#include "DefinitionTrait.hpp"


namespace Langulus::RTTI
{

   ///                                                                        
   ///   Trait type ID                                                        
   ///                                                                        
   /// Can be a naked pointer to a definition, or packed to a smaller size    
   /// - all this is configurable.                                            
   ///                                                                        
   struct MetaTrait : DefinitionTrait::Handle {
      constexpr MetaTrait() noexcept = default;
      constexpr MetaTrait(::std::nullptr_t) noexcept {}

      LANGULUS(ALWAYS_INLINED)
      constexpr MetaTrait(const DefinitionTrait* definition) noexcept {
         if (not definition)
            return;
         DefinitionTrait::Handle::operator = (definition->mHandle);
      }
   };

} // namespace Langulus::RTTI