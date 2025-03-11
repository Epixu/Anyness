#pragma once
#include "Meta.hpp"


namespace Langulus::RTTI
{

   class DefinitionTag;

   ///                                                                        
   ///   Tag ID                                                               
   ///                                                                        
   /// Can be a naked pointer to a definition, or packed to a smaller size    
   /// - all this is configurable.                                            
   ///                                                                        
   struct MetaTag 
   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      : Inner::MetaPacked<2>
   #else
      : Inner::MetaNaked<DefinitionTag>
   #endif
   {
      constexpr MetaTag() noexcept = default;
      constexpr MetaTag(::std::nullptr_t) noexcept {}
      constexpr MetaTag(const DefinitionTag*) noexcept;
   };

   using TMeta = MetaTag;

} // namespace Langulus::RTTI