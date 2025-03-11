#pragma once
#include "Meta.hpp"


namespace Langulus::RTTI
{

   class DefinitionConst;

   ///                                                                        
   ///   Constant ID                                                          
   ///                                                                        
   /// Can be a naked pointer to a definition, or packed to a smaller size    
   /// - all this is configurable.                                            
   ///                                                                        
   struct MetaConst
   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      : Inner::MetaPacked<2>
   #else
      : Inner::MetaNaked<DefinitionConst>
   #endif
   {
      constexpr MetaConst() noexcept = default;
      constexpr MetaConst(::std::nullptr_t) noexcept {}
      constexpr MetaConst(const DefinitionConst*) noexcept;
   };

   using CMeta = MetaConst;

} // namespace Langulus::RTTI