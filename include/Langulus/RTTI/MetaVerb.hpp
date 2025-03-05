#pragma once
#include "DefinitionVerb.hpp"


namespace Langulus::RTTI
{
#if LANGULUS_FEATURE(MANAGED_REFLECTION)
   namespace Inner
   {

      /// Relies on the definition limits to pack an ID into the smallest     
      /// possible space, but also uses some additional bits to encode some   
      /// often used information about the definition. The handle still has   
      /// to be transformed into a pointer for more advanced uses, but in     
      /// general it is likely to avoid an indirection altogether at the      
      /// cost of a bitwise operation, making it a bit more cache-friendly,   
      /// and worth experimenting with                                        
      struct MetaVerbStructured_8_8 : MetaPacked<1> {

      };

      struct MetaVerbStructured_16_8 : MetaPacked<2> {

      };

      struct MetaVerbStructured_24_8 : MetaPacked<3> {

      };

   } // namespace Langulus::RTTI::Inner
#endif

   ///                                                                        
   ///   Verb type ID                                                         
   ///                                                                        
   /// Can be a naked pointer to a definition, or a structured ID that is     
   /// either packed to a smaller size, or carry a lot of meta information    
   /// in the ID itself to avoid indirection - all this is configurable.      
   ///                                                                        
   struct MetaVerb : DefinitionVerb::Handle {
      constexpr MetaVerb() noexcept = default;
      constexpr MetaVerb(::std::nullptr_t) noexcept {}

      LANGULUS(ALWAYS_INLINED)
      constexpr MetaVerb(const DefinitionVerb* definition) noexcept {
         if (not definition)
            return;
         DefinitionVerb::Handle::operator = (definition->mHandle);
      }
   };

} // namespace Langulus::RTTI