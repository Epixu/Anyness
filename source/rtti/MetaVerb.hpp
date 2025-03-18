#pragma once
#include "Meta.hpp"


namespace Langulus::RTTI
{
   class DefinitionVerb;

   namespace Inner
   {
   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
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
   #endif

      /// A naked pointer to a definition. Probably the fastest, but most     
      /// memory-inefficient on 64bit systems                                 
      struct MetaVerbNaked {
      private:
         const DefinitionVerb* mDefinition;

      public:
         template<class, class...>
         bool IsExact() const noexcept;
         bool IsExact(const MetaVerbNaked&) const noexcept;

         /// Compare if two tags match exactly                                
         bool operator == (const MetaVerbNaked& rhs) const noexcept {
            return IsExact(rhs);
         }
      };

   } // namespace Langulus::RTTI::Inner


   ///                                                                        
   ///   Verb type ID                                                         
   ///                                                                        
   /// Can be a naked pointer to a definition, or a structured ID that is     
   /// either packed to a smaller size, or carry a lot of meta information    
   /// in the ID itself to avoid indirection - all this is configurable.      
   ///                                                                        
   struct MetaVerb 
   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      : Inner::MetaVerbStructured_8_8
   #else
      : Inner::MetaVerbNaked
   #endif
   {
      using CTTI_POD      = Yes;
      using CTTI_Nullable = Yes;

      constexpr MetaVerb() noexcept = default;
      constexpr MetaVerb(::std::nullptr_t) noexcept {}
      constexpr MetaVerb(const DefinitionVerb*) noexcept;
   };

   using VMeta = MetaVerb;

} // namespace Langulus::RTTI