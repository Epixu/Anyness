#pragma once
#include "Meta.hpp"


namespace Langulus::RTTI
{
   class DefinitionTag;

   namespace Inner
   {
      
      /// A naked pointer to a definition. Probably the fastest, but most     
      /// memory-inefficient on 64bit systems                                 
      struct MetaTagNaked {
      private:
         const DefinitionTag* mDefinition;

      public:
         template<class, class...>
         bool IsExact() const noexcept;
         bool IsExact(const MetaTagNaked&) const noexcept;

         /// Compare if two tags match exactly                                
         bool operator == (const MetaTagNaked& rhs) const noexcept {
            return IsExact(rhs);
         }
      };

   } // namespace Langulus::RTTI::Inner


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
      : Inner::MetaTagNaked
   #endif
   {
      constexpr MetaTag() noexcept = default;
      constexpr MetaTag(::std::nullptr_t) noexcept {}
      constexpr MetaTag(const DefinitionTag*) noexcept;
   };

   using TMeta = MetaTag;

} // namespace Langulus::RTTI