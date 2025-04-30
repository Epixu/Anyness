#pragma once
#include "Meta.hpp"
#include "Intent.hpp"


namespace Langulus::RTTI
{
   class DefinitionTag;

   namespace Inner
   {
      
      /// A naked pointer to a definition. Probably the fastest, but most     
      /// memory-inefficient on 64bit systems                                 
      struct MetaTagNaked {
      private:
         const DefinitionTag* mDefinition = nullptr;

      public:
         constexpr MetaTagNaked() noexcept = default;
         constexpr MetaTagNaked(const DefinitionTag* definition) noexcept
            : mDefinition {definition} {}

         explicit operator bool() const noexcept {
            return mDefinition != nullptr;
         }

         template<class, class...>
         bool IsExact() const noexcept;
         bool IsExact(const MetaTagNaked&) const noexcept;

         /// Compare if two tags match exactly                                
         bool operator == (const MetaTagNaked& rhs) const noexcept {
            return IsExact(rhs);
         }
      };

   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      using MetaTagBase = MetaPacked<2>;
   #else
      using MetaTagBase = MetaTagNaked;
   #endif

   } // namespace Langulus::RTTI::Inner


   ///                                                                        
   ///   Tag ID                                                               
   ///                                                                        
   /// Can be a naked pointer to a definition, or packed to a smaller size    
   ///                                                                        
   struct MetaTag : Inner::MetaTagBase {
      using CTTI_POD      = Yes;
      using CTTI_Nullable = Yes;

      constexpr MetaTag() noexcept = default;
      constexpr MetaTag(const MetaTag&) noexcept = default;
      constexpr MetaTag(MetaTag&&) noexcept = default;

      constexpr MetaTag(::std::nullptr_t) noexcept {}
      constexpr MetaTag(const DefinitionTag* definition) noexcept
         : Inner::MetaTagBase {definition} {}
      constexpr MetaTag(Cloned<MetaTag>&& meta) noexcept
         : MetaTag {*meta} {}
   };

   using TMeta = MetaTag;

} // namespace Langulus::RTTI