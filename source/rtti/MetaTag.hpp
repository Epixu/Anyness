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
         constexpr MetaTagNaked(const MetaTagNaked&) noexcept = default;
         constexpr MetaTagNaked(MetaTagNaked&&) noexcept = default;

         constexpr MetaTagNaked(::std::nullptr_t) noexcept {}

         explicit constexpr MetaTagNaked(const DefinitionTag* definition) noexcept
            : mDefinition {definition} {}

         constexpr MetaTagNaked& operator = (const MetaTagNaked&) noexcept = default;
         constexpr MetaTagNaked& operator = (MetaTagNaked&&) noexcept = default;
         constexpr MetaTagNaked& operator = (::std::nullptr_t) noexcept {
            mDefinition = nullptr;
            return *this;
         }
         constexpr MetaTagNaked& operator = (const DefinitionTag* definition) noexcept {
            mDefinition = definition;
            return *this;
         }

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

      using Inner::MetaTagBase::MetaTagBase;
      using Inner::MetaTagBase::operator =;

      template<template<class> class T> requires CT::Intent<T<MetaTag>>
      explicit constexpr MetaTag(T<MetaTag>&& meta) noexcept
         : MetaTag {*meta} {}

      template<template<class> class T> requires CT::Intent<T<MetaTag>>
      constexpr MetaTag& operator = (T<MetaTag>&& rhs) noexcept {
         new (this) MetaTag {*rhs};
         return *this;
      }
   };

   using TMeta = MetaTag;

} // namespace Langulus::RTTI