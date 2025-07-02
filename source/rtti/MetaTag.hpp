///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Meta.hpp"
#include "DefinitionTag.hpp"


namespace Langulus::RTTI
{
   namespace Inner
   {
   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      ///                                                                     
      /// Supports up to 65,535 tags                                          
      ///                                                                     
      #pragma pack(push, 1)
      struct MetaTagPacked_16 : MetaPacked<2> {
         using Base = MetaPacked<2>;

         constexpr MetaTagPacked_16() noexcept = default;
         constexpr MetaTagPacked_16(MetaTagPacked_16 const&) noexcept = default;
         constexpr MetaTagPacked_16(MetaTagPacked_16&&) noexcept = default;
         explicit constexpr MetaTagPacked_16(nullptr_t) noexcept;
         explicit constexpr MetaTagPacked_16(DefinitionTag const*) noexcept;

         constexpr MetaTagPacked_16& operator = (MetaTagPacked_16 const&) noexcept = default;
         constexpr MetaTagPacked_16& operator = (MetaTagPacked_16&&) noexcept = default;
         constexpr MetaTagPacked_16& operator = (nullptr_t) noexcept;
         constexpr MetaTagPacked_16& operator = (DefinitionTag const*) noexcept;

         auto GetName()       const noexcept -> Token;
         auto GetBoundaries() const noexcept -> Definition::BoundarySet const&;
      };
      #pragma pack(pop)
      
      static_assert(sizeof(MetaTagPacked_16) == 2);
   #endif
      
      ///                                                                     
      /// A naked pointer to a definition. Probably the fastest, but most     
      /// memory-inefficient on 64bit systems                                 
      ///                                                                     
      struct MetaTagNaked : MetaNaked<DefinitionTag> {
         using Base = MetaNaked<DefinitionTag>;

         using Base::Base;
         using Base::operator =;
         using Base::operator bool;

         auto GetName() const noexcept -> Token;

         #if LANGULUS_FEATURE(MANAGED_REFLECTION)
            auto GetBoundaries() const noexcept -> Definition::BoundarySet const&;
         #endif
      };

   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      using MetaTagBase = MetaTagPacked_16;
   #else
      using MetaTagBase = MetaTagNaked;
   #endif

   } // namespace Langulus::RTTI::Inner


   ///                                                                        
   ///   Tag ID                                                               
   ///                                                                        
   /// Can be a naked pointer to a definition, or a structured ID that is     
   /// either packed to a smaller size, or carry a lot of meta information    
   /// in the ID itself to avoid indirection                                  
   ///                                                                        
   struct MetaTag : Inner::MetaTagBase {
      using CTTI_POD      = Yes;
      using CTTI_Nullable = Yes;

      ignore_all_intents(MetaTag);

      using Inner::MetaTagBase::MetaTagBase;
      using Inner::MetaTagBase::operator =;
      using Inner::MetaTagBase::operator bool;
   };

   using TMeta = MetaTag;

} // namespace Langulus::RTTI

#if LANGULUS_FEATURE(MANAGED_REFLECTION)
   #include "MetaTagStructured.inl"
#endif

#include "MetaTagNaked.inl"
