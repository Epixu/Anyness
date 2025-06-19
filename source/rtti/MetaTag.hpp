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
      /// Relies on the definition limits to pack an ID into the smallest     
      /// possible space, but also uses some additional bits to encode some   
      /// often used information about the definition. The handle still has   
      /// to be transformed into a pointer for more advanced uses, but in     
      /// general it is likely to avoid an indirection altogether at the      
      /// cost of a bitwise operation, making it a bit more cache-friendly,   
      /// and worth experimenting with                                        
      ///                                                                     
      struct MetaTagPacked_16 : MetaPacked<DefinitionTag, 2> {
         using Base = MetaPacked<DefinitionTag, 2>;

         constexpr MetaTagPacked_16() noexcept = default;
         constexpr MetaTagPacked_16(MetaTagPacked_16 const&) noexcept = default;
         constexpr MetaTagPacked_16(MetaTagPacked_16&&) noexcept = default;
         constexpr MetaTagPacked_16(::std::nullptr_t) noexcept;
         constexpr MetaTagPacked_16(DefinitionTag const*) noexcept;

         constexpr MetaTagPacked_16& operator = (MetaTagPacked_16 const&) noexcept = default;
         constexpr MetaTagPacked_16& operator = (MetaTagPacked_16&&) noexcept = default;
         constexpr MetaTagPacked_16& operator = (::std::nullptr_t) noexcept;
         constexpr MetaTagPacked_16& operator = (DefinitionTag const*) noexcept;

         auto GetName() const noexcept -> Token;
      };
   #endif
      
      /// A naked pointer to a definition. Probably the fastest, but most     
      /// memory-inefficient on 64bit systems                                 
      struct MetaTagNaked : MetaNaked<DefinitionTag> {
         using MetaNaked<DefinitionTag>::MetaNaked;
         using MetaNaked<DefinitionTag>::operator =;
         using MetaNaked<DefinitionTag>::operator bool;

         auto GetName() const noexcept -> Token;
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