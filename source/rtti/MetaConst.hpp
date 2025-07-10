///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Meta.hpp"
#include "DefinitionConst.hpp"


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
      #pragma pack(push, 1)
      struct MetaConstPacked_16 : MetaPacked<2> {
         using Base = MetaPacked;

         constexpr MetaConstPacked_16() noexcept = default;
         constexpr MetaConstPacked_16(MetaConstPacked_16 const&) noexcept = default;
         constexpr MetaConstPacked_16(MetaConstPacked_16&&) noexcept = default;
         constexpr MetaConstPacked_16(nullptr_t) noexcept;
         constexpr MetaConstPacked_16(DefinitionConst const*) noexcept;

         constexpr MetaConstPacked_16& operator = (MetaConstPacked_16 const&) noexcept = default;
         constexpr MetaConstPacked_16& operator = (MetaConstPacked_16&&) noexcept = default;
         constexpr MetaConstPacked_16& operator = (nullptr_t) noexcept;
         constexpr MetaConstPacked_16& operator = (DefinitionConst const*) noexcept;

         auto GetName()          const noexcept -> Token;
         auto GetInfo()          const noexcept -> Token;
         auto GetVersionMajor()  const noexcept -> unsigned;
         auto GetVersionMinor()  const noexcept -> unsigned;
         auto GetBoundaries()    const noexcept -> Definition::BoundarySet const&;

      protected:
         auto GetDefinition()    const noexcept -> DefinitionConst const*;
      };
      #pragma pack(pop)
      
      static_assert(sizeof(MetaConstPacked_16) == 2);
   #endif

      /// A naked pointer to a definition. Probably the fastest, but most     
      /// memory-inefficient on 64bit systems                                 
      struct MetaConstNaked : MetaNaked<DefinitionConst> {
         using MetaNaked::MetaNaked;
         using MetaNaked::operator =;
         using MetaNaked::operator bool;
      };

   } // namespace Langulus::RTTI::Inner

   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      using MetaConst = Inner::MetaConstPacked_16;
   #else
      using MetaConst = Inner::MetaConstNaked;
   #endif

   using CMeta = MetaConst;

} // namespace Langulus::RTTI

#if LANGULUS_FEATURE(MANAGED_REFLECTION)
   #include "MetaConstStructured.inl"
#endif

#include "MetaConstNaked.inl"
