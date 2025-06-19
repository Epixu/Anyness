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
      struct MetaConstPacked_16 : MetaPacked<DefinitionConst, 2> {
         using Base = MetaPacked<DefinitionConst, 2>;

         constexpr MetaConstPacked_16() noexcept = default;
         constexpr MetaConstPacked_16(MetaConstPacked_16 const&) noexcept = default;
         constexpr MetaConstPacked_16(MetaConstPacked_16&&) noexcept = default;
         constexpr MetaConstPacked_16(::std::nullptr_t) noexcept;
         constexpr MetaConstPacked_16(DefinitionConst const*) noexcept;

         constexpr MetaConstPacked_16& operator = (MetaConstPacked_16 const&) noexcept = default;
         constexpr MetaConstPacked_16& operator = (MetaConstPacked_16&&) noexcept = default;
         constexpr MetaConstPacked_16& operator = (::std::nullptr_t) noexcept;
         constexpr MetaConstPacked_16& operator = (DefinitionConst const*) noexcept;

         auto GetName() const noexcept -> Token;
      };
   #endif

      /// A naked pointer to a definition. Probably the fastest, but most     
      /// memory-inefficient on 64bit systems                                 
      struct MetaConstNaked : MetaNaked<DefinitionConst> {
         using MetaNaked<DefinitionConst>::MetaNaked;
         using MetaNaked<DefinitionConst>::operator =;
         using MetaNaked<DefinitionConst>::operator bool;

         auto GetName() const noexcept -> Token;
      };

   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      using MetaConstBase = MetaConstPacked_16;
   #else
      using MetaConstBase = MetaConstNaked;
   #endif

   } // namespace Langulus::RTTI::Inner


   ///                                                                        
   ///   Constant ID                                                          
   ///                                                                        
   /// Can be a naked pointer to a definition, or a structured ID that is     
   /// either packed to a smaller size, or carry a lot of meta information    
   /// in the ID itself to avoid indirection                                  
   ///                                                                        
   struct MetaConst : Inner::MetaConstBase {
      using CTTI_POD      = Yes;
      using CTTI_Nullable = Yes;

      ignore_all_intents(MetaConst);

      using Inner::MetaConstBase::MetaConstBase;
      using Inner::MetaConstBase::operator =;
      using Inner::MetaConstBase::operator bool;
   };

   using CMeta = MetaConst;

} // namespace Langulus::RTTI