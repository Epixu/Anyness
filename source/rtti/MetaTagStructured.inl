///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once

#if not LANGULUS_FEATURE(MANAGED_REFLECTION)
   #error "This file shouldn't be included if MANAGED_REFLECTION feature is disabled"
#endif


namespace Langulus::RTTI::Inner
{
   constexpr MetaTagPacked_16::MetaTagPacked_16(nullptr_t) noexcept
      : Base {0} {}

   constexpr MetaTagPacked_16::MetaTagPacked_16(DefinitionTag const* d) noexcept
      : Base {d ? d->mID : 0} {}

   constexpr auto MetaTagPacked_16::operator = (nullptr_t)
   noexcept -> MetaTagPacked_16& {
      Base::operator = (0);
      return *this;
   }

   constexpr auto MetaTagPacked_16::operator = (DefinitionTag const* d)
   noexcept -> MetaTagPacked_16& {
      Base::operator = (d ? d->mID : 0);
      return *this;
   }

   constexpr bool MetaTagPacked_16::operator == (const MetaTagPacked_16& rhs) const noexcept {
      return Base::operator == (rhs);
   }
   
   /// Get the tag definition                                                 
   inline auto MetaTagPacked_16::GetDefinition() const noexcept -> DefinitionTag const* {
      return Instance.GetMetaTagByID(GetID());
   }
 
   /// Get the name of the tag, the result of NameOf                          
   inline auto MetaTagPacked_16::GetName() const noexcept -> Token {
      return GetDefinition()->mNameOf;
   }

   /// Get the info of the tag, the result of InfoOf                          
   inline auto MetaTagPacked_16::GetInfo() const noexcept -> Token {
      return GetDefinition()->mInfoOf;
   }

   /// Get the major version                                                  
   inline auto MetaTagPacked_16::GetVersionMajor()  const noexcept -> unsigned {
      return GetDefinition()->mVersionMajor;
   }

   /// Get the minor version                                                  
   inline auto MetaTagPacked_16::GetVersionMinor()  const noexcept -> unsigned {
      return GetDefinition()->mVersionMinor;
   }
}
