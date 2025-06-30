///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "MetaTag.hpp"


namespace Langulus::RTTI::Inner
{

   constexpr MetaTagPacked_16::MetaTagPacked_16(::std::nullptr_t) noexcept
      : Base {0} {}

   constexpr MetaTagPacked_16::MetaTagPacked_16(DefinitionTag const* definition) noexcept
      : Base {definition ? definition->mID : 0} {}

   constexpr MetaTagPacked_16& MetaTagPacked_16::operator = (::std::nullptr_t) noexcept {
      Base::operator = (0);
      return *this;
   }

   constexpr MetaTagPacked_16& MetaTagPacked_16::operator = (DefinitionTag const* definition) noexcept {
      Base::operator = (definition ? definition->mID : 0);
      return *this;
   }

   /// Get the name of the tag, the result of NameOf                          
   inline auto MetaTagPacked_16::GetName() const noexcept -> Token {
      return Instance.GetMetaTag(*this)->mToken;
   }

} // namespace Langulus::RTTI::Inner