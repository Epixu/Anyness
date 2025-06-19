///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "MetaConst.hpp"


namespace Langulus::RTTI::Inner
{

   constexpr MetaConstPacked_16::MetaConstPacked_16(::std::nullptr_t) noexcept
      : Base {0} {}

   constexpr MetaConstPacked_16::MetaConstPacked_16(DefinitionConst const* definition) noexcept
      : Base {definition ? definition->mID : 0} {}

   constexpr MetaConstPacked_16& MetaConstPacked_16::operator = (::std::nullptr_t) noexcept {
      Base::operator = (0);
      return *this;
   }

   constexpr MetaConstPacked_16& MetaConstPacked_16::operator = (DefinitionConst const* definition) noexcept {
      Base::operator = (definition ? definition->mID : 0);
      return *this;
   }

   /// Get the name of the tag, the result of NameOf                          
   auto MetaConstPacked_16::GetName() const noexcept -> Token {
      return Instance.GetMetaConst(*this)->mToken;
   }

} // namespace Langulus::RTTI::Inner