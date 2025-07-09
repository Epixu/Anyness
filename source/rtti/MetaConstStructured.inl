///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once


namespace Langulus::RTTI::Inner
{

   constexpr MetaConstPacked_16::MetaConstPacked_16(nullptr_t) noexcept
      : Base {0} {}

   constexpr MetaConstPacked_16::MetaConstPacked_16(DefinitionConst const* d) noexcept
      : Base {d ? d->mID : 0} {}

   constexpr auto MetaConstPacked_16::operator = (nullptr_t)
   noexcept -> MetaConstPacked_16& {
      Base::operator = (0);
      return *this;
   }

   constexpr auto MetaConstPacked_16::operator = (DefinitionConst const* d)
   noexcept -> MetaConstPacked_16& {
      Base::operator = (d ? d->mID : 0);
      return *this;
   }

   /// Get the name of the constant, the result of NameOf                     
   inline auto MetaConstPacked_16::GetName() const noexcept -> Token {
      return Instance.GetMetaConstByID(*this)->mNameOf;
   }

   /// Get the info of the constant, the result of NameOf                     
   inline auto MetaConstPacked_16::GetInfo() const noexcept -> Token {
      return Instance.GetMetaConstByID(*this)->mInfoOf;
   }

   inline auto MetaConstPacked_16::GetVersionMajor()  const noexcept -> unsigned {
      return Instance.GetMetaConstByID(*this)->mVersionMajor;
   }

   inline auto MetaConstPacked_16::GetVersionMinor()  const noexcept -> unsigned {
      return Instance.GetMetaConstByID(*this)->mVersionMinor;
   }

} // namespace Langulus::RTTI::Inner
