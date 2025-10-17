///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../Typenav.hpp"

namespace Langulus
{

   ///                                                                        
   ///   A byte                                                               
   ///                                                                        
   ///   std::byte is shitty, this one's better. It preserves arithmetic      
   /// operations on the byte. These operations counteract integer promotion, 
   /// the result is always truncated back down to a byte.                    
   ///                                                                        
   #pragma pack(push, 1)
   struct Byte {
      using Type          = uint8_t;
      using CTTI_Typed    = Type;
      using CTTI_POD      = Yes<>;
      using CTTI_Nullable = Yes<>;

      Type value;

      constexpr Byte() noexcept = default;
      constexpr Byte(Byte const&) noexcept = default;
      constexpr Byte(Byte&&) noexcept = default;
      explicit constexpr Byte(Type const& a) noexcept : value {a} {}

      constexpr Byte& operator = (Byte const&) noexcept = default;
      constexpr Byte& operator = (Byte&&) noexcept = default;
      constexpr Byte& operator = (Type const& a) noexcept {
         value = a;
         return *this;
      }

      constexpr Byte operator + (const Byte& rhs) const noexcept {
         return static_cast<Byte>(value + rhs.value);
      }

      constexpr Byte operator - (const Byte& rhs) const noexcept {
         return static_cast<Byte>(value - rhs.value);
      }

      constexpr Byte operator * (const Byte& rhs) const noexcept {
         return static_cast<Byte>(value * rhs.value);
      }

      constexpr Byte operator / (const Byte& rhs) const noexcept {
         return static_cast<Byte>(value / rhs.value);
      }

      constexpr Byte operator % (const Byte& rhs) const noexcept {
         return static_cast<Byte>(value % rhs.value);
      }

      constexpr Byte operator << (const Byte& rhs) const noexcept {
         return static_cast<Byte>(value << rhs.value);
      }

      constexpr Byte operator >> (const Byte& rhs) const noexcept {
         return static_cast<Byte>(value >> rhs.value);
      }

      constexpr Byte operator ^ (const Byte& rhs) const noexcept {
         return static_cast<Byte>(value ^ rhs.value);
      }

      constexpr Byte operator & (const Byte& rhs) const noexcept {
         return static_cast<Byte>(value & rhs.value);
      }

      constexpr Byte operator | (const Byte& rhs) const noexcept {
         return static_cast<Byte>(value | rhs.value);
      }

      constexpr Byte& operator += (const Byte& rhs) noexcept {
         value += rhs.value;
         return *this;
      }

      constexpr Byte& operator -= (const Byte& rhs) noexcept {
         value -= rhs.value;
         return *this;
      }

      constexpr Byte& operator *= (const Byte& rhs) noexcept {
         value *= rhs.value;
         return *this;
      }

      constexpr Byte& operator /= (const Byte& rhs) noexcept {
         value /= rhs.value;
         return *this;
      }

      constexpr Byte& operator %= (const Byte& rhs) noexcept {
         value %= rhs.value;
         return *this;
      }

      constexpr Byte& operator <<= (const Byte& rhs) noexcept {
         value <<= rhs.value;
         return *this;
      }

      constexpr Byte& operator >>= (const Byte& rhs) noexcept {
         value >>= rhs.value;
         return *this;
      }

      constexpr Byte& operator ^= (const Byte& rhs) noexcept {
         value ^= rhs.value;
         return *this;
      }

      constexpr Byte& operator &= (const Byte& rhs) noexcept {
         value &= rhs.value;
         return *this;
      }

      constexpr Byte& operator |= (const Byte& rhs) noexcept {
         value |= rhs.value;
         return *this;
      }

      constexpr auto operator <=> (const Byte&) const noexcept = default;
      constexpr bool operator ==  (const Byte&) const noexcept = default;

      /// Prefix operators                                                    
      constexpr Byte& operator ++ () noexcept { ++value; return *this; }
      constexpr Byte& operator -- () noexcept { --value; return *this; }

      /// Suffix operators                                                    
      constexpr Byte operator -- (int) noexcept { return static_cast<Byte>(value--); }
      constexpr Byte operator ++ (int) noexcept { return static_cast<Byte>(value++); }
   };
   #pragma pack(pop)
}
