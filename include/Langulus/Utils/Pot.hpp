///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include <cstdint>
#include <bit>
#include <concepts>


namespace Langulus
{
   /// An unsigned power-of-two number, represented by the index of the       
   /// most significant bit, with bit 0 resulting in zero, bit 1 -> 1,        
   /// bit 2 -> 2, bit 3 -> 4, bit 4 -> 8, bit 5 -> 16, etc..                 
   struct pot_t {
      uint8_t bit;

      constexpr pot_t() noexcept = default;
      constexpr pot_t(pot_t const&) noexcept = default;

      template<::std::unsigned_integral T>
      constexpr pot_t(T const& other) {
         if (not ::std::has_single_bit(other))
            throw ::std::runtime_error {"bad construction of pot_t"};
         bit = ::std::bit_width(other);
      }
      
      constexpr pot_t& operator = (pot_t const& rhs) noexcept = default;
      constexpr pot_t& operator = (::std::unsigned_integral auto const& rhs) {
         if (not ::std::has_single_bit(rhs))
            throw ::std::runtime_error {"bad assignment to pot_t"};
         bit = ::std::bit_width(rhs);
         return *this;
      }

      constexpr auto operator <=> (pot_t const& rhs) const noexcept = default;

      template<::std::unsigned_integral T>
      constexpr auto operator <=> (T const& rhs) const noexcept {
         return operator T () <=> rhs;
      }
      
      constexpr explicit operator bool () const noexcept {
         return bit != 0;
      }

      template<::std::unsigned_integral T>
      constexpr explicit operator T () const {
         if (not bit)
            return 0;
         if (bit > sizeof(T) * 8)
            throw ::std::runtime_error {"pot_t is too big to fit into T"};
         return T {1} << (bit - 1);
      }
   };
}
