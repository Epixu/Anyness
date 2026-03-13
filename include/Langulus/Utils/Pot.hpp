///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include <Langulus/Assume.hpp>
#include <bit>
#include <concepts>
#include <compare>


namespace Langulus
{
   ///                                                                        
   /// An unsigned power-of-two number, represented by the index of the       
   /// most significant bit, with bit 0 -> 1, bit 1 -> 2,                     
   /// bit 2 -> 4, bit 3 -> 8, bit 4 -> 16, bit 5 -> 32, etc..                
   ///   @attention can't be zero                                             
   struct pot_t {
      uint8_t bit;

      constexpr pot_t() noexcept = default;
      constexpr pot_t(pot_t const&) noexcept = default;

      template<::std::unsigned_integral T>
      explicit constexpr pot_t(T const& other) assumptious {
         LglsAssumeDev(::std::has_single_bit(other),
            "bad construction of pot_t");
         bit = ::std::bit_width(other) - 1;
      }
      
      constexpr pot_t& operator = (pot_t const& rhs) noexcept = default;
      constexpr pot_t& operator = (::std::unsigned_integral auto const& rhs) assumptious {
         LglsAssumeDev(::std::has_single_bit(rhs),
            "bad assignment to pot_t");
         bit = ::std::bit_width(rhs) - 1;
         return *this;
      }

      constexpr bool operator == (pot_t const& rhs) const noexcept = default;
      constexpr auto operator <=> (pot_t const& rhs) const noexcept = default;

      template<::std::unsigned_integral T>
      constexpr bool operator == (T const& rhs) const noexcept {
         return this->operator T () == rhs;
      }

      template<::std::unsigned_integral T>
      constexpr auto operator <=> (T const& rhs) const noexcept {
         return this->operator T () <=> rhs;
      }

      template<::std::unsigned_integral T> requires (not ::std::same_as<T, bool>)
      constexpr explicit operator T () const assumptious {
         LglsAssumeDev(bit < sizeof(T) * 8,
            "pot_t is too big to fit into T");
         return T {1} << bit;
      }

      constexpr uintptr_t mask() const noexcept {
         return this->operator uintptr_t () - 1;
      }

      constexpr size_t operator + (pot_t const& rhs) const noexcept {
         return (size_t {1} << bit) + (size_t {1} << rhs.bit);
      }

      template<::std::unsigned_integral T>
      constexpr T operator + (T const& rhs) const noexcept {
         return (T {1} << bit) + rhs;
      }
      
      constexpr size_t operator - (pot_t const& rhs) const noexcept {
         return (size_t {1} << bit) - (size_t {1} << rhs.bit);
      }
      
      template<::std::unsigned_integral T>
      constexpr T operator - (T const& rhs) const noexcept {
         return (T {1} << bit) - rhs;
      }

      constexpr pot_t operator / (pot_t const& rhs) const assumptious {
         LglsAssumeDev(rhs.bit <= bit,
            "pot_t division resulted in zero");
         pot_t result;
         result.bit = bit - rhs.bit;
         return result;
      }

      template<::std::unsigned_integral T>
      constexpr size_t operator / (T const& rhs) const noexcept {
         return (T {1} << bit) / rhs;
      }

      constexpr pot_t operator * (pot_t const& rhs) const assumptious {
         LglsAssumeDev(rhs.bit + bit <= 255u,
            "pot_t multiplication overflowed");
         pot_t result;
         result.bit = bit + rhs.bit;
         return result;
      }
      
      template<::std::unsigned_integral T>
      constexpr size_t operator * (T const& rhs) const noexcept {
         return (T {1} << bit) * rhs;
      }

      constexpr pot_t operator % (pot_t const& rhs) const assumptious {
         LglsAssumeDev(rhs.bit > bit,
            "pot_t modulo resulted in zero");
         return *this;
      }

      template<::std::unsigned_integral T>
      constexpr pot_t& operator >>= (T const& rhs) assumptious {
         LglsAssumeDev(rhs <= bit,
            "pot_t right shift underflowed");
         bit -= rhs;
         return *this;
      }

      template<::std::unsigned_integral T>
      constexpr pot_t& operator <<= (T const& rhs) assumptious {
         LglsAssumeDev(rhs + bit <= 255u,
            "pot_t left shift overflowed");
         bit += rhs;
         return *this;
      }

      template<::std::unsigned_integral T>
      constexpr pot_t operator << (T const& rhs) const noexcept {
         LglsAssumeDev(rhs + bit <= 255u,
            "pot_t left shift overflowed");
         pot_t result;
         result.bit = bit + rhs;
         return result;
      }

      template<::std::unsigned_integral T>
      constexpr pot_t operator >> (T const& rhs) const noexcept {
         LglsAssumeDev(rhs <= bit,
            "pot_t right shift underflowed");
         pot_t result;
         result.bit = bit - rhs;
         return result;
      }
   };

   constexpr pot_t operator""_pot(ulonger num) assumptious {
      return pot_t(num);
   }
}

#if LANGULUS_FEATURE(LOGGING)
namespace fmt
{
   ///                                                                        
   /// Extend FMT to be capable of logging pot_t                              
   template<>
   struct formatter<::Langulus::pot_t> {
      template<class CONTEXT>
      constexpr auto parse(CONTEXT& ctx) {
         return ctx.begin();
      }

      template<class CONTEXT> LANGULUS(INLINED)
      auto format(::Langulus::pot_t const& c, CONTEXT& ctx) const {
         return format_to(ctx.out(), "{}", static_cast<size_t>(c));
      }
   };
}
#endif
