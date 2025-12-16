///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../Typenav.hpp"
#include "../Literal.hpp"
#include "../Utils/ASCII.hpp"


namespace Langulus::CTTI
{
   /// Used to define a custom suffix as a short way to represent a data      
   /// type while scripting. Can be used in two ways to satisfy CT::Suffix<T>:
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Suffix = Yes<"s">;` in T                   
   template<class T>
   struct Suffix;
}

LANGULUS_CTTI_CONCEPT_DECVQ(Suffix);

namespace Langulus::CTTI
{
   ///                                                                        
   /// Some built-in suffices                                                 
   ///                                                                        
   template<>
   struct Suffix<bool> {
      static constexpr Literal Name = "b";
   };

   template<>
   struct Suffix<int8_t> {
      static constexpr Literal Name = "i8";
   };

   template<>
   struct Suffix<int16_t> {
      static constexpr Literal Name = "i16";
   };

   template<>
   struct Suffix<int32_t> {
      static constexpr Literal Name = ::std::same_as<int32_t, int> ? "i\0\0" : "i32";
   };

   template<>
   struct Suffix<int64_t> {
      static constexpr Literal Name = ::std::same_as<int64_t, int> ? "i\0\0" : "i64";
   };

   template<>
   struct Suffix<uint8_t> {
      static constexpr Literal Name = "u8";
   };

   template<>
   struct Suffix<uint16_t> {
      static constexpr Literal Name = "u16";
   };

   template<>
   struct Suffix<uint32_t> {
      static constexpr Literal Name = ::std::same_as<uint32_t, unsigned int> ? "u\0\0" : "u32";
   };

   template<>
   struct Suffix<uint64_t> {
      static constexpr Literal Name = ::std::same_as<uint64_t, unsigned int> ? "u\0\0" : "u64";
   };
   
   template<>
   struct Suffix<float> {
      static constexpr Literal Name = ::std::same_as<float, Real> ? "\0" : "f";
   };

   template<>
   struct Suffix<double> {
      static constexpr Literal Name = ::std::same_as<double, Real> ? "\0" : "d";
   };
}

namespace Langulus
{
   /// Get the suffix of a type at compile-time                               
   ///   @tparam T the type to get the name of                                
   ///   @return a compile-time string                                        
   template<class T>
   consteval auto SuffixOf() {
      using DT = Decvq<Deref<T>>;      
      if constexpr (CT::Complete<CTTI::Suffix<DT>>) {
         constexpr auto s = CTTI::Suffix<DT>::Name;
         static_assert(IsASCII(s), "Suffix must be ASCII");
         static_assert(s == "" or IsAlphabetical(s[0]),
            "Suffix must begin with an alphabetical symbol");
         return s;
      }
      else if constexpr (LANGULUS_CTTI_DELVE_IN(DT, Suffix)) {
         constexpr auto s = DT::CTTI_Suffix::Constant;
         static_assert(IsASCII(s), "Suffix must be ASCII");
         static_assert(s == "" or IsAlphabetical(s[0]),
            "Suffix must begin with an alphabetical symbol");
         return DT::CTTI_Suffix::Constant;
      }
      else return Literal {};
   }
}
