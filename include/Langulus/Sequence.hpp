///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Values.hpp"
#include "CT/Noexcept.hpp"
#include <utility>


namespace Langulus
{
   template<auto...IDX>
   using ExpandedSequence =
      ::std::integer_sequence<typename Values<IDX...>::FirstType, IDX...>;

   ///                                                                        
   ///   Compile-time integer sequences                                       
   ///                                                                        
   template<auto END>
   struct Sequence {
      using Type = decltype(END);
      static constexpr auto Size   = END;
      static constexpr bool Empty  = END == 0;
      static constexpr auto Expand = ::std::make_integer_sequence<Type, END> {};

      /// Iterate through each index in the sequence using generator pattern  
      ///   @param lambda - a templated lambda function                       
      /// Example use:                                                        
      ///   Sequence<Ret::Columns>::ForEach([&]<Offset COL>() noexcept {      
      ///      auto& lc = lhs.template GetColumn<COL>();                      
      ///      Sequence<Ret::Rows>::ForEach([&]<Offset ROW>() noexcept {      
      ///         *(r++) = (lc * rhs.template GetRow<ROW>()).HSum();          
      ///      });                                                            
      ///   });                                                               
      static constexpr void ForEach(auto&& lambda) noexcept_if(lambda) {
         [&]<Type...IDX>(ExpandedSequence<IDX...>) noexcept_if(lambda) {
            (lambda.template operator() <IDX> (), ...);
         }(Expand);
      }
   };
}

/// Convenience macro that generates an unfoldable function body              
/// Example use:                                                              
///   return LglsSequence(StateCount, {                                       
///      return ((StateType {1} << I) | ...);                                 
///   });                                                                     
#define LglsSequence(END, BODY)                                          \
   [&]<decltype(END)...I>(::std::integer_sequence<decltype(END), I...>)  \
      BODY                                                               \
   (::std::make_integer_sequence<decltype(END), END> {})
