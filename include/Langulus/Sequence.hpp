///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Core.hpp"
#include "Values.hpp"
#include <utility>


namespace Langulus
{

   template<auto...IDX>
   using ExpandedSequence = ::std::integer_sequence<typename Values<IDX...>::FirstType, IDX...>;


   ///                                                                        
   ///   Compile-time integer sequences                                       
   ///                                                                        
   template<auto END>
   struct Sequence {
   protected:
      template<class LAMBDA>
      static consteval bool Noexcept() {
         return noexcept(Fake<LAMBDA&&>().template operator() <0> ());
      }

   public:
      using Type = decltype(END);
      static constexpr auto Size   = END;
      static constexpr bool Empty  = END == 0;
      static constexpr auto Expand = ::std::make_integer_sequence<Type, END> {};

      /// Iterate through each index in the sequence using generator pattern  
      ///   @param generator - a templated lambda function                    
      /// Example use:                                                        
      ///   Sequence<Ret::Columns>::ForEach([&]<Offset COL>() noexcept {      
      ///      auto& lc = lhs.template GetColumn<COL>();                      
      ///      Sequence<Ret::Rows>::ForEach([&]<Offset ROW>() noexcept {      
      ///         *(r++) = (lc * rhs.template GetRow<ROW>()).HSum();          
      ///      });                                                            
      ///   });                                                               
      template<class LAMBDA>
      static constexpr void ForEach(LAMBDA&& generator) noexcept(Noexcept<LAMBDA>()) {
         [&]<Type...IDX>(ExpandedSequence<IDX...>) noexcept(Noexcept<LAMBDA>()) {
            (generator.template operator() <IDX> (), ...);
         }(Expand);
      }
   };

} // namespace Langulus

/// Convenience macro that generates an unfoldable function body              
/// Example use:                                                              
///   return LANGULUS_SEQUENCE(StateCount, {                                  
///      return ((StateType {1} << I) | ...);                                 
///   });                                                                     
#define LANGULUS_SEQUENCE(END, BODY)                                     \
   [&]<decltype(END)...I>(::std::integer_sequence<decltype(END), I...>)  \
      BODY                                                               \
   (::std::make_integer_sequence<decltype(END), END> {});
