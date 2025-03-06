///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Same.hpp"
#include "../TypeNav.hpp"
#include "../Types.hpp"
#include "../Intent.hpp"


namespace Langulus::CTTI
{

   /// Can be used in two ways to satisfy CT::Unfoldable<T>:                  
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Unfoldable = Yes;` in T                    
   template<class T>
   struct Unfoldable {
      static constexpr bool Enabled = CT::Intent<T> or CT::Array<T>
         or (::std::ranges::range<T> and CT::Typed<T>);
   };
   
} // namespace Langulus::CTTI

LANGULUS_CTTI_CONCEPT(Unfoldable);

namespace Langulus::CT
{
   namespace Inner
   {

      ///   @return a pointer of the first nested non-unfoldable type         
      template<class T, class UNLESS = void>
      consteval Typelist auto Unfold() {
         if constexpr (Similar<T, UNLESS> or not Unfoldable<T>) {
            // Immediately break the nesting if UNLESS condition is met 
            // Alternatively, break nesting if T is reflected as not    
            // unfoldable                                               
            return Types<T> {};
         }
         else if constexpr (Sheddable<T>) {
            // Shed and nest (intents end up here)                      
            return Unfold<Shed<T>>();
         }
         else if constexpr (Array<T>) {
            // Shed array extents and nest                              
            return Unfold<Deext<T>>();
         }
         else if constexpr (Typed<T>) {
            // This includes ::std::ranges::range as well as anything   
            // that is statically typed in Anyness, unless reflected    
            // as not Unfoldable                                        
            return Unfold<TypeOf<T>>();
         }
         else Types<T> {};
      }

   } // namespace Langulus::CT::Inner
      
   /// Unfolds T, if it is a bounded array or std::range, and returns the     
   /// contained type. Nested for ranges containing other ranges, or arrays   
   /// containing other arrays/ranges, etc. Sheds sheddables like intents     
   ///   @tparam T - type to unfold                                           
   ///   @tparam UNLESS - stop unfolding if the type is similar, useful in    
   ///      contexts where you actually want to insert a std::map for         
   ///      example, and not unfold it down to pairs                          
   template<class T, class UNLESS = void>
   using Unfold = typename decltype(Inner::Unfold<T, UNLESS>())::First;

} // namespace Langulus::CT
