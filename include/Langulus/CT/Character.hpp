///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../CTTI.hpp"


namespace Langulus::CTTI
{

   /// Can be used in two ways to satisfy CT::Character<T>:                   
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Character = Yes;` in T                     
   template<class T>
   struct Character {
      static constexpr bool Enabled =
            ::std::same_as<::std::remove_reference_t<T>, char>
         or ::std::same_as<::std::remove_reference_t<T>, wchar_t>
         or ::std::same_as<::std::remove_reference_t<T>, char8_t>
         or ::std::same_as<::std::remove_reference_t<T>, char16_t>
         or ::std::same_as<::std::remove_reference_t<T>, char32_t>;
   };
   
} // namespace Langulus::CTTI

LANGULUS_CTTI_CONCEPT(Character);