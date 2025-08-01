///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../Typenav.hpp"


namespace Langulus::CTTI
{
   /// Can be used in two ways to satisfy CT::Character<T>:                   
   /// The types char, wchar_t, char8_t, char16_t and char32_t are considered 
   /// CT::Character by default                                               
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Character = Yes<>;` in T                   
   template<class T>
   struct Character {
      static constexpr bool Default = true;
      static constexpr bool Enabled =
            ::std::same_as<Decvq<Deref<T>>, char>
         or ::std::same_as<Decvq<Deref<T>>, wchar_t>
         or ::std::same_as<Decvq<Deref<T>>, char8_t>
         or ::std::same_as<Decvq<Deref<T>>, char16_t>
         or ::std::same_as<Decvq<Deref<T>>, char32_t>;
   };
}

LANGULUS_CTTI_CONCEPT_DECVQ(Character);
