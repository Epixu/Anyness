///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../Typenav.hpp"
#include <ranges>


namespace Langulus::CTTI
{
   /// Can be used in two ways to satisfy CT::Contiguous<T>:                  
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Contiguous = Yes/No;` in T                 
   template<class T>
   struct Contiguous {
      static constexpr bool Enabled = ::std::ranges::contiguous_range<T>;
   };
}

LANGULUS_CTTI_CONCEPT(Contiguous);
