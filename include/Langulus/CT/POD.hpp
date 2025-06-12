///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Abstract.hpp"


namespace Langulus::CTTI
{

   /// Affects CT::POD<T>                                                     
   ///   @note: is_trivially_destructible_v is required to strenghten the     
   ///      is_trivial_v check on GCC/Clang due to compiler bugs; MSVC is fine
   template<class T>
   struct POD {
      static constexpr bool Enabled = not CT::Abstract<T> and (
         CT::Sparse<T> or CT::Fundamental<T> or (
                ::std::is_trivial_v<T>
            and ::std::is_standard_layout_v<T>
            and ::std::is_trivially_destructible_v<T>
         )
      );
   };
   
} // namespace Langulus::CTTI

LANGULUS_CTTI_CONCEPT(POD);