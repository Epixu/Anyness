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


namespace Langulus::CTTI
{
   /// Used to define a custom file extensions for serialization.             
   /// Can be used in two ways to satisfy CT::Files<T>:                       
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Files = Yes<"txt,rtf,etc">;` in T          
   template<class T>
   struct Files {
      static constexpr Literal Name = "<missing file extensions>";
      static constexpr bool Enabled = false;
   };
}

LANGULUS_CTTI_CONCEPT(Files);
