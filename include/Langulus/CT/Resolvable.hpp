///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../Typenav.hpp"


namespace Langulus::Anyness
{
   struct Any;
}

namespace Langulus::CTTI
{
   /// Can be used in two ways to satisfy CT::Resolvable<T>:                  
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Resolvable = Yes<>;` in T                  
   /// Either way, the type still has to have the required interface for this 
   /// to work. By default, all types with method GetResolved() are considered
   /// CT::Resolvable                                                         
   template<class T>
   struct Resolvable {
      static constexpr bool Default = true;
      static constexpr bool Enabled = requires (T& a) {
         { a.GetResolved() } -> ::std::same_as<Anyness::Any>;
      };
   };
}

LANGULUS_CTTI_CONCEPT_DECVQ(Resolvable);
