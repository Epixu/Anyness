#pragma once
#include "../Typenav.hpp"
#include "../Literal.hpp"


namespace Langulus::CTTI
{
   /// Used to define a custom suffix as a short way to represent a data      
   /// type while scripting. Can be used in two ways to satisfy CT::Suffix<T>:
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Suffix = Yes<"s">;` in T                   
   template<class T>
   struct Suffix {
      static constexpr Literal Name = "<missing suffix>";
      static constexpr bool Enabled = false;
   };
}

LANGULUS_CTTI_CONCEPT(Suffix);
