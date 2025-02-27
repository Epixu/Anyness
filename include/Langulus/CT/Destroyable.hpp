#pragma once
#include "../CTTI.hpp"


namespace Langulus::CTTI
{

   /// Can be used in two ways to satisfy CT::Destroyable<T>:                 
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Destroyable = Yes/No;` in T                
   template<class T>
   struct Destroyable {
      static constexpr bool Value = not ::std::is_trivially_destructible_v<T>
                                    and ::std::is_destructible_v<T>;
   };
   
} // namespace Langulus::CTTI

LANGULUS_CTTI_CONCEPT(Destroyable);