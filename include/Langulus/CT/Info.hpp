#pragma once
#include "../Typenav.hpp"


namespace Langulus::CTTI
{

   /// Can be used in two ways to satisfy CT::Info<T>:                        
   /// 1. Specialize for T/concept having Enabled as true and an info string  
   /// 2. Add a public `using CTTI_Info = Yes<"some info">;` in T             
   template<class T>
   struct Info {
      static constexpr Literal Text = "<no info provided>";
      static constexpr bool Enabled = false;
   };

   template<auto E>
   struct InfoValue {
      static constexpr Literal Text = "<no info provided>";
      static constexpr bool Enabled = false;
   };

} // namespace Langulus::CTTI

LANGULUS_CTTI_CONCEPT(Info);

namespace Langulus::CT
{
   template<auto E>
   concept InfoValue = CTTI::InfoValue<E>::Enabled;

   template<auto E>
   concept NotInfoValue = not InfoValue<E>;

} // namespace Langulus::CT
