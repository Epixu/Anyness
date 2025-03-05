#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::State
{

   template<StateValue V = Variable>
   struct Past {
      using CTTI_State = Yes;
      static constexpr bool Static = V != Variable;
      static constexpr bool Enable = V == Enable;
   };

} // namespace Langulus::Anyness::Component
