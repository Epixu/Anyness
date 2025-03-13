#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::DefineState
{

   template<State::StateValue V = State::Variable>
   struct Tracked {
      using CTTI_State = Yes;
      static constexpr bool Static = V != State::Variable;
      static constexpr bool Enable = V == State::Enabled;
   };

} // namespace Langulus::Anyness::DefineState

namespace Langulus::Anyness::State
{

   constexpr DefineState::Tracked Tracked = {};

} // namespace Langulus::Anyness::State
