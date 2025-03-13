#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::DefineState
{

   struct Default {
      using CTTI_State = Yes;
   };

} // namespace Langulus::Anyness::DefineState

namespace Langulus::Anyness::State
{

   constexpr DefineState::Default Default = {};

} // namespace Langulus::Anyness::State
