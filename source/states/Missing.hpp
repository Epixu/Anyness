#pragma once
#include "../Container.hpp"
#include "Past.hpp"
#include "Future.hpp"


namespace Langulus::Anyness::DefineState
{

   ///                                                                        
   /// Used to check for past/future state                                    
   struct Missing {};

} // namespace Langulus::Anyness::DefineState

namespace Langulus::Anyness::State
{

   constexpr DefineState::Missing Missing = {};

} // namespace Langulus::Anyness::State
