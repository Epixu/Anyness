///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::DefineState
{
   ///                                                                        
   /// If enabled, data is marked as a missing past                           
   ///   @tparam V decides whether state is dynamic or static                 
   template<State::StateValue V>
   struct Past {
      using CTTI_State = Yes<>;
      static constexpr bool Static  = V != State::Variable;
      static constexpr bool Dynamic = V == State::Variable;
      static constexpr bool Enable  = V == State::Enabled;
      static constexpr bool CanBeMissing = Dynamic or Enable;
      
      // Every state needs a unique ID in order to find matches even    
      // when template arguments are different                          
      static constexpr int UID = 4;

      constexpr bool IsPast() const requires Static {
         return Enable;
      }

      template<CT::Container C>
      constexpr bool IsPast(this C const& self) noexcept requires Dynamic {
         return self.GetStateInner() & Past {};
      }

      template<CT::Container C>
      auto EnablePast(this C& self) noexcept -> C& requires Dynamic {
         self.GetStateInner() += Past {};
         return self;
      }

      template<CT::Container C>
      auto DisablePast(this C& self) noexcept -> C& requires Dynamic {
         self.GetStateInner() -= Past {};
         return self;
      }
   };
}

namespace Langulus::Anyness::State
{
   constexpr DefineState::Past<> Past = {};
}
