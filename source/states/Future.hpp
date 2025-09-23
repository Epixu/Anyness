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
   /// If enabled, data is marked as a missing future                         
   ///   @tparam V - decides whether state is dynamic or static               
   template<State::StateValue V>
   struct Future {
      using CTTI_State = Yes<>;
      static constexpr bool Static  = V != State::Variable;
      static constexpr bool Dynamic = V == State::Variable;
      static constexpr bool Enable  = V == State::Enabled;
      static constexpr bool CanBeMissing = Dynamic or Enable;

      constexpr bool IsFuture() const requires Static {
         return Enable;
      }

      template<CT::Container C>
      constexpr bool IsFuture(this C const& self) noexcept requires Dynamic {
         return self.GetStateInner() & Future {};
      }

      template<CT::Container C>
      auto EnableFuture(this C& self) noexcept -> C& requires Dynamic {
         self.GetStateInner() += Future {};
         return self;
      }

      template<CT::Container C>
      auto DisableFuture(this C& self) noexcept -> C& requires Dynamic {
         self.GetStateInner() -= Future {};
         return self;
      }
   };
}

namespace Langulus::Anyness::State
{
   constexpr DefineState::Future<> Future = {};
}
