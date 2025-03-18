#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::DefineState
{

   ///                                                                        
   /// If enabled, data is marked as a missing future                         
   ///   @tparam V - decides whether state is dynamic or static               
   template<State::StateValue V = State::Variable>
   struct Future {
      using CTTI_State = Yes;
      static constexpr bool Static  = V != State::Variable;
      static constexpr bool Dynamic = V == State::Variable;
      static constexpr bool Enable  = V == State::Enabled;

      constexpr bool IsFuture() const requires Static {
         return Enable;
      }

      template<CT::Container C>
      constexpr bool IsFuture(this const C& self) noexcept requires Dynamic {
         return self.mState & Future {};
      }

      template<CT::Container C>
      auto EnableFuture(this C& self) noexcept -> C& requires Dynamic {
         self.mState += Future {};
         return self;
      }

      template<CT::Container C>
      auto DisableFuture(this C& self) noexcept -> C& requires Dynamic {
         self.mState -= Future {};
         return self;
      }
   };

} // namespace Langulus::Anyness::DefineState

namespace Langulus::Anyness::State
{

   constexpr DefineState::Future Future = {};

} // namespace Langulus::Anyness::State
