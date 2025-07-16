#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::DefineState
{

   ///                                                                        
   /// If enabled, data is marked as a missing past                           
   ///   @tparam V - decides whether state is dynamic or static               
   template<State::StateValue V>
   struct Past {
      using CTTI_State = Yes<>;
      static constexpr bool Static  = V != State::Variable;
      static constexpr bool Dynamic = V == State::Variable;
      static constexpr bool Enable  = V == State::Enabled;

      constexpr bool IsPast() const requires Static {
         return Enable;
      }

      template<CT::Container C>
      constexpr bool IsPast(this const C& self) noexcept requires Dynamic {
         return self.mState & Past {};
      }

      template<CT::Container C>
      auto EnablePast(this C& self) noexcept -> C& requires Dynamic {
         self.mState += Past {};
         return self;
      }

      template<CT::Container C>
      auto DisablePast(this C& self) noexcept -> C& requires Dynamic {
         self.mState -= Past {};
         return self;
      }
   };

} // namespace Langulus::Anyness::DefineState

namespace Langulus::Anyness::State
{

   constexpr DefineState::Past<> Past = {};

} // namespace Langulus::Anyness::State
