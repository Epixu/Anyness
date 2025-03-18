#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::DefineState
{

   ///                                                                        
   /// If enabled, data is marked as encrypted                                
   ///   @tparam V - decides whether state is dynamic or static               
   template<State::StateValue V = State::Variable>
   struct Encrypted {
      using CTTI_State = Yes;
      static constexpr bool Static  = V != State::Variable;
      static constexpr bool Dynamic = V == State::Variable;
      static constexpr bool Enable  = V == State::Enabled;

      constexpr bool IsEncrypted() const requires Static {
         return Enable;
      }

      template<CT::Container C>
      constexpr bool IsEncrypted(this const C& self) noexcept requires Dynamic {
         return self.mState & C::template GetStateBit<Encrypted>();
      }

      template<CT::Container C>
      auto EnableEncrypted(this C& self) noexcept -> C& requires Dynamic {
         self.mState |= C::template GetStateBit<Encrypted>();
         return self;
      }

      template<CT::Container C>
      auto DisableEncrypted(this C& self) noexcept -> C& requires Dynamic {
         self.mState &= ~C::template GetStateBit<Encrypted>();
         return self;
      }
   };

} // namespace Langulus::Anyness::DefineState

namespace Langulus::Anyness::State
{

   constexpr DefineState::Encrypted Encrypted = {};

} // namespace Langulus::Anyness::State
