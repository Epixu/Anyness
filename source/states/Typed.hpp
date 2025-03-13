#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::DefineState
{

   ///                                                                        
   /// If enabled, data won't ever change type - useful for templated packs   
   /// Used to constrain the memory manipulations for safety                  
   ///   @tparam V - decides whether state is dynamic or static               
   template<State::StateValue V = State::Variable>
   struct Typed {
      using CTTI_State = Yes;
      static constexpr bool Static  = V != State::Variable;
      static constexpr bool Dynamic = V == State::Variable;
      static constexpr bool Enable  = V == State::Enabled;

      consteval bool IsTypeConstrained() const requires Static {
         return Enable;
      }

      template<CT::Container C>
      constexpr bool IsTypeConstrained(this const C& self) noexcept requires Dynamic {
         return self.mState & C::template GetStateBit<Typed>();
      }

      template<CT::Container C>
      void EnableTypeConstrained(this C& self) noexcept requires Dynamic {
         self.mState |= C::template GetStateBit<Typed>();
      }

      template<CT::Container C>
      void DisableTypeConstrained(this C& self) noexcept requires Dynamic {
         self.mState &= ~C::template GetStateBit<Typed>();
      }
   };

} // namespace Langulus::Anyness::DefineState

namespace Langulus::Anyness::State
{

   constexpr DefineState::Typed Typed = {};

} // namespace Langulus::Anyness::State
