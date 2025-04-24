#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::DefineState
{

   ///                                                                        
   /// If enabled, data is considered disjunct instead of conjunct            
   /// Useful to encode alternative arguments or branched execution           
   ///   @tparam V - decides whether state is dynamic or static               
   template<State::StateValue V>
   struct Or {
      using CTTI_State = Yes;
      static constexpr bool Static  = V != State::Variable;
      static constexpr bool Dynamic = V == State::Variable;
      static constexpr bool Enable  = V == State::Enabled;

      constexpr bool IsOr() const requires Static {
         return Enable;
      }

      template<CT::Container C>
      constexpr bool IsOr(this const C& self) noexcept requires Dynamic {
         return self.mState & Or {};
      }

      template<CT::Container C>
      auto EnableOr(this C& self) noexcept -> C& requires Dynamic {
         self.mState += Or {};
         return self;
      }

      template<CT::Container C>
      auto DisableOr(this C& self) noexcept -> C& requires Dynamic {
         self.mState -= Or {};
         return self;
      }
   };

} // namespace Langulus::Anyness::DefineState

namespace Langulus::Anyness::State
{

   constexpr DefineState::Or<> Or = {};

} // namespace Langulus::Anyness::State
