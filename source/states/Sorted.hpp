#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::DefineState
{

   ///                                                                        
   /// If enabled, data is actively sorted when inserted/removed              
   ///   @tparam V - decides whether state is dynamic or static               
   template<State::StateValue V>
   struct Sorted {
      using CTTI_State = Yes;
      static constexpr bool Static  = V != State::Variable;
      static constexpr bool Dynamic = V == State::Variable;
      static constexpr bool Enable  = V == State::Enabled;

      constexpr bool IsSorted() const requires Static {
         return Enable;
      }

      template<CT::Container C>
      constexpr bool IsSorted(this const C& self) noexcept requires Dynamic {
         return self.mState & Sorted {};
      }

      template<CT::Container C>
      auto EnableSorting(this C& self) noexcept -> C& requires Dynamic {
         self.mState += Sorted {};
         return self;
      }

      template<CT::Container C>
      auto DisableSorting(this C& self) noexcept -> C& requires Dynamic {
         self.mState -= Sorted {};
         return self;
      }
   };

} // namespace Langulus::Anyness::DefineState

namespace Langulus::Anyness::State
{

   constexpr DefineState::Sorted<> Sorted = {};

} // namespace Langulus::Anyness::State
