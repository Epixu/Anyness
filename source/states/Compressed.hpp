#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::DefineState
{

   ///                                                                        
   /// If enabled, data is marked as compressed                               
   ///   @tparam V - decides whether state is dynamic or static               
   template<State::StateValue V>
   struct Compressed {
      using CTTI_State = Yes;
      static constexpr bool Static  = V != State::Variable;
      static constexpr bool Dynamic = V == State::Variable;
      static constexpr bool Enable  = V == State::Enabled;

      constexpr bool IsCompressed() const requires Static {
         return Enable;
      }

      template<CT::Container C>
      constexpr bool IsCompressed(this const C& self) noexcept requires Dynamic {
         return self.mState & Compressed {};
      }

      template<CT::Container C>
      auto EnableCompressed(this C& self) noexcept -> C& requires Dynamic {
         self.mState += Compressed {};
         return self;
      }

      template<CT::Container C>
      auto DisableCompressed(this C& self) noexcept -> C& requires Dynamic {
         self.mState -= Compressed {};
         return self;
      }
   };

} // namespace Langulus::Anyness::DefineState

namespace Langulus::Anyness::State
{

   constexpr DefineState::Compressed<> Compressed = {};

} // namespace Langulus::Anyness::State
