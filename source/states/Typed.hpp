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
   /// If enabled, data won't ever change type - useful for templated packs   
   /// Used to constrain the memory manipulations for safety                  
   ///   @tparam V - decides whether state is dynamic or static               
   template<State::StateValue V>
   struct Typed {
      using CTTI_State = Yes<>;
      static constexpr bool Static  = V != State::Variable;
      static constexpr bool Dynamic = V == State::Variable;
      static constexpr bool Enable  = V == State::Enabled;

      template<CT::TypeErased C>
      constexpr bool IsTypeConstrained(this const C&) requires Static {
         return Enable;
      }

      template<CT::TypeErased C>
      constexpr bool IsTypeConstrained(this const C& self) noexcept requires Dynamic {
         return self.mState & Typed {};
      }

      template<CT::TypeErased C>
      auto EnableTypeConstrained(this C& self) noexcept -> C& requires Dynamic {
         self.mState += Typed {};
         return self;
      }

      template<CT::TypeErased C>
      auto DisableTypeConstrained(this C& self) noexcept -> C& requires Dynamic {
         self.mState -= Typed {};
         return self;
      }
   };
}

namespace Langulus::Anyness::State
{
   constexpr DefineState::Typed<> Typed = {};
}
