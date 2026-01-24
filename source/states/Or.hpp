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
   /// If enabled, data is considered disjunct instead of conjunct.           
   /// Useful to encode alternative arguments or branched execution.          
   /// This simple bit is crucial for handling ambiguity.                     
   ///   @tparam V decides whether state is dynamic or static                 
   template<State::StateValue V>
   struct Or {
      using CTTI_State = Yes<>;
      static constexpr bool Static  = V != State::Variable;
      static constexpr bool Dynamic = V == State::Variable;
      static constexpr bool Enable  = V == State::Enabled;
      
      // Every state needs a unique ID in order to find matches even    
      // when template arguments are different                          
      static constexpr int UID = 3;

      constexpr bool IsOr() const requires Static {
         return Enable;
      }

      template<CT::Container C>
      constexpr bool IsOr(this const C& self) noexcept requires Dynamic {
         return self.GetStateInner() & Or {};
      }

      template<CT::Container C>
      auto EnableOr(this C& self) noexcept -> C& requires Dynamic {
         self.GetStateInner() += Or {};
         return self;
      }

      template<CT::Container C>
      auto DisableOr(this C& self) noexcept -> C& requires Dynamic {
         self.GetStateInner() -= Or {};
         return self;
      }
   };
}

namespace Langulus::Anyness::State
{
   constexpr DefineState::Or<> Or = {};
}
