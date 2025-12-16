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
   /// If enabled, data won't ever change type. Very useful when a type-      
   /// erased container has to represent a templated counterpart.             
   /// Needed to constrain the memory manipulations for safety.               
   ///   @tparam V decides whether state is dynamic or static                 
   template<State::StateValue V>
   struct Typed {
      using CTTI_State = Yes<>;
      static constexpr bool Static  = V != State::Variable;
      static constexpr bool Dynamic = not Static;
      static constexpr bool Enable  = V == State::Enabled;
      
      // Every state needs a unique ID in order to find matches even    
      // when template arguments are different                          
      static constexpr int UID = 7;

      template<CT::TypeErased C>
      constexpr bool IsTypeConstrained(this const C&) requires Static {
         return Enable;
      }

      template<CT::TypeErased C>
      constexpr bool IsTypeConstrained(this const C& self) noexcept requires Dynamic {
         return self.GetStateInner() & Typed {};
      }

      template<CT::TypeErased C>
      auto EnableTypeConstrained(this C& self) noexcept -> C& requires Dynamic {
         self.GetStateInner() += Typed {};
         return self;
      }

      template<CT::TypeErased C>
      auto DisableTypeConstrained(this C& self) noexcept -> C& requires Dynamic {
         self.GetStateInner() -= Typed {};
         return self;
      }
   };
}

namespace Langulus::Anyness::State
{
   constexpr DefineState::Typed<> Typed = {};
}
