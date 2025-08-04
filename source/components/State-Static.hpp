///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
#include "../states/Default.hpp"
#include <bitset>
//#include <utility>


namespace Langulus::Anyness::Component
{
   template<CT::State...STATES>
   struct StateStatic {
      using CTTI_Component = Yes<>;
      static constexpr ::std::size_t Count = sizeof...(STATES);
      using State = ::std::bitset<Count>;

      consteval State GetState() {
         auto combiner = []<class...S, auto...I>(::std::index_sequence<I...>) -> State {
            static_assert(S::Static and ..., "States aren't static");
            return ((S::Enable << I) | ...);
         };
         return combiner.template operator()<STATES...>(::std::make_index_sequence<Count>{});
      }
   };
}
