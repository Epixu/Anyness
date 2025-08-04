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


namespace Langulus::Anyness::Component
{
   template<CT::State...STATES>
   struct StateHeap {
      using CTTI_Component = Yes<>;
      static constexpr ::std::size_t Count = sizeof...(STATES);
      using State = ::std::bitset<Count>;

      template<class Self>
      State GetState(this const Self& self) noexcept {
         return self.GetHeap<HEAP_ID>().GetElement<T, ID>();
      }
   };
}
