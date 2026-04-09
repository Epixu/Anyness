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


namespace Langulus::Anyness::Component
{
   template<CT::State...STATES>
   struct StateHeap {
      using CTTI_Component = Yes<>;

      //static constexpr Cid Id = ID;
      static constexpr int ComponentPrecedence = -1000;
      static constexpr size_t StateCount = sizeof...(STATES);

      using StateType = Tif<sizeof...(STATES) <= 8, uint8_t, uint16_t>;

      template<class Self>
      StateType GetState(this const Self& self) noexcept {
         return self.GetHeap<HEAP_ID>().GetElement<T, ID>();
      }
   };
}
