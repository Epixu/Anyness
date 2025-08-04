///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// A static reserve                                                       
   ///                                                                        
   template<auto SIZE>
   struct ReserveStatic {
      using CTTI_Component = Yes<>;
      using ReserveType = decltype(SIZE);

      static_assert(SIZE > 0, "Can't have a container of zero or negative capacity");

      /// Get the number of reserved (maybe uninitialized) elements           
      ///   @return the number of reserved (maybe uninitialized) elements     
      constexpr auto GetReserved() const noexcept { return SIZE; }

   protected:
      template<unsigned>
      friend struct HeapMovable;

      /// Set number of reserved elements is impossible - it's at compile-time
      constexpr void SetReserved(ReserveType) const noexcept { LANGULUS(NOOP); }
   };
}
