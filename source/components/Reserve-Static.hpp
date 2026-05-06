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
   template<auto SIZE, Cid ID, Cid...SHARED>
   struct ReserveStatic {
      using CTTI_Component = Yes<>;
      using CTTI_ReflectAs = void;
      using ReserveType = decltype(SIZE);

      static constexpr Cid  Id = ID;
      static constexpr int  ComponentPrecedence = -1000;
      template<Cid SID>
      static constexpr bool Relevant = IdMatch<SID, ID, SHARED...>;

      static_assert(SIZE > 0,
         "Can't have a container of zero or negative capacity");

      /// Get the number of reserved (maybe uninitialized) elements           
      template<Cid SID = ID> requires Relevant<SID>
      constexpr auto GetReserved() const noexcept -> ReserveType {
         return SIZE;
      }
   };
}
