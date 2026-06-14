///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
#include <Langulus/CT/Signed.hpp>
#include <Langulus/CT/Integer.hpp>


namespace Langulus::Anyness::Component
{
   ///                                                                        
   ///   A compile-time reserve                                               
   ///                                                                        
   ///   Reserve shows how many elements inside a container are allocated.    
   ///   Compile-time reserving isn't really reserving, and doesn't take up   
   /// space, but is useful for defining single-element containers that       
   /// still need the API required to function alongside other components.    
   ///   In these cases, reserve is equal to SIZE if container has a heap     
   /// component that has been allocated - otherwise it is 0. If no heap      
   /// component exists or can't be null, then the reserve is always SIZE.    
   ///   @tparam SIZE the reserve type and value                              
   ///   @tparam ID provider ID to keep reserve of                            
   ///   @tparam SHARED provider IDs that share the same reserve variable     
   template<auto SIZE, Cid ID, Cid...SHARED>
   struct ReserveStatic {
      using CTTI_Component = Yes<>;
      using CTTI_ReflectAs = void;
      using ReserveType = decltype(SIZE);
      using Id = Values<ID, SHARED...>;

      static constexpr int  ComponentPrecedence = -1000;
      template<Cid SID>
      static constexpr bool Relevant = Id::template Contains<SID>;

      static_assert(SIZE > 0,
         "Can't have a container of zero or negative capacity");
      static_assert(CT::Integer<ReserveType> and not CT::Signed<ReserveType>,
         "Reserve type must be an unsigned integer");
   
      /// Get the number of reserved (maybe uninitialized) elements           
      template<Cid SID = ID, CT::Container C> requires Relevant<SID>
      constexpr auto GetReserved(this C const& self) noexcept -> ReserveType {
         if constexpr (CT::HasVariableCount<C>)
            return self.template GetRaw<SID>() ? SIZE : ReserveType {0};
         else
            return SIZE;
      }
   };
}
