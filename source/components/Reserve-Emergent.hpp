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
   /// A dynamic reserve derived from the allocation size directly.           
   /// As such, it will not increase container's stack size, but will require 
   /// an indirection in order to read/write it.                              
   ///   @tparam ID - ID of the heap to track capacity for                    
   ///   @tparam T - type of the counter                                      
   template<unsigned ID, class T>
   struct ReserveEmergent {
      using CTTI_Component = Yes<>;
      using ReserveType = T;
      static constexpr int ComponentPrecedence = 1000;

      /// Get the number of reserved (maybe uninitialized) elements           
      template<CT::Container C>
      T GetReserved(this const C& self) noexcept {
         auto allocation = self.GetAllocation();
         return allocation ? allocation->GetFrontendSize() / self.GetStride() : 0;
      }

      /// Reserve a number of elements without initializing them.             
      /// If reserved data is smaller than currently initialized count, the   
      /// excess elements will be dereferenced/destroyed.                     
      ///   @param count - number of elements to reserve                      
      template<CT::Container C>
      C& Reserve(this C& self, const T count) {
         if (count < self.GetCount())
            self.AllocateLess(count);
         else
            self.AllocateMore(count);
         return self;
      }
   };
}
