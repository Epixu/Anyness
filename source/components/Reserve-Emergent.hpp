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
   /// an indirection (and a division) in order to read/write it.             
   ///   @tparam ID ID of the heap to track capacity for                      
   ///   @tparam T type of the counter                                        
   template<unsigned ID, class T>
   struct ReserveEmergent {
      using CTTI_Component = Yes<>;
      using ReserveType = T;
      static constexpr int ComponentPrecedence = -1000;

      /// Get the number of reserved (maybe uninitialized) elements           
      template<CT::Container C>
      constexpr T GetReserved(this const C& self) noexcept {
         if constexpr (requires { self.GetAllocation(); }) {
            const auto al = self.GetAllocation();
            if (not al)
               return 0;

            if constexpr (CT::ContainsOne<C>) {
               // Compile-time benefit for statically sized containers  
               return 1;
            }
            else {
               const size_t header = self.GetHeapHeaderSize(
                  self.GetCount(), self.GetIndirections());

               if constexpr (CT::TypeErased<C>) {
                  const auto type = self.GetType();
                  LglsAssumeDev(type, "Requesting allocation size for an untyped container");
                  return (al->GetSize() - Align(header, type.GetAlignment())) / type.GetSize();
               }
               else {
                  using type = TypeOf<C>;
                  return (al->GetSize() - Align(header, alignof(type))) / sizeof(type);
               }
            }
         }
         else if constexpr (CT::ContainsOne<C>)
            return 1;
         else {
            static_assert(false,
               "Emergent reserve can't derive the amount of reserved items, "
               "because container supporting multiple elements "
               "has no ownership component"
            );
         }
      }

      /// Reserve a number of elements without initializing them.             
      /// If reserved data is smaller than currently initialized count, the   
      /// excess elements will be dereferenced/destroyed.                     
      ///   @param count number of elements to reserve                        
      template<CT::ContainsMany C>
      C& Reserve(this C& self, const T count) {
         if (count < self.GetCount())
            self.AllocateLess(count);
         else
            self.AllocateMore(count);
         return self;
      }
   };
}
