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
   /// an indirection (and a division) in order to read it.                   
   ///   @tparam ID provider ID to keep reserve of                            
   ///   @tparam T the reserve type                                           
   ///   @tparam SHARED provider IDs that share the same reserve variable     
   template<Cid ID, class T, Cid...SHARED>
   struct ReserveEmergent {
      using CTTI_Component = Yes<>;
      using ReserveType = T;

      static constexpr Cid Id = ID;
      static constexpr int ComponentPrecedence = -1000;

      static_assert(CT::Integer<T> and not CT::Signed<T>,
         "Reserve type must be an unsigned integer");

      /// Get the number of reserved (maybe uninitialized) elements           
      template<Cid SID = ID, CT::Container C>
      constexpr T GetReserved(this const C& self) noexcept {
         static_assert(SID == ID or ((SID == SHARED) or ...));
         if constexpr (requires { self.template GetAllocation<SID>(); }) {
            const auto al = self.template GetAllocation<SID>();
            if (not al)
               return 0;

            if constexpr (CT::ContainsOne<C>)
               return 1;
            else {
               static_assert(C::CountHeapFooterRequests() == 0,
                  "ReserveEmergent can't be used in containers with heap footer, "
                  "because it causes a circular dependency - "
                  "reserved count can't be calculated without knowing the "
                  "reserved elements beforehand."
               );

               const size_t header = self.template GetHeapHeaderSize<SID>();
               return (al->GetSize() - header) / self.template GetStride<SID>();
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
      ///   @param reserve number of elements to reserve                      
      template<Cid SID = ID, CT::ContainsMany C>
      C& Reserve(this C& self, const T reserve) {
         static_assert(SID == ID or ((SID == SHARED) or ...));
         if (reserve < self.template GetCount<SID>())
            self.template AllocateLess<SID>(reserve);
         else
            self.template AllocateMore<SID>(reserve);
         return self;
      }
   };
}
