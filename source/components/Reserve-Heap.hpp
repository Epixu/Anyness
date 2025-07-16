#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// A dynamic reserve derived from the heap directly                       
   /// As such, it will not increase container's stack size                   
   ///   @tparam ID - ID of the heap to track capacity for                    
   ///   @tparam T - type of the counter                                      
   ///                                                                        
   template<unsigned ID = 0, class T = ::std::size_t>
   struct ReserveHeap {
      using CTTI_Component = Yes<>;
      using ReserveType = T;

      /// Get the number of reserved (maybe uninitialized) elements           
      template<CT::Container C>
      T GetReserved(this const C& self) noexcept {
         auto allocation = self.GetAllocation();
         return allocation ? allocation->GetFrontendSize() : 0;
      }

      /// Reserve a number of elements without initializing them              
      /// If reserved data is smaller than currently initialized count, the   
      /// excess elements will be dereferenced/destroyed                      
      ///   @param count - number of elements to reserve                      
      template<CT::Container C>
      C& Reserve(this C& self, const T count) {
         if (count < self.GetCount())
            self.AllocateLess(count);
         else
            self.AllocateMore(count);
         return self;
      }
      
   protected:
      template<unsigned>
      friend struct HeapMovable;

      /// Set number of reserved elements is impossible - we always use what  
      /// the allocation says                                                 
      constexpr void SetReserved(ReserveType) const noexcept { LANGULUS(NOOP); }
   };

} // namespace Langulus::Anyness::Component
