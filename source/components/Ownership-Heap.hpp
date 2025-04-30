#pragma once
#include "../fractalloc/Allocation.hpp"


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Use the memory manager to extract the allocation from heap pointer     
   /// Manage its ownership                                                   
   ///   @tparam ID - which heap are we keeping track of?                     
   ///   @tparam AUTO - whether ownership will be automatically used on       
   ///      construction/assignment. False if container is just a view, or in 
   ///      other cases where you want to carry an allocation pointer, but    
   ///      not necessarily reference it                                      
   ///                                                                        
   template<unsigned ID = 0, bool AUTO = true>
   struct OwnershipHeap {
      using CTTI_Component = Yes;
      static constexpr bool Owned = AUTO;

      /// Get the allocation                                                  
      template<CT::Container C>
      auto GetAllocation(this const C& self) noexcept -> AllocationPtr {
         return Allocator::Find(self.mType, *self.mSparseHeap);
      }

      /// Get the memory reference count                                      
      template<CT::Container C>
      auto GetUses(this const C& self) noexcept {
         auto allocation = self.GetAllocation();
         return allocation ? allocation->GetUses() : 0;
      }

   protected:
      void Keep() const noexcept;
      void Free() const noexcept;
   };

} // namespace Langulus::Anyness::Component
