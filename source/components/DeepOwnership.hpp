#pragma once
#include "../Container.hpp"
#include <Langulus/Assume.hpp>


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Reserves a part of the heap to keep track of sparse element's          
   /// allocations                                                            
   ///   @tparam ID - which heap are we keeping track of?                     
   ///                                                                        
   template<unsigned ID = 0>
   struct DeepOwnership {
      using CTTI_Component = Yes;

   protected:
      template<unsigned>
      friend struct HeapMovable;

      template<CT::Container C>
      using View = typename C::ViewType;
      template<CT::Container C>
      using Count = typename C::CountType;

      /// Get entry array if containing pointers                              
      ///   @attention entries exist only for sparse containers               
      ///   @return the array of entries                                      
      template<CT::Container C>
      auto GetEntry(this C&& self) has_assumptions -> AllocationPtr* {
         AssumeDev(self.IsSparse(), HERE(),
            "Entries do not exist for dense container");
         AssumeDev(self.GetAllocation(), HERE(),
            "Entries do not exist for sparse containers which are out of jurisdiction");
         AssumeDev(self.GetHeap(), HERE(),
            "No memory available");

         return reinterpret_cast<AllocationPtr*>(self.GetHeapEnd());
      }

      /// This function is called for all container components when the       
      /// allocation changes to update any heap-allocated data pointers       
      template<CT::Container C>
      void OnAllocationChange(this C& self, const View<C>& oldv) {
         AssumeDev(self.GetAllocation() != oldv.GetAllocation(), HERE(),
            "Allocation didn't change");

         if constexpr (C::Sparse) {
            // Move entry data to its new place                         
            MoveMemory(self.GetEntry(), oldv.GetEntry(), self.GetCount());
         }
      }
   };

} // namespace Langulus::Anyness::Component
