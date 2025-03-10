#pragma once
#include "../fractalloc/Allocation.hpp"


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Use the memory manager to extract the allocation from heap pointer     
   /// Manage its ownership                                                   
   ///   @tparam HEAP_ID - which heap are we keeping track of?                
   ///                                                                        
   template<unsigned HEAP_ID = 0>
   struct OwnershipHeap {
      using CTTI_Component = Yes;
   };

} // namespace Langulus::Anyness::Component
