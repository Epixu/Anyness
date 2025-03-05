#pragma once
#include <Langulus/Fractalloc/Allocation.hpp>


namespace Langulus::Anyness::Component
{

   template<unsigned HEAP_ID = 0>
   struct AllocationStack {
   private:
      Fractalloc::Allocation* mAllocation;

   public:
      using CTTI_Component = Yes;
   };

} // namespace Langulus::Anyness::Component
