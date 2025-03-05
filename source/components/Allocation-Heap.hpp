#pragma once


namespace Langulus::Anyness::Component
{

   template<unsigned HEAP_ID = 0>
   struct AllocationHeap {
      using CTTI_Component = Yes;
   };

} // namespace Langulus::Anyness::Component
