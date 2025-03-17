#pragma once
#if LANGULUS_FEATURE(MANAGED_MEMORY)
   #include "fractalloc/Allocator.hpp"
#else
   #include "unmanaged/Allocator.hpp"
#endif


namespace Langulus::Anyness
{

   /// Allocation is the same for managed and unmanaged builds                
   using Fractalloc::Allocation;

   /// Can be a packed pointer                                                
   using AllocationPtr = Fractalloc::Allocation*;

   #if LANGULUS_FEATURE(MANAGED_MEMORY)
      using Fractalloc::Allocator;
      using Fractalloc::Byte;
   #else
      using Unmanaged::Allocator;
      using Unmanaged::Byte;
   #endif

} // namespace Langulus::Anyness