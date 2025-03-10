#pragma once
#if LANGULUS_FEATURE(MANAGED_MEMORY)
   #include "fractalloc/Allocator.hpp"
#else
   #include "unmanaged/Allocator.hpp"
#endif


namespace Langulus::Anyness
{

   using Fractalloc::Allocation;

   #if LANGULUS_FEATURE(MANAGED_MEMORY)
      using Fractalloc::Allocator;
      using Fractalloc::Byte;
   #else
      using Unmanaged::Allocator;
      using Unmanaged::Byte;
   #endif

} // namespace Langulus::Anyness