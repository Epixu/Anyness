///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
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
   #else
      using Unmanaged::Allocator;
   #endif
}
