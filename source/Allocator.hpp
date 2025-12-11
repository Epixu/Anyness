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
   #if LANGULUS_FEATURE(MANAGED_MEMORY)
      using Fractalloc::Allocation;
      using Fractalloc::Allocator;
      using MemoryState = Fractalloc::State;
   #else
      using Unmanaged::Allocation;
      using Unmanaged::Allocator;
   
      struct MemoryState {
         consteval bool Assert() const noexcept { return true; }
      };
   #endif
   
   /// Can be a packed pointer                                                
   using AllocationPtr = Allocation*;

   /// Can be a packed pointer                                                
   using EntryPtr = AllocationPtr*;
}
