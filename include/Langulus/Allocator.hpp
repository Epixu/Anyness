///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Core.hpp"

#if LANGULUS_FEATURE(MANAGED_MEMORY)
   #include "../source/fractalloc/Allocator.hpp"
   #include "../source/fractalloc/PackedPointer.hpp"
#else
   #include "../source/unmanaged/Allocator.hpp"
#endif


namespace Langulus
{
   #if LANGULUS_FEATURE(MANAGED_MEMORY)
      using Fractalloc::Allocation;
      using Fractalloc::Allocator;
      using MemoryState = Fractalloc::State;
      
      template<class T>
      using PackedPtr = Fractalloc::PackedPointer<T>;
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
