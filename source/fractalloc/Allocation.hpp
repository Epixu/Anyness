///                                                                           
/// Langulus::Fractalloc                                                      
/// Copyright (c) 2015 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include <Langulus/Core.hpp>

#if not LANGULUS_FEATURE(MANAGED_MEMORY)
namespace Langulus::Unmanaged
{
   struct Allocator;
}
#endif

namespace Langulus::Fractalloc
{
   #if LANGULUS_FEATURE(MANAGED_MEMORY)
      class Pool;
   #else
      class MallocHandle;
   #endif

   ///                                                                        
   ///   Memory allocation                                                    
   ///                                                                        
   struct alignas(Alignment) Allocation {
   protected:
   #if LANGULUS_FEATURE(MANAGED_MEMORY)
      friend class Pool;
      friend struct Allocator;
   #else
      friend struct Unmanaged::Allocator;
   #endif

      // The number of references to this memory.                       
      // Most often used, so first for immediate access.                
      int mReferences = 1;

      // Allocated bytes for this chunk                                 
      size_t mAllocatedBytes;

      // This pointer has three uses, depending on mReferences:         
      // If mReferences > 0 and MANAGED_MEMORY is enabled, it points    
      //    to the pool this allocation was allocated in.               
      // If mReferences > 0 and MANAGED_MEMORY is disabled, it          
      //    refers to the handle for std::free().                       
      // If mReferences == 0, it refers to the next free entry to be    
      //    reused.                                                     
      union {
         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            Pool* mPool;
         #else
            MallocHandle* mMallocHandle;
         #endif

         Allocation* mNextFreeEntry;
      };

   #if LANGULUS_FEATURE(MEMORY_STATISTICS)
      // Acts like a timestamp of when the allocation happened          
      unsigned mStep;
   #endif

   public:
       Allocation() = delete;
       Allocation(const Allocation&) = delete;
       Allocation(Allocation&&) = delete;
      ~Allocation() = delete;

   #if LANGULUS_FEATURE(MANAGED_MEMORY)
      explicit Allocation(size_t, Pool*) noexcept;
   #else
      explicit Allocation(size_t, MallocHandle*) noexcept;
   #endif

      /*static consteval size_t GetHeaderSize() noexcept;
      static size_t GetMinAllocation(size_t align) noexcept;
      static size_t GetNewAllocationSize(size_t align, size_t size) noexcept;*/

      auto GetUses() const noexcept { return mReferences; }
      auto GetBackendSize() const noexcept -> size_t;
      auto GetFrontendSize() const noexcept -> size_t;
      auto GetBlockStart() const noexcept -> uint8_t*;
      auto GetBlockEnd() const noexcept -> uint8_t const*;
      bool Contains(const void*) const noexcept;
      void Keep(int = 1) noexcept;
      void Free(int = 1) noexcept;
   };
}

//#include "Allocation.inl"
