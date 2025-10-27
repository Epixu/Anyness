///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include <Langulus/Core.hpp>

#if LANGULUS_FEATURE(MANAGED_MEMORY)
   #error "This file shouldn't be included if MANAGED_MEMORY is enabled"
#endif

namespace Langulus::Unmanaged
{
   class MallocHandle;
   struct Allocator;

   ///                                                                        
   ///   Memory allocation                                                    
   ///                                                                        
   struct alignas(Alignment) Allocation {
   protected:
      friend struct Allocator;

      // The number of references to this memory.                       
      // Most often used, so first for immediate access.                
      int mReferences = 1;
      // Allocated bytes for this chunk                                 
      size_t mAllocatedBytes;
      // The alignment of the contained data                            
      size_t mAlignment;
      // Refers to the handle for std::free().                          
      MallocHandle* mMallocHandle;

   #if LANGULUS_FEATURE(MEMORY_STATISTICS)
      // Acts like a timestamp of when the allocation happened          
      unsigned mStep;
   #endif

   public:
       Allocation() = delete;
       Allocation(const Allocation&) = delete;
       Allocation(Allocation&&) = delete;
      ~Allocation() = delete;

      explicit Allocation(size_t alignment, size_t size, MallocHandle*) noexcept;
      
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
