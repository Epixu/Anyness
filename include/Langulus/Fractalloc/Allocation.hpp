///                                                                           
/// Langulus::Fractalloc                                                      
/// Copyright (c) 2015 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../Core.hpp"


namespace Langulus::Fractalloc
{

   class MallocHandle;
   class Pool;
   using Size = ::std::size_t;
   using Byte = ::std::uint8_t;


   ///                                                                        
   ///   Memory allocation                                                    
   ///                                                                        
   struct Allocation {
   protected:
      // The number of references to this memory                        
      // Most often used, so first for immediate access                 
      int mReferences = 1;

      // Allocated bytes for this chunk                                 
      Size mAllocatedBytes;

      // This pointer has three uses, depending on mReferences          
      // If mReferences > 0 and MANAGED_MEMORY is enabled, it points    
      //    to the pool this allocation was allocated in                
      // If mReferences > 0 and MANAGED_MEMORY is disabled, it          
      //    refers to the handle for std::free()                        
      // If mReferences == 0, it refers to the next free entry to be    
      //    reused                                                      
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
      explicit Allocation(Size, Pool*) noexcept;
   #else
      explicit Allocation(Size, MallocHandle*) noexcept;
   #endif

      static consteval Size GetHeaderSize() noexcept;
      static consteval Size GetMinAllocation() noexcept;
      static Size GetNewAllocationSize(Size) noexcept;

      auto GetUses() const noexcept { return mReferences; }
      Size GetBackendSize() const noexcept;
      Size GetFrontendSize() const noexcept;
      auto GetBlockStart() const noexcept -> Byte*;
      auto GetBlockEnd() const noexcept -> Byte const*;
      bool Contains(const void*) const noexcept;

      template<class T>
      T* As() const noexcept;

      void Keep(int = 1) noexcept;
      void Free(int = 1) noexcept;
   };

} // namespace Langulus::Fractalloc

#include "Allocation.inl"