///                                                                           
/// Langulus::Fractalloc                                                      
/// Copyright (c) 2015 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#if not LANGULUS_FEATURE(MANAGED_MEMORY)
#error This file shouldn't be included if MANAGED_MEMORY is disabled
#endif

#include "Allocation.hpp"
#include "../rtti/MetaData.hpp"


namespace Langulus::Fractalloc
{

   using RTTI::DMeta;


   ///                                                                        
   ///   Memory pool                                                          
   ///                                                                        
   class Pool final {
   friend struct Allocator;
   protected:
      // Bytes allocated by the backend                                 
      const Size mAllocatedByBackend {};
      const Size mAllocatedByBackendLog2 {};
      const Size mAllocatedByBackendLSB {};

      // Bytes allocated by the frontend                                
      Size mAllocatedByFrontend {};
      // Number of entries that have been used overall                  
      Size mEntries {};
      // A chain of freed entries in the range [0-mEntries)             
      Allocation* mLastFreed {};
      // Current threshold, that is, max size of a new entry            
      Size mThreshold {};
      Size mThresholdPrevious {};
      // Smallest allocation possible for the pool                      
      Size mThresholdMin {};
      // Pointer to start of usable memory                              
      Byte* mMemory {};
      Byte* mMemoryEnd {};
      // Associated meta data, when types are reflected with nondefault 
      // PoolTactic                                                     
      DMeta mMeta {};
      // Handle for the pool allocation, for use with ::std::free       
      void* mHandle {};

      // Next pool in the pool chain                                    
      Pool* mNext {};

   #if LANGULUS_FEATURE(MEMORY_STATISTICS)
      // Acts like a timestamp of when the allocation happened          
      Size mStep;
      Size mValidEntries {};
   #endif

   public:
      Pool() = delete;
      Pool(const Pool&) = delete;
      Pool(Pool&&) = delete;
      ~Pool() = delete;

      Pool(DMeta, Size, void*) noexcept;

      // Default pool allocation is 1 MB                                
      static constexpr Size DefaultPoolSize = 1024 * 1024;
      static constexpr Size InvalidIndex = ::std::numeric_limits<Size>::max();

   public:
      static constexpr Size GetSize() noexcept;
      static constexpr Size GetNewAllocationSize(Size) noexcept;

      auto GetPoolStart() const noexcept -> Byte*;

      constexpr Size GetMinAllocation() const noexcept;
      constexpr Size GetTotalSize() const noexcept;
      constexpr Size GetMaxEntries() const noexcept;
      constexpr Size GetAllocatedByBackend() const noexcept;
      constexpr Size GetAllocatedByFrontend() const noexcept;
      constexpr bool IsInUse() const noexcept;
      constexpr bool CanContain(Size) const noexcept;
      bool Contains(const void*) const noexcept;
      auto Find(const void*) const has_assumptions -> const Allocation*;

      auto Allocate(Size) has_assumptions -> Allocation*;
      bool Reallocate(Allocation*, Size) has_assumptions;
      void Deallocate(Allocation*) has_assumptions;
      void FreePoolChain();
      void Null();
      void Touch();
      void Trim();

      Size ThresholdFromIndex(Size) const noexcept;
      Size IndexFromAddress(const void*) const has_assumptions;
      Size ValidateIndex(Size) const noexcept;
      Size UpIndex(Size) const noexcept;
      auto AllocationFromIndex(Size) const noexcept -> const Allocation*;
      auto AllocationFromAddress(const void*) const has_assumptions -> const Allocation*;
   };

} // namespace Langulus::Fractalloc
