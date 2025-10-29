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
   #error "This file shouldn't be included if MANAGED_MEMORY is disabled"
#endif

#include "Allocation.hpp"
#include "../rtti/MetaData.hpp"


namespace Langulus::Fractalloc
{
   using RTTI::DMeta;


   ///                                                                        
   ///   Memory pool                                                          
   ///                                                                        
   class alignas(Alignment) Pool final {
   friend struct Allocator;
   protected:
      // Bytes allocated by the backend                                 
      const size_t mAllocatedByBackend {};
      const size_t mAllocatedByBackendLog2 {};
      const size_t mAllocatedByBackendLSB {};

      // Bytes allocated by the frontend                                
      size_t mAllocatedByFrontend {};
      // Number of entries that have been used overall                  
      size_t mEntries {};
      // A chain of freed entries in the range [0-mEntries)             
      Allocation* mLastFreed {};
      // Current threshold, that is, max size of a new entry            
      size_t mThreshold {};
      size_t mThresholdPrevious {};
      // Smallest allocation possible for the pool                      
      size_t mThresholdMin {};
      // Pointer to start of usable memory                              
      uint8_t* mMemory {};
      uint8_t* mMemoryEnd {};
      // Associated meta data, when types are reflected with nondefault 
      // PoolTactic                                                     
      DMeta mMeta {};
      // Alignment used when allocating entries                         
      size_t mAlign = Alignment;
      // Next pool in the pool chain                                    
      Pool* mNext {};

   #if LANGULUS_FEATURE(MEMORY_STATISTICS)
      // Acts like a timestamp of when the allocation happened          
      size_t mStep;
      size_t mValidEntries {};
   #endif

   public:
      Pool() = delete;
      Pool(const Pool&) = delete;
      Pool(Pool&&) = delete;
      //~Pool() = delete;

      Pool(DMeta) has_assumptions;
      Pool(DMeta, size_t) has_assumptions;

      // Default pool allocation is 1 MB                                
      static constexpr size_t InvalidIndex = -1;

      auto GetPoolStart() const noexcept -> uint8_t*;
      auto GetPoolEnd() const noexcept -> uint8_t*;
      auto GetAlignment() const noexcept -> size_t { return mMeta.GetAlignment(); }

      constexpr auto GetMinAllocation() const noexcept -> size_t;
      constexpr auto GetTotalSize() const noexcept -> size_t;
      constexpr auto GetMaxEntries() const noexcept -> size_t;
      constexpr auto GetAllocatedByBackend() const noexcept -> size_t;
      constexpr auto GetAllocatedByFrontend() const noexcept -> size_t;
      constexpr bool IsInUse() const noexcept;
      constexpr bool CanContain(size_t) const noexcept;
      bool Contains(const void*) const noexcept;
      auto Find(const void*) const has_assumptions -> const Allocation*;

      auto Allocate(size_t) has_assumptions -> Allocation*;
      bool Reallocate(Allocation*, size_t) has_assumptions;
      void Deallocate(Allocation*) has_assumptions;
      void FreePoolChain();
      void Null();
      void Touch();
      void Trim();

      auto ThresholdFromIndex(size_t) const noexcept -> size_t;
      auto IndexFromAddress(const void*) const has_assumptions -> size_t;
      auto ValidateIndex(size_t) const noexcept -> size_t;
      auto UpIndex(size_t) const noexcept -> size_t;
      auto AllocationFromIndex(size_t) const noexcept -> const Allocation*;
      auto AllocationFromAddress(const void*) const has_assumptions -> const Allocation*;
   };
}
