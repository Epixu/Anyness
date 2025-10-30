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
      // Next pool in the pool chain                                    
      Pool* mNext = nullptr;
      // A chain of freed entries in the range [0-mEntries)             
      Allocation* mLastFreed = nullptr;

      // Associated meta data, when types are reflected with nondefault 
      // PoolTactic                                                     
      DMeta mMeta;

      // Bytes allocated by the frontend                                
      size_t mAllocatedByFrontend;
      // Number of entries that have been used overall                  
      size_t mEntries = 0;

      #if LANGULUS_FEATURE(MEMORY_STATISTICS)
         // Acts like a timestamp of when the allocation happened       
         size_t mStep;
         // Keeps track of how many entries are currently in use        
         size_t mValidEntries = 0;
      #endif

      // Bytes allocated by the backend                                 
      const pot_t mAllocatedByBackend;
      // Alignment used when allocating entries                         
      const pot_t mAlign;
      // Smallest allocation possible for the pool                      
      const pot_t mThresholdMin;

      //const pot_t mAllocatedByBackendLSB {};
      // Current threshold, that is, max allowed size of a new entry    
      pot_t mThresholdMax;
      // Currently the biggest allocation present in the pool           
      //    @attention this is provided by entry->mSize                 
      pot_t mBiggestEntry;

      // Pointer to start of usable memory                              
      uint8_t* const mMemory;
      // Pointer to the end of usable memory                            
      uint8_t* const mMemoryEnd;

      // The size distribution of entries                               
      //    @attention this is indexed by log2(entry->mSize)            
      size_t mDistribution[sizeof(size_t) * 8] = {};

   public:
      Pool() = delete;
      Pool(const Pool&) = delete;
      Pool(Pool&&) = delete;

      Pool(DMeta) has_assumptions;
      Pool(DMeta, pot_t) has_assumptions;

      // Default pool allocation is 1 MB                                
      static constexpr size_t InvalidIndex = -1;

      auto GetPoolStart() const noexcept -> uint8_t*;
      auto GetPoolEnd() const noexcept -> uint8_t*;
      auto GetAlignment() const noexcept -> size_t { return mMeta.GetAlignment(); }

      constexpr auto GetMaxEntries() const noexcept -> pot_t;
      constexpr auto GetMinAllocation() const noexcept -> pot_t;
      constexpr auto GetTotalSize() const noexcept -> size_t;
      constexpr auto GetAllocatedByBackend() const noexcept -> pot_t;
      constexpr auto GetAllocatedByFrontend() const noexcept -> size_t;
      constexpr bool IsInUse() const noexcept;
      constexpr bool CanContain(pot_t) const noexcept;
      bool Contains(const void*) const noexcept;
      auto Find(const void*) const has_assumptions -> const Allocation*;

      auto Allocate(pot_t) has_assumptions -> Allocation*;
      bool Reallocate(Allocation*, pot_t) has_assumptions;
      void Deallocate(Allocation*) has_assumptions;
      void FreePoolChain();
      void Null();
      void Touch();
      void Trim();

      auto ThresholdFromIndex(size_t) const noexcept -> pot_t;
      auto IndexFromAddress(const void*) const has_assumptions -> size_t;
      auto ValidateIndex(size_t) const noexcept -> size_t;
      auto UpIndex(size_t) const noexcept -> size_t;
      auto AllocationFromIndex(size_t) const noexcept -> const Allocation*;
      auto AllocationFromAddress(const void*) const has_assumptions -> const Allocation*;
   };
}
