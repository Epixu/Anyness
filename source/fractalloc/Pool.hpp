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


namespace Langulus::Fractalloc
{
   struct Allocation;
   struct PoolBank;
   
   ///                                                                        
   ///   Memory pool                                                          
   ///                                                                        
   /// Aligned to a dynamically determined cache size, with the following     
   /// data structure:                                                        
   /// [pool data][padding][allocation data...][padding][client data...]      
   struct Pool {
   protected:
      friend struct Allocator;
      friend struct PoolBank;
      
      // A chain of freed entries in the range [0; mEntries)            
      Allocation* mLastFreed = nullptr;
      // Allocation table                                               
      Allocation* mAllocationData;
      // The size distribution of entries                               
      //    @attention this is indexed by log2(entry->mSize)            
      size_t mDistribution[sizeof(size_t) * 8] = {};
      // Pointer to start of client data                                
      uint8_t* const mClientData;
      // Set by meta.GetAlignment()                                     
      pot_t mDataAlignment;
      // Set by meta.GetMinAlloc()                                      
      pot_t mDataMinAlloc;
      // Id of pool inside a type chain (used for packing pointers)     
      unsigned mID = 0;
      // Next pool in the pool chain                                    
      Pool* mNext = nullptr;

      // Bytes allocated by the frontend                                
      size_t mAllocatedByFrontend = 0;
      // An index that guarantees a new unused entry                    
      size_t mNextEntry = 0;
      // Keeps track of how many entries are currently in use           
      size_t mValidEntries = 0;
      // An entry larger than the next allowed mThresholdMax will clog  
      // the pool, until it is freed.                                   
      bool mClogged = false;

      #if LANGULUS_FEATURE(MEMORY_STATISTICS)
         // Acts like a timestamp of when the allocation happened       
         size_t mStep;
      #endif

      // Bytes allocated by the backend (aka the reserved client bytes) 
      const pot_t mAllocatedByBackend;
      // Alignment used when allocating entries                         
      const pot_t mAlign;
      // Alignment used when allocating entries                         
      const pot_t mPoolAlignment;
      // Smallest allocation possible for the pool                      
      const pot_t mThresholdMin;

      // Current threshold, that is, max allowed size of a new entry    
      pot_t mThresholdMax;
      // Currently the biggest allocation present in the pool           
      //    @attention this is provided by entry->mSize                 
      pot_t mBiggestEntry;
      // The biggest possible amount of entries                         
      const pot_t mMaxEntries;

   IF_LANGULUS_TESTING(public:)
      Pool() = delete;
      Pool(const Pool&) = delete;
      Pool(Pool&&) = delete;

      Pool(
         pot_t data_alignment,
         pot_t data_min_alloc,
         pot_t pool_alignment,
         pot_t client_size
      ) has_assumptions;

      static size_t Cost(pot_t dataAlignment, pot_t dataMinAlloc, pot_t) noexcept;

      auto GetAllocationData() const noexcept -> Allocation*;
      auto GetLastFreedEntry() const noexcept -> Allocation*;
      auto GetClientData() const noexcept -> uint8_t*;

      auto GetMaxEntries() const noexcept -> pot_t;
      auto GetCurrentEntries() const noexcept -> size_t;
      auto GetValidEntries() const noexcept -> size_t;
      auto GetMinAllocation() const noexcept -> pot_t;
      auto GetTotalSize() const noexcept -> size_t;
      auto GetAllocatedByBackend() const noexcept -> pot_t;
      auto GetAllocatedByFrontend() const noexcept -> size_t;
      bool IsInUse() const noexcept;
      bool CanContain(pot_t) const noexcept;
      bool ContainsData(const void*) const noexcept;
      bool ContainsAllocation(const Allocation*) const noexcept;
      auto Find(const void*) const has_assumptions -> const Allocation*;

      auto Allocate(pot_t) has_assumptions -> Allocation*;
      bool Reallocate(Allocation*, pot_t) has_assumptions;
      void Deallocate(Allocation*) has_assumptions;
      
      auto ThresholdFromIndex(size_t) const noexcept -> pot_t;
      auto IndexFromAddress(const void*) const has_assumptions -> size_t;
      auto UpIndex(size_t) const noexcept -> size_t;
      auto AllocationFromIndex(size_t) const noexcept -> Allocation*;
      auto AllocationFromAddress(const void*) const has_assumptions -> Allocation*;

      void FreePoolChain();
      void Null();
      void Touch();
      void Trim();
   };
   
   /// Fast log2                                                              
   /// https://stackoverflow.com/questions/11376288                           
   LANGULUS(ALWAYS_INLINED)
   constexpr size_t FastLog2(const size_t x) noexcept {
      if (x < 2)
         return 0;
      return size_t {8 * sizeof(size_t)} - ::std::countl_zero(x) - size_t {1};
   }
}
