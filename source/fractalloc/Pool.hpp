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
   /// Aligned to a dynamically determined cache size, with the following     
   /// data structure:                                                        
   /// [pool data][padding][allocation data...][padding][client data...]      
   struct Pool {
   protected:
      friend struct Allocator;
      
      // A chain of freed entries in the range [0; mEntries)            
      Allocation* mLastFreed = nullptr;
      // Allocation table                                               
      Allocation* mAllocationData;
      // The size distribution of entries                               
      //    @attention this is indexed by log2(entry->mSize)            
      size_t mDistribution[sizeof(size_t) * 8] = {};
      // Pointer to start of client data                                
      uint8_t* const mClientData;
      // Next pool in the pool chain                                    
      Pool* mNext = nullptr;

      // Bytes allocated by the frontend                                
      size_t mAllocatedByFrontend = 0;
      // An index that guarantees a new unused entry                    
      size_t mNextEntry = 0;
      bool mClogged = false;

      #if LANGULUS_FEATURE(MEMORY_STATISTICS)
         // Acts like a timestamp of when the allocation happened       
         size_t mStep;
         // Keeps track of how many entries are currently in use        
         size_t mValidEntries = 0;
      #endif
      
      // Associated meta data                                           
      DMeta mMeta;

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

      Pool(DMeta, pot_t pool_alignment, pot_t client_size) has_assumptions;

      static size_t Cost(DMeta, pot_t) noexcept;

      auto GetAllocationData() const noexcept -> Allocation*;
      auto GetClientData() const noexcept -> uint8_t*;

      auto GetMaxEntries() const noexcept -> pot_t;
      auto GetMinAllocation() const noexcept -> pot_t;
      auto GetTotalSize() const noexcept -> size_t;
      auto GetAllocatedByBackend() const noexcept -> pot_t;
      auto GetAllocatedByFrontend() const noexcept -> size_t;
      bool IsInUse() const noexcept;
      bool CanContain(pot_t) const noexcept;
      bool Contains(const void*) const noexcept;
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
}
