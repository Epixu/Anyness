///                                                                           
/// Langulus::Fractalloc                                                      
/// Copyright (c) 2015 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Pool.hpp"

#if not LANGULUS_FEATURE(MANAGED_MEMORY)
   #error "This file shouldn't be included if MANAGED_MEMORY is disabled"
#endif


namespace Langulus::Fractalloc
{
   /// Fast log2                                                              
   /// https://stackoverflow.com/questions/11376288                           
   LANGULUS(ALWAYS_INLINED)
   constexpr size_t FastLog2(const size_t x) noexcept {
      return x < 2 ? 0 : size_t {8 * sizeof(size_t)} - ::std::countl_zero(x) - size_t {1};
   }
   
   /// Get least significant bit                                              
   /// https://stackoverflow.com/questions/757059                             
   LANGULUS(ALWAYS_INLINED)
   constexpr uint32_t LSB(const uint32_t n) noexcept {
      constexpr uint32_t DeBruijnBitPosition[32] = {
          0,  1, 28,  2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17,  4, 8,
         31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18,  6, 11,  5, 10, 9
      };
      constexpr uint32_t f = 0x077CB531u;
      return DeBruijnBitPosition[(uint32_t {n & (0 - n)} * f) >> uint32_t {27}];
   }

   LANGULUS(ALWAYS_INLINED)
   constexpr uint64_t LSB(const uint64_t n) noexcept {
      constexpr uint64_t DeBruijnBitPosition[64] = {
         0,   1,  2, 53,  3,  7, 54, 27,  4, 38, 41,  8, 34, 55, 48, 28,
         62,  5, 39, 46, 44, 42, 22,  9, 24, 35, 59, 56, 49, 18, 29, 11,
         63, 52,  6, 26, 37, 40, 33, 47, 61, 45, 43, 21, 23, 58, 17, 10,
         51, 25, 36, 32, 60, 20, 57, 16, 50, 31, 19, 15, 30, 14, 13, 12
      };
      constexpr uint64_t f = 0x022fdd63cc95386dul;
      return DeBruijnBitPosition[(uint64_t {n & (0 - n)} * f) >> uint64_t {58}];
   }

   /// Initialize a pool                                                      
   ///   @param meta - meta data associated with pool                         
   ///   @param poolAlignment - the alignment of the pool itself              
   ///   @param size - bytes of the usable block to initialize with           
   LANGULUS(INLINED)
   Pool::Pool(DMeta meta, pot_t poolAlignment, pot_t size) has_assumptions
      : mAllocationData {reinterpret_cast<Allocation*>(Align(reinterpret_cast<uintptr_t>(this + 1), alignof(Allocation)))}
      , mClientData     {reinterpret_cast<uint8_t*>(this) + Cost(meta, size)}
      , mMeta           {meta}
      , mAllocatedByBackend {size}
      , mAlign          {::std::max(meta.GetAlignment(), pot_t(Alignment))}
      , mPoolAlignment  {poolAlignment}
      , mThresholdMin   {::std::max(meta.GetMinAllocation(), mAlign)}
      , mThresholdMax   {size}
      , mBiggestEntry   {mThresholdMin}
      , mMaxEntries     {size / mThresholdMin}
   {
      LglsAssumeDevAndOptimize(meta,
         "Invalid type");
      LglsAssumeDev(size >= mThresholdMin,
         "Size must be able to hold at least one allocation");

      IF_LANGULUS_MEMORY_STATISTICS(mStep = Instance.GetStatistics().mStep);

      // Touching is mandatory for pools - without touching the         
      // memory, it might remain just a promise by the OS, making       
      // initial pool allocations very, very, VERY slow at the most     
      // inappropriate of times.                                        
      Touch();
   }
   
   /// Get the cost of allocating a pool - this includes sizeof(Pool), all    
   /// possible entry overhead, including padding for alignment               
   LANGULUS(INLINED)
   size_t Pool::Cost(DMeta type, pot_t size) noexcept {
      const pot_t align = ::std::max(type.GetAlignment(), pot_t(Alignment));
      const pot_t minAlloc = ::std::max(type.GetMinAllocation(), align);
      const pot_t maxEntries = size / minAlloc;
      return Align(
         Align(sizeof(Pool), alignof(Allocation)) + maxEntries * sizeof(Allocation),
         align
      );
   }

   /// Get the minimum allocation for an entry inside this pool               
   ///   @return the size in bytes, always a power-of-two                     
   LANGULUS(INLINED)
   pot_t Pool::GetMinAllocation() const noexcept {
      return mThresholdMin;
   }

   /// Get the total size of the pool, including this instance and padding    
   ///   @return the size in bytes                                            
   LANGULUS(INLINED)
   size_t Pool::GetTotalSize() const noexcept {
      return mAllocatedByBackend + Cost(mMeta, mAllocatedByBackend);
   }

   /// Get the max number of possible entries                                 
   /// (if all of them are as small as possible)                              
   ///   @return the size in bytes, always a power-of-two                     
   LANGULUS(INLINED)
   pot_t Pool::GetMaxEntries() const noexcept {
      return mAllocatedByBackend / mThresholdMin;
   }

   /// Free the whole pool chain                                              
   ///   @attention make sure this is called for the first pool in the chain  
   LANGULUS(INLINED)
   void Pool::FreePoolChain() {
      if (mNext)
         mNext->FreePoolChain();

      #if LANGULUS_COMPILER(MSVC) or LANGULUS_COMPILER(CLANG_CL)
         _aligned_free(this);
      #else
         ::std::free(this);
      #endif
   }

   /// Get the start of the allocation data                                   
   LANGULUS(INLINED)
   auto Pool::GetAllocationData() const noexcept -> Allocation* {
      return mAllocationData;
   }
   
   /// Get the start of the usable memory for the pool                        
   LANGULUS(INLINED)
   auto Pool::GetClientData() const noexcept -> uint8_t* {
      return mClientData;
   }

   /*LANGULUS(INLINED)
   auto Pool::GetPoolAlignment() const noexcept -> pot_t {
      return mPoolAlignment;
   }*/

   /*LANGULUS(INLINED)
   auto Pool::GetAlignment() const noexcept -> pot_t {
      return mMeta.GetAlignment();
   }*/

   /// Get the bytes reserved for the bool                                    
   ///   @return bytes allocated for the pool                                 
   LANGULUS(INLINED)
   pot_t Pool::GetAllocatedByBackend() const noexcept {
      return mAllocatedByBackend;
   }

   /// Get the used number of bytes - the sum of all allocations              
   ///   @return bytes allocated by the client                                
   LANGULUS(INLINED)
   size_t Pool::GetAllocatedByFrontend() const noexcept {
      return mAllocatedByFrontend;
   }

   /// Allocate an entry inside the pool                                      
   ///   @param bytes - number of bytes to allocate                           
   ///   @return the new allocation, or nullptr if pool is full               
   inline auto Pool::Allocate(pot_t bytes) has_assumptions -> Allocation* {
      // Check if we can add a new entry                                
      if (mThresholdMin > bytes)
         bytes = mThresholdMin;
      if (not CanContain(bytes))
         return nullptr;

      Allocation* newEntry;
      if (mLastFreed) {
         // Recycle entries                                             
         newEntry = mLastFreed;
         mLastFreed = mLastFreed->GetNextFreeEntry();
         new (newEntry) Allocation {bytes, mPoolAlignment};
      }
      else {
         // The entire pool is full or empty, skip search for free      
         // spot, add a new allocation directly	instead                 
         newEntry = AllocationFromIndex(mNextEntry);
         new (newEntry) Allocation {bytes, mPoolAlignment};

         ++mNextEntry;

         if (::std::has_single_bit(mNextEntry)) {
            // Next entry will move to the next level                   
            mThresholdMax >>= 1u;
         }
      }

      // Update the distribution                                        
      if (bytes > mBiggestEntry)
         mBiggestEntry = bytes;
      ++mDistribution[bytes.bit];
      LglsAssumeDev(
         mAllocatedByFrontend + static_cast<size_t>(bytes) > mAllocatedByFrontend,
         "mAllocatedByFrontend overflowed");
      mAllocatedByFrontend += static_cast<size_t>(bytes);
      IF_LANGULUS_MEMORY_STATISTICS(++mValidEntries);
      return newEntry;
   }

   /// Resize an entry                                                        
   ///   @param entry - entry to resize                                       
   ///   @param bytes - new number of bytes                                   
   ///   @return true if entry was enlarged without conflict                  
   inline bool Pool::Reallocate(Allocation* entry, pot_t bytes) has_assumptions {
      LglsAssumeDev(entry >= mAllocationData
                and entry < mAllocationData + static_cast<size_t>(mMaxEntries)
                and entry->GetUses(),
         "Invalid deallocation");
      
      if (mThresholdMin > bytes)
         bytes = mThresholdMin;
      
      if (bytes > entry->GetSize()) {
         // We're enlarging the entry                                   
         // Make sure we don't violate max threshold                    
         if (bytes > mThresholdMax)
            return false;

         // Update the distribution                                     
         if (bytes > mBiggestEntry)
            mBiggestEntry = bytes;
         mAllocatedByFrontend += bytes - entry->GetSize();

         size_t it = entry->mSize;
         LglsAssumeDev(mDistribution[it], "Distribution underflow");
         --mDistribution[it];
         ++mDistribution[bytes.bit];
      }
      else {
         // We're shrinking the entry                                   
         // No checks required, just update the distribution            
         const size_t removal = entry->GetSize() - bytes;
         LglsAssumeDevAndOptimize(mAllocatedByFrontend >= removal,
            "mAllocatedByFrontend underflowed");
         mAllocatedByFrontend -= removal;

         size_t it = entry->mSize;
         LglsAssumeDev(mDistribution[it], "Distribution underflow");
         --mDistribution[it];
         ++mDistribution[bytes.bit];
         if (mBiggestEntry == entry->GetSize() and 0 == mDistribution[it]) {
            // All biggest entries have been removed and we can safely  
            // increase mThresholdMax, so collisions are less likely    
            do { mThresholdMax <<= 1u; }
            while (not mDistribution[--it]);
            mBiggestEntry.bit = it;
         }
      }

      entry->mSize = bytes.bit;
      return true;
   }

   /// Remove an entry                                                        
   ///   @attention assumes entry is valid                                    
   ///   @param entry - entry to remove                                       
   inline void Pool::Deallocate(Allocation* entry) has_assumptions {
      LglsAssumeDev(entry >= mAllocationData
                and entry < mAllocationData + static_cast<size_t>(mMaxEntries)
                and entry->GetUses(),
         "Invalid deallocation");
      LglsAssumeDevAndOptimize(mNextEntry,
         "Bad valid entry count");
      LglsAssumeDev(mAllocatedByFrontend >= entry->GetSize(),
         "Bad frontend allocation size");

      mAllocatedByFrontend -= static_cast<size_t>(entry->GetSize());
      entry->mReferences = 0;

      if (0 == mAllocatedByFrontend) {
         // The freed entry was the last used entry.                    
         // Reset the entire pool.                                      
         mThresholdMax = mAllocatedByBackend;
         mBiggestEntry = mThresholdMin;
         mLastFreed = nullptr;
         mNextEntry = 0;
         mDistribution[entry->mSize] = 0;
         #if LANGULUS_FEATURE(MEMORY_STATISTICS)
            LglsAssumeDev(mValidEntries == 1, "Incorrect mValidEntries");
            mValidEntries = 0;
         #endif
      }
      else {
         // Update the distribution                                     
         size_t it = entry->mSize;
         --mDistribution[it];
         if (mBiggestEntry == entry->GetSize() and 0 == mDistribution[it]) {
            // All biggest entries have been removed and we can safely  
            // increase mThresholdMax, so collisions are less likely    
            do { mThresholdMax <<= 1u; }
            while (not mDistribution[--it]);
            mBiggestEntry.bit = it;
         }

         // Push the removed entry to the last freed list.              
         // The removed entry becomes the last freed entry, and its     
         // pool pointer becomes a jump to the previous last freed.     
         entry->mNextFreeEntryFinder = mLastFreed - entry;
         mLastFreed = entry;
         #if LANGULUS_FEATURE(MEMORY_STATISTICS)
            LglsAssumeDev(mValidEntries > 1, "Incorrect mValidEntries");
            --mValidEntries;
         #endif
      }
   }

   /// Get valid entry that corresponds to an arbitrary pointer               
   ///   @attention assumes ptr is inside pool                                
   ///   @param ptr - the pointer to get the element index of                 
   ///   @return pointer to the valid allocation, or nullptr if unused        
   LANGULUS(INLINED)
   auto Pool::AllocationFromAddress(const void* ptr) const has_assumptions -> Allocation* {
      //LglsAssumeDev(mNextEntry != 0, "Pool shouldn't be empty");

      // Step up until a valid entry inside bounds is hit               
      auto index = IndexFromAddress(ptr);
      while (index != 0
        and (index >= mNextEntry or 0 == AllocationFromIndex(index)->GetUses()))
         index = UpIndex(index);

      // Check if we reached root of pool and it is unused              
      if (index == 0 and 0 == mAllocationData->GetUses())
         return nullptr;
      
      return AllocationFromIndex(index);
   }

   /// Check if there is any used memory                                      
   ///   @return true on at least one valid entry                             
   LANGULUS(INLINED)
   bool Pool::IsInUse() const noexcept {
      return mAllocatedByFrontend > 0;
   }

   /// Check if memory can contain a number of bytes                          
   ///   @attention assumes that bytes include any padding and overhead       
   ///   @param bytes - number of bytes to check                              
   ///   @return true if bytes can be contained in a new/recycled element     
   LANGULUS(INLINED)
   bool Pool::CanContain(pot_t bytes) const noexcept {
      return mThresholdMax >= mThresholdMin and bytes <= mThresholdMax;
   }

   /// Null the client data                                                   
   LANGULUS(INLINED)
   void Pool::Null() {
      memset(mClientData, 0, static_cast<size_t>(mAllocatedByBackend));
   }

   /// Touch client data                                                      
   /// https://stackoverflow.com/questions/18929011                           
   LANGULUS(INLINED)
   void Pool::Touch() {
      auto it = mClientData;
      const auto itEnd = mClientData + static_cast<size_t>(mAllocatedByBackend);
      while (it < itEnd) {
         volatile auto touch = *it;
         (void) touch;
         it += 4096;
      }
   }
   
   /// Remove all empty small entries at the end and increase threshold as    
   /// much as possible                                                       
   LANGULUS(INLINED)
   void Pool::Trim() {
      LglsAssumeDevAndOptimize(mNextEntry, "Should have at least one entry");
      constexpr size_t one = 1;
      Allocation* entry;
      size_t ecounter = mNextEntry;
      do {
         --ecounter;
         const size_t basePower = ::std::bit_width(ecounter) - 1;
         const size_t baselessIndex = ecounter - (one << basePower);
         const size_t levelIndex = (baselessIndex << one) + one;
         const size_t levelSize = (one << (mAllocatedByBackend.bit - mThresholdMin.bit - basePower - 1));
         entry = mAllocationData + levelIndex * levelSize;
         if (entry->mReferences)
            break;
      }
      while (ecounter > 0);

      mNextEntry = ecounter + 1;

      // Scan all unused entries up to mNextEntry and chain them        
      mLastFreed = nullptr;
      ecounter = 0;
      do {
         entry = AllocationFromIndex(ecounter);
         if (not entry->mReferences) {
            mLastFreed = entry;
            break;
         }
      } while (++ecounter < mNextEntry - 1);

      auto prev = mLastFreed;
      while (++ecounter < mNextEntry - 1) {
         entry = AllocationFromIndex(ecounter);
         if (entry->mReferences)
            continue;

         prev->mNextFreeEntryFinder = entry - prev;
         prev = entry;
      }

      if (prev)
         prev->mNextFreeEntryFinder = 0;

      mThresholdMax = ThresholdFromIndex(mNextEntry - 1);
   }

   /// Get threshold associated with an index                                 
   ///   @attention assumes index is not zero                                 
   ///   @param index - the index                                             
   ///   @return the threshold                                                
   LANGULUS(INLINED)
   pot_t Pool::ThresholdFromIndex(size_t index) const noexcept {
      pot_t result;
      result.bit = mAllocatedByBackend.bit - ::std::bit_width(index);
      return result;
   }

   /// Get allocation from index                                              
   ///   @param index - the index                                             
   ///   @return the allocation (not validated and constrained)               
   LANGULUS(INLINED)
   auto Pool::AllocationFromIndex(size_t index) const noexcept -> Allocation* {
      // Credit goes to Vladislav Penchev                               
      if (index == 0)
         return mAllocationData;

      constexpr size_t one = 1;
      const size_t basePower = ::std::bit_width(index) - 1;
      const size_t baselessIndex = index - (one << basePower);
      const size_t levelIndex = (baselessIndex << one) + one;
      const size_t levelSize = (one << (mMaxEntries.bit - basePower - 1));
      return mAllocationData + levelIndex * levelSize;
   }

   /// Get index from address                                                 
   ///   @attention assumes pointer is inside the pool                        
   ///   @param ptr - the address                                             
   ///   @return the index                                                    
   LANGULUS(INLINED)
   size_t Pool::IndexFromAddress(const void* ptr) const has_assumptions {
      LglsAssumeDev(Contains(ptr), "Pointer is outside pool");

      // Credit goes to Yasen Vidolov                                   
      const size_t i = static_cast<const uint8_t*>(ptr) - mClientData;
      if (i < mThresholdMax or 0 == mNextEntry)
         return 0;

      // We got the index, but it is not constrained to the pool        
      constexpr size_t one = 1;
      size_t i_clear_lsb = i & ~(i - one);
      size_t index = ((mAllocatedByBackend + i) / i_clear_lsb - one) >> one;
      while (index >= mNextEntry)
         index = UpIndex(index);
      return index;
   }

   /// Validate an index, check if corresponding to a valid allocation        
   /// or shift it up until one is found                                      
   ///   @param index - index to validate                                     
   ///   @returns the valid index, or InvalidIndex if invalid                 
   /*LANGULUS(INLINED)
   size_t Pool::ValidateIndex(size_t index) const noexcept {
      LglsAssumeDev(mNextEntry != 0, "Pool shouldn't be empty");

      // Step up until a valid entry inside bounds is hit               
      while (index != 0
        and (index >= mNextEntry or 0 == AllocationFromIndex(index)->GetUses()))
         index = UpIndex(index);

      // Check if we reached root of pool and it is unused              
      if (index == 0 and 0 == mAllocationData->GetUses())
         return InvalidIndex;
      return index;
   }*/

   /// Get index above another index                                          
   ///   @param index                                                         
   ///   @return index above the given one                                    
   LANGULUS(INLINED)
   size_t Pool::UpIndex(const size_t index) const noexcept {
      // Credit goes to Vladislav Penchev                               
      return index >> (LSB(index) + 1uz);
   }

   /// Check if a memory address resigns inside pool's range                  
   ///   @param address - address to check                                    
   ///   @return true if address belongs to this pool                         
   LANGULUS(INLINED)
   bool Pool::Contains(const void* address) const noexcept {
      return address >= mClientData
         and address < mClientData + static_cast<size_t>(mAllocatedByBackend);
   }

   /// Find a memory entry from pointer                                       
   ///   @param memory - memory pointer                                       
   ///   @return the memory entry that manages the memory pointer, or         
   ///      nullptr if memory is not ours, or is no longer used               
   LANGULUS(INLINED)
   auto Pool::Find(const void* memory) const has_assumptions -> const Allocation* {
      if (not Contains(memory))
         return nullptr;

      const auto entry = AllocationFromAddress(memory);
      return entry and entry->Contains(memory) ? entry : nullptr;
   }
}
