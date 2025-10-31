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

   /// Initialize a pool of the default pool size used by 'meta'              
   ///   @param meta - data associated with pool                              
   ///   @param size - size requested by client                               
   LANGULUS(INLINED)
   Pool::Pool(DMeta meta) has_assumptions
      : Pool {meta, meta.GetMinPoolsize()} {}

   /// Initialize a pool with custom size                                     
   ///   @attention assumes that size is a power-of-two                       
   ///   @assumes size can contain at least one mMeta.GetMinAllocation()      
   ///      + Align(sizeof(Allocation), mMeta.GetAlignment())                 
   ///   @attention this constructor relies that instance is placed in the    
   ///      beginning of a heap allocation of size Pool::NewAllocationSize()  
   ///   @param meta - optional meta data associated with pool                
   ///   @param size - bytes of the usable block to initialize with           
   LANGULUS(INLINED)
   Pool::Pool(DMeta meta, pot_t size) has_assumptions
      : mMeta         {meta}
      , mAllocatedByBackend {size}
      , mAlign        {::std::max(meta.GetAlignment(), alignof(Allocation))}
      , mThresholdMin {Roof2(Allocation::Cost(mAlign) + static_cast<size_t>(mMeta.GetMinAllocation()))}
      , mThresholdMax {size}
      , mBiggestEntry {mThresholdMin}
      , mMemory       {GetPoolStart()}
      , mMemoryEnd    {mMemory + static_cast<size_t>(size)}
   //, mAllocatedByBackendLSB  {LSB(size >> size_t {1})}
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
   
   /// Get the cost of allocating a single allocation - this includes         
   /// sizeof(Pool) together with any padding for data alignment              
   LANGULUS(INLINED)
   size_t Pool::Cost(pot_t alignment) noexcept {
      return Align(sizeof(Pool), alignment);
   }

   /// Get the minimum allocation for an entry inside this pool               
   ///   @return the size in bytes, always a power-of-two                     
   LANGULUS(INLINED)
   constexpr pot_t Pool::GetMinAllocation() const noexcept {
      return mThresholdMin;
   }

   /// Get the total size of the pool, including this instance and padding    
   ///   @return the size in bytes                                            
   LANGULUS(INLINED)
   constexpr size_t Pool::GetTotalSize() const noexcept {
      return Cost(mAlign) + static_cast<size_t>(mAllocatedByBackend);
   }

   /// Get the max number of possible entries                                 
   /// (if all of them are as small as possible)                              
   ///   @return the size in bytes, always a power-of-two                     
   LANGULUS(INLINED)
   constexpr pot_t Pool::GetMaxEntries() const noexcept {
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

   /// Get the start of the usable memory for the pool                        
   ///   @return the start of the memory                                      
   LANGULUS(INLINED)
   auto Pool::GetPoolStart() const noexcept -> uint8_t* {
      const auto poolStart = reinterpret_cast<const uint8_t*>(this);
      return const_cast<uint8_t*>(poolStart + Cost(mAlign));
   }

   /// Get the bytes reserved for the bool                                    
   ///   @return bytes allocated for the pool                                 
   LANGULUS(INLINED)
   constexpr pot_t Pool::GetAllocatedByBackend() const noexcept {
      return mAllocatedByBackend;
   }

   /// Get the used number of bytes - the sum of all allocations              
   ///   @return bytes allocated by the client                                
   LANGULUS(INLINED)
   constexpr size_t Pool::GetAllocatedByFrontend() const noexcept {
      return mAllocatedByFrontend;
   }

   /// Allocate an entry inside the pool - returned pointer is aligned        
   ///   @param bytes - number of bytes to allocate                           
   ///   @return the new allocation, or nullptr if pool is full               
   inline auto Pool::Allocate(const pot_t bytes) has_assumptions -> Allocation* {
      // Check if we can add a new entry                                
      const size_t padding = Allocation::Cost(mAlign);
      const pot_t  minAlloc = mMeta.GetMinAllocation();
      const pot_t  resized = minAlloc > bytes ? minAlloc : bytes;
      const size_t request = Roof2(padding + static_cast<size_t>(resized));
      const pot_t  backendSize = pot_t(request);
      if (not CanContain(backendSize))
         return nullptr;

      Allocation* newEntry;
      if (mLastFreed) {
         // Recycle entries                                             
         newEntry = mLastFreed;
         mLastFreed = mLastFreed + mLastFreed->mNextFreeEntryFinder;
         new (newEntry) Allocation {resized, this};
      }
      else {
         // The entire pool is full or empty, skip search for free      
         // spot, add a new allocation directly	instead                 
         newEntry = const_cast<Allocation*>(AllocationFromIndex(mEntries));
         new (newEntry) Allocation {resized, this};

         ++mEntries;

         if (reinterpret_cast<uint8_t*>(newEntry)
           + static_cast<uintptr_t>(mThresholdMax) >= mMemoryEnd
         ) {
            // Next entry will go beyond the memory limits.             
            // Reset carriage to the beginning and narrow the threshold.
            mThresholdMax >>= 1u;
         }
      }

      // Update the distribution                                        
      if (resized > mBiggestEntry)
         mBiggestEntry = resized;
      ++mDistribution[resized.bit];
      LglsAssumeDevAndOptimize(
         mAllocatedByFrontend + request >= mAllocatedByFrontend,
         "mAllocatedByFrontend overflowed");
      mAllocatedByFrontend += request;
      IF_LANGULUS_MEMORY_STATISTICS(++mValidEntries);
      return newEntry;
   }

   /// Resize an entry                                                        
   ///   @param entry - entry to resize                                       
   ///   @param bytes - new number of bytes                                   
   ///   @return true if entry was enlarged without conflict                  
   inline bool Pool::Reallocate(Allocation* entry, const pot_t bytes) has_assumptions {
      LglsAssumeDev(Contains(entry) and entry->GetUses(),
         "Invalid reallocation");

      const pot_t minAlloc = mMeta.GetMinAllocation();
      const pot_t resized = minAlloc > bytes ? minAlloc : bytes;

      if (resized > entry->mSize) {
         // We're enlarging the entry                                   
         // Make sure we don't violate max threshold                    
         const size_t addition = resized - entry->mSize;
         const size_t prevsize = entry->GetBackendSize();
         const size_t newtotal = prevsize + addition;
         if (newtotal > mThresholdMax)
            return false;

         // Update the distribution                                     
         if (resized > mBiggestEntry)
            mBiggestEntry = resized;
         mAllocatedByFrontend += Roof2(newtotal) - prevsize;
         LglsAssumeDev(mDistribution[entry->mSize.bit],
            "Distribution underflow");
         --mDistribution[entry->mSize.bit];
         ++mDistribution[resized.bit];
      }
      else {
         // We're shrinking the entry                                   
         // No checks required, just update the distribution            
         const size_t removal = entry->mSize - resized;
         LglsAssumeDevAndOptimize(mAllocatedByFrontend >= removal,
            "mAllocatedByFrontend underflowed");
         size_t it = entry->mSize.bit;
         --mDistribution[it];
         ++mDistribution[resized.bit];
         mAllocatedByFrontend -= removal;

         if (mBiggestEntry == entry->mSize and 0 == mDistribution[it]) {
            // All biggest entries have been removed and we can safely  
            // increase mThresholdMax, so collisions are less likely    
            do { mThresholdMax <<= 1u; }
            while (not mDistribution[--it]);
            mBiggestEntry.bit = it;
         }
      }

      entry->mSize = resized;
      return true;
   }

   /// Remove an entry                                                        
   ///   @attention assumes entry is valid                                    
   ///   @param entry - entry to remove                                       
   inline void Pool::Deallocate(Allocation* entry) has_assumptions {
      LglsAssumeDev(Contains(entry) and entry->GetUses(),
         "Invalid deallocation");
      LglsAssumeDevAndOptimize(mEntries,
         "Bad valid entry count");
      LglsAssumeDev(mAllocatedByFrontend >= entry->GetBackendSize(),
         "Bad frontend allocation size");

      mAllocatedByFrontend -= entry->GetBackendSize();
      entry->mReferences = 0;

      if (0 == mAllocatedByFrontend) {
         // The freed entry was the last used entry.                    
         // Reset the entire pool.                                      
         mThresholdMax = mAllocatedByBackend;
         mBiggestEntry = mThresholdMin;
         mLastFreed = nullptr;
         mEntries = 0;
         mDistribution[entry->mSize.bit] = 0;
         #if LANGULUS_FEATURE(MEMORY_STATISTICS)
            LglsAssumeDev(mValidEntries == 1, "Incorrect mValidEntries");
            mValidEntries = 0;
         #endif
      }
      else {
         // Update the distribution                                     
         size_t it = entry->mSize.bit;
         --mDistribution[it];

         if (mBiggestEntry == entry->mSize and 0 == mDistribution[it]) {
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
   auto Pool::AllocationFromAddress(const void* ptr) const has_assumptions -> const Allocation* {
      const auto index = ValidateIndex(IndexFromAddress(ptr));
      return index == InvalidIndex ? nullptr : AllocationFromIndex(index);
   }

   /// Check if there is any used memory                                      
   ///   @return true on at least one valid entry                             
   LANGULUS(INLINED)
   constexpr bool Pool::IsInUse() const noexcept {
      return mAllocatedByFrontend > 0;
   }

   /// Check if memory can contain a number of bytes                          
   ///   @attention assumes that bytes include any padding and overhead       
   ///   @param bytes - number of bytes to check                              
   ///   @return true if bytes can be contained in a new/recycled element     
   LANGULUS(INLINED)
   constexpr bool Pool::CanContain(pot_t bytes) const noexcept {
      return mThresholdMax >= mThresholdMin and bytes <= mThresholdMax;
   }

   /// Null the memory                                                        
   LANGULUS(INLINED)
   void Pool::Null() {
      memset(mMemory, 0, static_cast<size_t>(mAllocatedByBackend));
   }

   /// Touch unused memory                                                    
   /// https://stackoverflow.com/questions/18929011                           
   LANGULUS(INLINED)
   void Pool::Touch() {
      auto it = mMemory;
      while (it < mMemoryEnd) {
         volatile auto touch = *it;
         (void) touch;
         it += 4096;
      }
   }
   
   /// Remove all empty entries at the end and increase threshold as much     
   /// as possible                                                            
   LANGULUS(INLINED)
   void Pool::Trim() {
      LglsAssumeDevAndOptimize(mEntries, "Should have at least one entry");
      const Allocation* entry;
      size_t ecounter = mEntries;
      do {
         entry = AllocationFromIndex(--ecounter); //TODO could be optimized further
         if (entry->mReferences)
            break;
      }
      while (ecounter > 0);

      mEntries = ecounter + 1;

      // Scan all unused entries up to mEntries and chain them          
      mLastFreed = nullptr;
      ecounter = 0;
      do {
         entry = AllocationFromIndex(ecounter);
         if (not entry->mReferences) {
            mLastFreed = const_cast<Allocation*>(entry);
            break;
         }
      } while (++ecounter < mEntries - 1);

      auto prev = mLastFreed;
      while (++ecounter < mEntries - 1) {
         entry = AllocationFromIndex(ecounter);
         if (entry->mReferences)
            continue;

         prev->mNextFreeEntryFinder = entry - prev; //TODO might be swapped, not sure
         prev = const_cast<Allocation*>(entry);
      }

      if (prev)
         prev->mNextFreeEntryFinder = 0;

      mThresholdMax = ThresholdFromIndex(mEntries - 1);
   }

   /// Get threshold associated with an index                                 
   ///   @attention assumes index is not zero                                 
   ///   @param index - the index                                             
   ///   @return the threshold                                                
   LANGULUS(INLINED)
   pot_t Pool::ThresholdFromIndex(size_t index) const noexcept {
      pot_t result;
      result.bit = mAllocatedByBackend.bit - ::std::bit_width(index); //FastLog2(index);
      return result;
   }

   /// Get allocation from index                                              
   ///   @param index - the index                                             
   ///   @return the allocation (not validated and constrained)               
   LANGULUS(INLINED)
   auto Pool::AllocationFromIndex(size_t index) const noexcept -> const Allocation* {
      // Credit goes to Vladislav Penchev                               
      if (index == 0)
         return reinterpret_cast<const Allocation*>(mMemory);

      constexpr size_t one = 1;
      const size_t basePower = ::std::bit_width(index) - 1; //FastLog2(index);
      const size_t baselessIndex = index - (one << basePower);
      const size_t levelIndex = (baselessIndex << one) + one;
      const size_t levelSize = (one << (mAllocatedByBackend.bit - basePower - 1));
      return reinterpret_cast<const Allocation*>(mMemory + levelIndex * levelSize);
   }

   /// Get index from address                                                 
   ///   @attention assumes pointer is inside the pool                        
   ///   @param ptr - the address                                             
   ///   @return the index                                                    
   LANGULUS(INLINED)
   size_t Pool::IndexFromAddress(const void* ptr) const has_assumptions {
      LglsAssumeDev(Contains(ptr), "Entry outside pool");

      // Credit goes to Yasen Vidolov (G1)                              
      const size_t i = static_cast<const uint8_t*>(ptr) - mMemory;
      if (i < mThresholdMax or 0 == mEntries)
         return 0;

      // We got the index, but it is not constrained to the pool        
      constexpr size_t one = 1;
      size_t index = ((mAllocatedByBackend + i) / (i & ~(i - one)) - one) >> one;
      while (index >= mEntries)
         index = UpIndex(index);
      return index;
   }

   /// Validate an index, check if corresponding to a valid allocation        
   /// or shift it up until one is found                                      
   ///   @param index - index to validate                                     
   ///   @returns the valid index, or InvalidIndex if invalid                 
   LANGULUS(INLINED)
   size_t Pool::ValidateIndex(size_t index) const noexcept {
      // Pool is empty, so search is pointless                          
      if (mEntries == 0)
         return InvalidIndex;

      // Step up until a valid entry inside bounds is hit               
      while (index != 0
        and (index >= mEntries or 0 == AllocationFromIndex(index)->GetUses()))
         index = UpIndex(index);

      // Check if we reached root of pool and it is unused              
      if (index == 0 and 0 == reinterpret_cast<const Allocation*>(mMemory)->GetUses())
         return InvalidIndex;
      return index;
   }

   /// Get index above another index                                          
   ///   @param index                                                         
   ///   @return index above the given one                                    
   LANGULUS(INLINED)
   size_t Pool::UpIndex(const size_t index) const noexcept {
      // Credit goes to Vladislav Penchev                               
      return index >> (LSB(index) + size_t {1});
   }

   /// Check if a memory address resigns inside pool's range                  
   ///   @param address - address to check                                    
   ///   @return true if address belongs to this pool                         
   LANGULUS(INLINED)
   bool Pool::Contains(const void* address) const noexcept {
      return address >= mMemory
         and address < mMemory + static_cast<size_t>(mAllocatedByBackend);
   }

   /// Find a memory entry from pointer                                       
   ///   @param memory - memory pointer                                       
   ///   @return the memory entry that manages the memory pointer, or         
   ///      nullptr if memory is not ours, or is no longer used               
   LANGULUS(INLINED)
   auto Pool::Find(const void* memory) const has_assumptions -> const Allocation* {
      if (Contains(memory)) {
         const auto entry = AllocationFromAddress(memory);
         return entry and entry->Contains(memory) ? entry : nullptr;
      }
      return nullptr;
   }
}
