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
   ///   @attention relies that size is a power-of-two                        
   ///   @attention this constructor relies that instance is placed in the    
   ///      beginning of a heap allocation of size Pool::NewAllocationSize()  
   ///   @param meta - optional meta data associated with pool                
   ///   @param size - bytes of the usable block to initialize with           
   ///   @param memory - handle for use with std::free()                      
   LANGULUS(INLINED)
   Pool::Pool(DMeta meta, size_t size, void* memory) noexcept
      : mAllocatedByBackend     {size}
      , mAllocatedByBackendLog2 {FastLog2(size)}
      , mAllocatedByBackendLSB  {LSB(size >> size_t {1})}
      , mThreshold              {size}
      , mThresholdPrevious      {size}
      , mThresholdMin           {meta 
         ? Roof2(meta.GetMinAllocation())
         : ::Langulus::MinimalAllocation}
      , mMeta                   {meta}
      , mHandle                 {memory}
   {
      mMemory = GetPoolStart();
      mMemoryEnd = mMemory + mAllocatedByBackend;

      IF_LANGULUS_MEMORY_STATISTICS(mStep = Instance.GetStatistics().mStep);

      // Touching is mandatory for pools - without touching the         
      // memory, it might remain just a promise by the OS, making       
      // initial pool allocations very, very, VERY slow at the most     
      // inappropriate of times                                         
      Touch();
   }

   /// Get the minimum allocation for an entry inside this pool               
   ///   @return the size in bytes, always a power-of-two                     
   LANGULUS(INLINED)
   constexpr size_t Pool::GetMinAllocation() const noexcept {
      return mThresholdMin;
   }

   /// Get the total byte size of the pool, including overhead                
   ///   @return the size in bytes                                            
   LANGULUS(INLINED)
   constexpr size_t Pool::GetTotalSize() const noexcept {
      return sizeof(Pool) + mAllocatedByBackend;
   }

   /// Get the max number of possible entries                                 
   /// (if all of them are as small as possible)                              
   ///   @return the size in bytes, always a power-of-two                     
   LANGULUS(INLINED)
   constexpr size_t Pool::GetMaxEntries() const noexcept {
      return mAllocatedByBackend / GetMinAllocation();
   }

   /// Free the whole pool chain                                              
   ///   @attention make sure this is called for the first pool in the chain  
   LANGULUS(INLINED)
   void Pool::FreePoolChain() {
      if (mNext)
         mNext->FreePoolChain();
      free(mHandle);
   }

   /// Get the size for a new pool allocation, with alignment/additional      
   /// memory requirements                                                    
   ///   @assumes size is a power-of-two                                      
   ///   @assumes size can contain at least one Allocation::GetMinAllocation  
   ///   @param size - the number of bytes to request for the pool            
   ///   @return the number of bytes to allocate for use in the pool          
   LANGULUS(INLINED)
   constexpr size_t Pool::GetNewAllocationSize(size_t size) noexcept {
      constexpr auto minimum = DefaultPoolSize + sizeof(Pool);
      return ::std::max(size + sizeof(Pool), minimum);
   }

   /// Get the start of the usable memory for the pool                        
   ///   @return the start of the memory                                      
   LANGULUS(INLINED)
   auto Pool::GetPoolStart() const noexcept -> uint8_t* {
      const auto poolStart = reinterpret_cast<const uint8_t*>(this);
      return const_cast<uint8_t*>(poolStart + sizeof(Pool));
   }

   /// Get the true allocation size, as bytes requested from OS               
   ///   @return bytes allocated for the pool, including alignment/overhead   
   LANGULUS(INLINED)
   constexpr size_t Pool::GetAllocatedByBackend() const noexcept {
      return mAllocatedByBackend;
   }

   /// Get the allocation size, as bytes requested from client                
   ///   @return bytes allocated by the client                                
   LANGULUS(INLINED)
   constexpr size_t Pool::GetAllocatedByFrontend() const noexcept {
      return mAllocatedByFrontend;
   }

   /// Allocate an entry inside the pool - returned pointer is aligned        
   ///   @param bytes - number of bytes to allocate                           
   ///   @return the new allocation, or nullptr if pool is full               
   inline auto Pool::Allocate(const size_t bytes) has_assumptions -> Allocation* {
      // Check if we can add a new entry                                
      const auto bytesWithPadding = Allocation::GetNewAllocationSize(bytes);
      if (not CanContain(bytesWithPadding))
         return nullptr;

      Allocation* newEntry;
      if (mLastFreed) {
         // Recycle entries                                             
         newEntry = mLastFreed;
         mLastFreed = mLastFreed->mNextFreeEntry;
         new (newEntry) Allocation {
            bytesWithPadding - sizeof(Allocation), this
         };
      }
      else {
         // The entire pool is full (or empty), skip search for free    
         // spot, add a new allocation directly	instead                 
         newEntry = const_cast<Allocation*>(AllocationFromIndex(mEntries));
         new (newEntry) Allocation {
            bytesWithPadding - sizeof(Allocation), this
         };

         ++mEntries;

         if (reinterpret_cast<uint8_t*>(newEntry) + mThreshold >= mMemoryEnd) {
            // Reset carriage and shift level when it goes beyond       
            mThresholdPrevious = mThreshold;
            mThreshold >>= size_t {1};
         }
      }

      // Always adapt min threshold if bigger entry is introduced       
      if (bytesWithPadding > mThresholdMin)
         mThresholdMin = Roof2(bytesWithPadding);

      LglsAssumeDevAndOptimize(
         mAllocatedByFrontend + bytesWithPadding >= mAllocatedByFrontend,
         "Frontend byte counter overflow"
      );
      mAllocatedByFrontend += bytesWithPadding;
      IF_LANGULUS_MEMORY_STATISTICS(++mValidEntries);
      return newEntry;
   }

   /// Remove an entry                                                        
   ///   @attention assumes entry is valid                                    
   ///   @param entry - entry to remove                                       
   inline void Pool::Deallocate(Allocation* entry) has_assumptions {
      LglsAssumeDevAndOptimize(entry->mReferences != 0,
         "Removing an invalid entry");
      LglsAssumeDevAndOptimize(mEntries,
         "Bad valid entry count");
      LglsAssumeDev(mAllocatedByFrontend >= entry->GetBackendSize(),
         "Bad frontend allocation size");

      mAllocatedByFrontend -= entry->GetBackendSize();
      entry->mReferences = 0;

      if (0 == mAllocatedByFrontend) {
         // The freed entry was the last used entry                     
         // Reset the entire pool                                       
         mThreshold = mThresholdPrevious = mAllocatedByBackend;
         mThresholdMin = Allocation::GetMinAllocation();
         mLastFreed = nullptr;
         mEntries = 0;
         IF_LANGULUS_MEMORY_STATISTICS(mValidEntries = 0);
      }
      else {
         // Push the removed entry to the last freed list               
         // The removed entry becomes the last freed entry, and its     
         // pool pointer becomes a jump to the previous last freed      
         entry->mNextFreeEntry = mLastFreed;
         mLastFreed = entry;
         IF_LANGULUS_MEMORY_STATISTICS(--mValidEntries);

         //TODO: keep track of size distrubution, 
         // shrink min threshold if all leading buckets go empty
      }
   }

   /// Resize an entry                                                        
   ///   @param entry - entry to resize                                       
   ///   @param bytes - new number of bytes                                   
   ///   @return true if entry was enlarged without conflict                  
   inline bool Pool::Reallocate(Allocation* entry, const size_t bytes) has_assumptions {
      LglsAssumeDev(bytes and Contains(entry) and entry and entry->GetUses(),
         "Invalid reallocation");

      if (bytes > entry->GetFrontendSize()) {
         // We're enlarging the entry                                   
         // Make sure we don't violate threshold                        
         const auto addition = bytes - entry->GetFrontendSize();
         const auto newtotal = entry->GetBackendSize() + addition;
         if (newtotal > mThreshold)
            return false;

         if (newtotal > mThresholdMin)
            mThresholdMin = Roof2(newtotal);

         mAllocatedByFrontend += addition;
      }
      else {
         // We're shrinking the entry                                   
         // No checks required                                          
         const auto removal = entry->GetFrontendSize() - bytes;
         LglsAssumeDevAndOptimize(mAllocatedByFrontend >= removal,
            "Bad frontend allocation size");

         mAllocatedByFrontend -= removal;

         //TODO: keep track of size distrubution, 
         // shrink min threshold if all leading buckets go empty
      }

      entry->mAllocatedBytes = bytes;
      return true;
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
   constexpr bool Pool::CanContain(size_t bytes) const noexcept {
      return mThreshold >= mThresholdMin and bytes <= mThreshold;
   }

   /// Null the memory                                                        
   LANGULUS(INLINED)
   void Pool::Null() {
      memset(mMemory, 0, mAllocatedByBackend);
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

         prev->mNextFreeEntry = const_cast<Allocation*>(entry);
         prev = prev->mNextFreeEntry;
      }

      if (prev)
         prev->mNextFreeEntry = nullptr;

      mThreshold = ThresholdFromIndex(mEntries - 1);
      mThresholdPrevious = mThreshold != mAllocatedByBackend
         ? size_t {mThreshold * 2} : mThreshold;
   }

   /// Get threshold associated with an index                                 
   ///   @attention assumes index is not zero                                 
   ///   @param index - the index                                             
   ///   @return the threshold                                                
   LANGULUS(INLINED)
   size_t Pool::ThresholdFromIndex(size_t index) const noexcept {
      return size_t {1} << (mAllocatedByBackendLSB - FastLog2(index));
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
      const size_t basePower = FastLog2(index);
      const size_t baselessIndex = index - (one << basePower);
      const size_t levelIndex = (baselessIndex << one) + one;
      const size_t levelSize = (one << (mAllocatedByBackendLSB - basePower));
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
      if (i < mThreshold or 0 == mEntries)
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
      while (index != 0 and (index >= mEntries
                         or 0 == AllocationFromIndex(index)->GetUses()))
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
      return address >= mMemory and address < mMemory + mAllocatedByBackend;
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

} // namespace Langulus::Fractalloc
