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

#include "Pool.hpp"
#include <Langulus/CT/Signed.hpp>
#include <Langulus/Assume.hpp>


namespace Langulus::Fractalloc
{
   
   /// Fast log2                                                              
   /// https://stackoverflow.com/questions/11376288                           
   LANGULUS(ALWAYS_INLINED)
   constexpr Size FastLog2(const Size x) noexcept {
      return x < 2 ? 0 : Size {8 * sizeof(Size)} - ::std::countl_zero(x) - Size {1};
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
   
   /// Check if an unsigned integer is a power of two                         
   ///   @param n - the number to test                                        
   ///   @return true if number has exactly one bit set                       
   /*LANGULUS(ALWAYS_INLINED)
   constexpr bool IsPowerOfTwo(CT::Unsigned auto n) noexcept {
      return ::std::has_single_bit(n);
   }

   /// Returns the number of consecutive 0 bits in the value of x, starting	
   /// from the least significant 'right' bit                                 
   ///   @param x - the value to scan                                         
   ///   @return the number of consecutive zero bits                          
   LANGULUS(ALWAYS_INLINED)
   constexpr int CountTrailingZeroes(CT::Unsigned auto x) noexcept {
      return ::std::countr_zero(x);
   }

   /// Returns the number of consecutive 0 bits in the value of x, starting   
   /// from the most significant 'left' bit                                   
   ///   @param x - the value to scan                                         
   ///   @return the number of consecutive zero bits                          
   LANGULUS(ALWAYS_INLINED)
   constexpr int CountLeadingZeroes(CT::Unsigned auto x) noexcept {
      return ::std::countl_zero(x);
   }*/

   /// Round to the upper power-of-two                                        
   ///   @tparam SAFE - set to true if you want it to throw on overflow       
   ///   @param x - the unsigned integer to round up                          
   ///   @return the closest upper power-of-two to x                          
   template<bool SAFE = false, CT::Unsigned T> LANGULUS(ALWAYS_INLINED)
   constexpr T Roof2(const T x) noexcept(not SAFE) {
      static_assert(sizeof(T) <= 8, "Not implemented");

      if constexpr (SAFE) {
         constexpr T lastPowerOfTwo = (T {1}) << (T {sizeof(T) * 8 - 1});
         AssumeDev(x <= lastPowerOfTwo, HERE(), "Roof2 overflowed");
      }

      if consteval {
         T n = x;
         --n;
         n |= n >> 1;
         n |= n >> 2;
         n |= n >> 4;
         if constexpr (sizeof(T) > 1)
            n |= n >> 8;
         if constexpr (sizeof(T) > 2)
            n |= n >> 16;
         if constexpr (sizeof(T) > 4)
            n |= n >> 32;
         ++n;
         return n;
      }
      else {
         return x <= 1 ? x : static_cast<T>(T {1} << 
            static_cast<T>(sizeof(T) * 8 - ::std::countl_zero(static_cast<T>(x - 1))));
      }
   }


   /// Initialize a pool                                                      
   ///   @attention relies that size is a power-of-two                        
   ///   @attention this constructor relies that instance is placed in the    
   ///      beginning of a heap allocation of size Pool::NewAllocationSize()  
   ///   @param meta - optional meta data associated with pool                
   ///   @param size - bytes of the usable block to initialize with           
   ///   @param memory - handle for use with std::free()                      
   LANGULUS(INLINED)
   Pool::Pool(DMeta meta, Size size, void* memory) noexcept
      : mAllocatedByBackend     {size}
      , mAllocatedByBackendLog2 {FastLog2(size)}
      , mAllocatedByBackendLSB  {LSB(size >> Size {1})}
      , mThreshold              {size}
      , mThresholdPrevious      {size}
      , mThresholdMin           {Roof2(meta.GetMinAllocation())}
      , mMeta                   {meta}
      , mHandle                 {memory}
   {
      mMemory = GetPoolStart();
      mMemoryEnd = mMemory + mAllocatedByBackend;

      #if LANGULUS_FEATURE(MEMORY_STATISTICS)
         mStep = Instance.GetStatistics().mStep;
      #endif

      // Touching is mandatory for pools - without touching the         
      // memory, it might remain just a promise by the OS, making       
      // initial pool allocations very, very, VERY slow                 
      Touch();
   }

   /// Get the minimum allocation for an entry inside this pool               
   ///   @return the size in bytes, always a power-of-two                     
   LANGULUS(INLINED)
   constexpr Size Pool::GetMinAllocation() const noexcept {
      return mThresholdMin;
   }

   /// Get the total byte size of the pool, including overhead                
   ///   @return the size in bytes                                            
   LANGULUS(INLINED)
   constexpr Size Pool::GetTotalSize() const noexcept {
      return Pool::GetSize() + mAllocatedByBackend;
   }

   /// Get the max number of possible entries                                 
   /// (if all of them are as small as possible)                              
   ///   @return the size in bytes, always a power-of-two                     
   LANGULUS(INLINED)
   constexpr Size Pool::GetMaxEntries() const noexcept {
      return mAllocatedByBackend / GetMinAllocation();
   }

   /// Free the whole pool chain                                              
   ///   @attention make sure this is called for the first pool in the chain  
   LANGULUS(INLINED)
   void Pool::FreePoolChain() {
      if (mNext)
         mNext->FreePoolChain();
      ::std::free(mHandle);
   }

   /// Get the size of the Pool structure, rounded up for alignment           
   ///   @return the byte size of the pool, including alignment               
   LANGULUS(INLINED)
   constexpr Size Pool::GetSize() noexcept {
      return sizeof(Pool) + Alignment - (sizeof(Pool) % Alignment);
   }

   /// Get the size for a new pool allocation, with alignment/additional      
   /// memory requirements                                                    
   ///   @assumes size is a power-of-two                                      
   ///   @assumes size can contain at least one Allocation::GetMinAllocation  
   ///   @param size - the number of bytes to request for the pool            
   ///   @return the number of bytes to allocate for use in the pool          
   LANGULUS(INLINED)
   constexpr Size Pool::GetNewAllocationSize(Size size) noexcept {
      constexpr auto minimum = Pool::DefaultPoolSize + Pool::GetSize();
      return ::std::max(size + Pool::GetSize(), minimum);
   }

   /// Get the start of the usable memory for the pool                        
   ///   @return the start of the memory                                      
   LANGULUS(INLINED)
   auto Pool::GetPoolStart() const noexcept -> Byte* {
      const auto poolStart = reinterpret_cast<const Byte*>(this);
      return const_cast<Byte*>(poolStart + Pool::GetSize());
   }

   /// Get the true allocation size, as bytes requested from OS               
   ///   @return bytes allocated for the pool, including alignment/overhead   
   LANGULUS(INLINED)
   constexpr Size Pool::GetAllocatedByBackend() const noexcept {
      return mAllocatedByBackend;
   }

   /// Get the allocation size, as bytes requested from client                
   ///   @return bytes allocated by the client                                
   LANGULUS(INLINED)
   constexpr Size Pool::GetAllocatedByFrontend() const noexcept {
      return mAllocatedByFrontend;
   }

   /// Allocate an entry inside the pool - returned pointer is aligned        
   ///   @param bytes - number of bytes to allocate                           
   ///   @return the new allocation, or nullptr if pool is full               
   inline auto Pool::Allocate(const Size bytes) has_assumptions -> Allocation* {
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
            bytesWithPadding - Allocation::GetHeaderSize(), this
         };
      }
      else {
         // The entire pool is full (or empty), skip search for free    
         // spot, add a new allocation directly	instead                 
         newEntry = const_cast<Allocation*>(AllocationFromIndex(mEntries));
         new (newEntry) Allocation {
            bytesWithPadding - Allocation::GetHeaderSize(), this
         };

         ++mEntries;

         if (reinterpret_cast<Byte*>(newEntry) + mThreshold >= mMemoryEnd) {
            // Reset carriage and shift level when it goes beyond       
            mThresholdPrevious = mThreshold;
            mThreshold >>= Size {1};
         }
      }

      // Always adapt min threshold if bigger entry is introduced       
      if (bytesWithPadding > mThresholdMin)
         mThresholdMin = Roof2(bytesWithPadding);

      LANGULUS_ASSUME(DevAssumes,
         mAllocatedByFrontend + bytesWithPadding >= mAllocatedByFrontend,
         "Frontend byte counter overflow");
      mAllocatedByFrontend += bytesWithPadding;
      IF_LANGULUS_MEMORY_STATISTICS(++mValidEntries);
      return newEntry;
   }

   /// Remove an entry                                                        
   ///   @attention assumes entry is valid                                    
   ///   @param entry - entry to remove                                       
   inline void Pool::Deallocate(Allocation* entry) has_assumptions {
      LANGULUS_ASSUME(DevAssumes, entry->mReferences != 0,
         "Removing an invalid entry");
      LANGULUS_ASSUME(DevAssumes, mEntries,
         "Bad valid entry count");
      LANGULUS_ASSUME(DevAssumes, mAllocatedByFrontend >= entry->GetFrontendSize(),
         "Bad frontend allocation size");

      mAllocatedByFrontend -= entry->GetFrontendSize();
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
   inline bool Pool::Reallocate(Allocation* entry, const Size bytes) has_assumptions {
      LANGULUS_ASSUME(DevAssumes,
         bytes and Contains(entry) and entry and entry->GetUses(),
         "Invalid reallocation");

      if (bytes > entry->mAllocatedBytes) {
         // We're enlarging the entry                                   
         // Make sure we don't violate threshold                        
         const auto addition = bytes - entry->mAllocatedBytes;
         const auto newtotal = entry->GetFrontendSize() + addition;
         if (newtotal > mThreshold)
            return false;

         if (newtotal > mThresholdMin)
            mThresholdMin = Roof2(newtotal);

         mAllocatedByFrontend += addition;
      }
      else {
         // We're shrinking the entry                                   
         // No checks required                                          
         const auto removal = entry->mAllocatedBytes - bytes;
         LANGULUS_ASSUME(DevAssumes, mAllocatedByFrontend >= removal,
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
   constexpr bool Pool::CanContain(Size bytes) const noexcept {
      return mThreshold >= mThresholdMin and bytes <= mThreshold;
   }

   /// Null the memory                                                        
   LANGULUS(INLINED)
   void Pool::Null() {
      ZeroMemory(mMemory, mAllocatedByBackend);
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
      LANGULUS_ASSUME(DevAssumes, mEntries, "Should have at least one entry");

      const Allocation* entry;
      Size ecounter = mEntries;
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
         ? Size {mThreshold * 2} : mThreshold;
   }

   /// Get threshold associated with an index                                 
   ///   @attention assumes index is not zero                                 
   ///   @param index - the index                                             
   ///   @return the threshold                                                
   LANGULUS(INLINED)
   Size Pool::ThresholdFromIndex(Size index) const noexcept {
      return Size {1} << (mAllocatedByBackendLSB - FastLog2(index));
   }

   /// Get allocation from index                                              
   ///   @param index - the index                                             
   ///   @return the allocation (not validated and constrained)               
   LANGULUS(INLINED)
   auto Pool::AllocationFromIndex(Size index) const noexcept -> const Allocation* {
      // Credit goes to Vladislav Penchev                               
      if (index == 0)
         return reinterpret_cast<const Allocation*>(mMemory);

      constexpr Size one = 1;
      const Size basePower = FastLog2(index);
      const Size baselessIndex = index - (one << basePower);
      const Size levelIndex = (baselessIndex << one) + one;
      const Size levelSize = (one << (mAllocatedByBackendLSB - basePower));
      return reinterpret_cast<const Allocation*>(mMemory + levelIndex * levelSize);
   }

   /// Get index from address                                                 
   ///   @attention assumes pointer is inside the pool                        
   ///   @param ptr - the address                                             
   ///   @return the index                                                    
   LANGULUS(INLINED)
   Size Pool::IndexFromAddress(const void* ptr) const has_assumptions {
      LANGULUS_ASSUME(DevAssumes, Contains(ptr), "Entry outside pool");

      // Credit goes to Yasen Vidolov (G1)                              
      const Size i = static_cast<const Byte*>(ptr) - mMemory;
      if (i < mThreshold or 0 == mEntries)
         return 0;

      // We got the index, but it is not constrained to the pool        
      constexpr Size one = 1;
      Size index = ((mAllocatedByBackend + i) / (i & ~(i - one)) - one) >> one;
      while (index >= mEntries)
         index = UpIndex(index);
      return index;
   }

   /// Validate an index, check if corresponding to a valid allocation        
   /// or shift it up until one is found                                      
   ///   @param index - index to validate                                     
   ///   @returns the valid index, or InvalidIndex if invalid                 
   LANGULUS(INLINED)
   Size Pool::ValidateIndex(Size index) const noexcept {
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
   Size Pool::UpIndex(const Size index) const noexcept {
      // Credit goes to Vladislav Penchev                               
      return index >> (LSB(index) + Size {1});
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
