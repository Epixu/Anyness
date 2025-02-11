///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Allocation.hpp"
#include <Core/Utilities.hpp>


namespace Langulus::Anyness
{
   namespace Inner
   {

      /// Fast log2                                                           
      /// https://stackoverflow.com/questions/11376288                        
      ///   @param u - number                                                 
      ///   @return the log2                                                  
      LANGULUS(INLINED)
      constexpr Offset FastLog2(const Offset x) noexcept {
         return x < 2 ? 0 
            : Offset {8 * sizeof(Offset)} - ::std::countl_zero(x) - Offset {1};
      }

      /// Get least significant bit                                           
      /// https://stackoverflow.com/questions/757059                          
      ///   @param n - number                                                 
      ///   @return the least significant bit                                 
      LANGULUS(INLINED)
      constexpr Offset LSB(const Offset n) noexcept {
         #if LANGULUS(BITNESS) == 32
            constexpr Offset DeBruijnBitPosition[32] = {
               0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8,
               31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9
            };
            constexpr Offset f = 0x077CB531u;
            return DeBruijnBitPosition[(Offset {n & (0 - n)} * f) >> Offset {27}];
         #elif LANGULUS(BITNESS) == 64
            constexpr Offset DeBruijnBitPosition[64] = {
               0,   1,  2, 53,  3,  7, 54, 27,  4, 38, 41,  8, 34, 55, 48, 28,
               62,  5, 39, 46, 44, 42, 22,  9, 24, 35, 59, 56, 49, 18, 29, 11,
               63, 52,  6, 26, 37, 40, 33, 47, 61, 45, 43, 21, 23, 58, 17, 10,
               51, 25, 36, 32, 60, 20, 57, 16, 50, 31, 19, 15, 30, 14, 13, 12
            };
            constexpr Offset f = 0x022fdd63cc95386dul;
            return DeBruijnBitPosition[(Offset {n & (0 - n)} * f) >> Offset {58}];
         #else
            #error Implement for your architecture
         #endif
      }

   } // namespace Langulus::Anyness::Inner


   /// Initialize an allocation                                               
   ///   @attention this constructor relies that instance is placed in the    
   ///      beginning of a heap allocation of size GetNewAllocationSize()     
   ///   @param bytes - the number of allocated bytes                         
   ///   @param pool - the pool/handle of the entry                           
   LANGULUS(INLINED)
   constexpr Allocation::Allocation(Offset bytes, Pool* pool) noexcept
      : mAllocatedBytes {bytes}
      , mReferences {1}
      , mPool {pool} {}

   /// Get the size of the Allocation structure, rounded up for alignment     
   ///   @return the byte size of the entry, including alignment              
   LANGULUS(INLINED)
   constexpr Offset Allocation::GetSize() noexcept {
      static_assert(IsPowerOfTwo(Alignment),
         "Alignment is not a power-of-two number");
      return sizeof(Allocation) + Alignment - (sizeof(Allocation) % Alignment);
   }

   /// Get the size required for allocating a new Allocation                  
   /// The layout is: [Allocation::GetSize()][client memory]                  
   ///   @param size - the usable number of bytes required                    
   ///   @return the byte size for a new Allocation, including padding        
   LANGULUS(INLINED)
   constexpr Offset Allocation::GetNewAllocationSize(Offset size) noexcept {
      const auto minimum = Allocation::GetMinAllocation();
      const auto proposed = Allocation::GetSize() + size;
      return ::std::max(proposed, minimum);
   }

   /// Get the minimum possible allocation, including the overhead            
   ///   @return the byte size                                                
   LANGULUS(INLINED)
   constexpr Offset Allocation::GetMinAllocation() noexcept {
      return Allocation::GetSize() + Alignment;
   }

   /// Check if the memory of the entry is in use                             
   ///   @return true if entry has any references                             
   LANGULUS(INLINED)
   constexpr Count Allocation::GetUses() const noexcept {
      return mReferences;
   }

   /// Return the end of usable block memory (always const)                   
   ///   @return pointer to the entry's memory end                            
   LANGULUS(INLINED)
   Byte const* Allocation::GetBlockEnd() const noexcept {
      return GetBlockStart() + mAllocatedBytes;
   }

   /// Return the aligned start of usable block memory                        
   ///   @return pointer to the entry's publicly usable memory                
   LANGULUS(INLINED)
   Byte* Allocation::GetBlockStart() const noexcept {
      const auto entryStart = reinterpret_cast<const Byte*>(this);
      return const_cast<Byte*>(entryStart + Allocation::GetSize());
   }
   
   /// Get the total of the entry, and its allocated data, in bytes           
   ///   @return the byte size of the entry plus the usable region after it   
   LANGULUS(INLINED)
   constexpr Offset Allocation::GetTotalSize() const noexcept {
      return Allocation::GetSize() + mAllocatedBytes;
   }

   /// Get the number of allocated bytes in this entry                        
   ///   @return the byte size of usable memory region                        
   LANGULUS(INLINED)
   constexpr Offset Allocation::GetAllocatedSize() const noexcept {
      return mAllocatedBytes;
   }

   /// Check if memory address is inside this entry                           
   ///   @param address - address to check if inside this entry               
   ///   @return true if address is inside                                    
   LANGULUS(INLINED)
   bool Allocation::Contains(const void* address) const noexcept {
      const auto a = reinterpret_cast<const Byte*>(address);
      const auto blockStart = GetBlockStart();
      return a >= blockStart and a < blockStart + mAllocatedBytes;
   }

   /// Test if one entry overlaps another                                     
   ///   @param other - entry to check for collision                          
   ///   @return true if memories dont intersect                              
   LANGULUS(INLINED)
   bool Allocation::CollisionFree(const Allocation& other) const noexcept {
      const auto blockStart1 = GetBlockStart();
      const auto blockStart2 = other.GetBlockStart();
      return (blockStart2 - blockStart1) > ::std::ptrdiff_t(mAllocatedBytes)
         and (blockStart1 - blockStart2) > ::std::ptrdiff_t(other.mAllocatedBytes);
   }

   /// Get the start of the entry as a given type                             
   ///   @return a pointer to the first element                               
   template<class T> LANGULUS(INLINED)
   T* Allocation::As() const noexcept {
      return reinterpret_cast<T*>(
         const_cast<Allocation*>(this)->GetBlockStart());
   }

   /// Reference the entry once                                               
   LANGULUS(INLINED)
   constexpr void Allocation::Keep() noexcept {
      ++mReferences;
   }

   /// Reference the entry 'c' times                                          
   ///   @param c - the number of references to add                           
   LANGULUS(INLINED)
   constexpr void Allocation::Keep(Count c) noexcept {
      mReferences += c;
   }

   /// Dereference the entry once                                             
   LANGULUS(INLINED)
   constexpr void Allocation::Free() noexcept {
      --mReferences;
   }

   /// Dereference the entry 'c' times                                        
   ///   @param c - the number of references to remove                        
   LANGULUS(INLINED)
   constexpr void Allocation::Free(Count c) noexcept {
      mReferences -= c;
   }

} // namespace Langulus::Fractalloc
