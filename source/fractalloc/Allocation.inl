///                                                                           
/// Langulus::Fractalloc                                                      
/// Copyright (c) 2015 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Allocation.hpp"
#include "Pool.hpp"


namespace Langulus::Fractalloc
{
   /// Initialize an allocation                                               
   ///   @param bytes - the number of allocated bytes in bitshift form        
   ///   @param pool - the pool this allocation belongs to                    
   LANGULUS(ALWAYS_INLINED)
   Allocation::Allocation(pot_t bytes, Pool* pool) has_assumptions {
      LglsAssumeDev(bytes, "Invalid bytes");
      LglsAssumeDev(pool,  "Invalid pool");

      const auto pool_begin = pool->GetPoolStart();
      LglsAssumeDev(reinterpret_cast<uint8_t*>(this) >= pool_begin,
         "Entry isn't after pool's beginning");

      const auto pool_diff = (reinterpret_cast<uint8_t*>(this) - pool_begin)
         / Roof2(sizeof(Allocation) + Alignment);
      LglsAssumeDev(pool_begin + pool_diff < pool->GetPoolEnd(),
         "Entry isn't before pool's end");
      LglsAssumeDev(pool_diff <= ::std::numeric_limits<decltype(mPoolFinder)>::max(),
         "Pool finder is too far to fit in variable");

      mSizeMSB = bytes;
      mPoolFinder = static_cast<decltype(mPoolFinder)>(pool_diff);
   }

   /// Get the pool this allocation belongs to                                
   auto Allocation::GetPool() const noexcept -> Pool const* {

   }

   /// User bytes + the header size                                           
   ///   @return the byte size of the entry plus the usable region after it   
   LANGULUS(ALWAYS_INLINED)
   size_t Allocation::GetBackendSize() const noexcept {
      return Align(sizeof(Allocation), mPool->GetAlignment()) + GetFrontendSize();
   }

   /// Get the user bytes                                                     
   ///   @return the byte size of usable memory region                        
   LANGULUS(ALWAYS_INLINED)
   size_t Allocation::GetFrontendSize() const noexcept {
      return mAllocatedBytes;
   }

   /// Return the aligned start of usable block memory (const)                
   ///   @return aligned pointer to the entry's memory                        
   LANGULUS(ALWAYS_INLINED)
   uint8_t* Allocation::GetBlockStart() const noexcept {
      const auto entryStart = reinterpret_cast<const uint8_t*>(this);
      return const_cast<uint8_t*>(entryStart)
           + Align(sizeof(Allocation), mPool->GetAlignment());
   }

   /// Return the end of usable block memory (always const)                   
   ///   @return aligned pointer to the entry's memory end                    
   LANGULUS(ALWAYS_INLINED)
   uint8_t const* Allocation::GetBlockEnd() const noexcept {
      return GetBlockStart() + mAllocatedBytes;
   }
   
   /// Check if memory address is inside this entry                           
   ///   @param address - address to check if inside this entry               
   ///   @return true if address is inside                                    
   LANGULUS(ALWAYS_INLINED)
   bool Allocation::Contains(const void* address) const noexcept {
      const auto a = static_cast<const uint8_t*>(address);
      const auto blockStart = GetBlockStart();
      return a >= blockStart and a < blockStart + mAllocatedBytes;
   }

   /// Reference the entry 'c' times                                          
   ///   @param c - the number of references to add                           
   LANGULUS(ALWAYS_INLINED)
   void Allocation::Keep(int c) noexcept {
      mReferences += c;
   }

   /// Dereference the entry 'c' times                                        
   ///   @param c - the number of references to remove                        
   LANGULUS(ALWAYS_INLINED)
   void Allocation::Free(int c) noexcept {
      mReferences -= c;
   }
}
