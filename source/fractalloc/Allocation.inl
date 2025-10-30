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
   Allocation::Allocation(pot_t bytes, Pool const* pool) has_assumptions {
      LglsAssumeDevAndOptimize(pool, "Invalid pool");
      mPoolSize = pool->GetAllocatedByFrontend();
      mSize = bytes;
      mAlignment = pool->GetAlignment();
      LglsAssumeDev(GetPool() == pool, "Incorrect pool pointer deduction");
   }

   /// Get the cost of allocating a single allocation - this includes         
   /// sizeof(Allocation) together with any padding for data alignment        
   size_t Allocation::Cost(pot_t alignment) noexcept {
      return Align(sizeof(Allocation), alignment);
   }

   /// Get the pool this allocation belongs to                                
   /// Pools are always aligned, so all we have to do is mask out 'this'      
   auto Allocation::GetPool() const noexcept -> Pool const* {
      return reinterpret_cast<Pool const*>(
         reinterpret_cast<uintptr_t>(this) & ~mPoolSize.mask()
      );
   }

   /// User bytes + the header size                                           
   ///   @return the byte size of the entry plus the usable region after it   
   LANGULUS(ALWAYS_INLINED)
   size_t Allocation::GetBackendSize() const noexcept {
      return Cost(mAlignment) + GetFrontendSize();
   }

   /// Get the user bytes                                                     
   ///   @return the byte size of usable memory region                        
   LANGULUS(ALWAYS_INLINED)
   size_t Allocation::GetFrontendSize() const noexcept {
      return static_cast<size_t>(mSize);
   }

   /// Return the aligned start of usable block memory (const)                
   ///   @return aligned pointer to the entry's memory                        
   LANGULUS(ALWAYS_INLINED)
   uint8_t* Allocation::GetBlockStart() const noexcept {
      const auto entryStart = reinterpret_cast<const uint8_t*>(this);
      return const_cast<uint8_t*>(entryStart) + Cost(mAlignment);
   }

   /// Return the end of usable block memory (always const)                   
   ///   @return aligned pointer to the entry's memory end                    
   LANGULUS(ALWAYS_INLINED)
   uint8_t const* Allocation::GetBlockEnd() const noexcept {
      return GetBlockStart() + GetFrontendSize();
   }
   
   /// Check if memory address is inside this entry                           
   ///   @param address - address to check if inside this entry               
   ///   @return true if address is inside                                    
   LANGULUS(ALWAYS_INLINED)
   bool Allocation::Contains(const void* address) const noexcept {
      const auto a = static_cast<const uint8_t*>(address);
      const auto blockStart = GetBlockStart();
      return a >= blockStart and a < blockStart + GetFrontendSize();
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
