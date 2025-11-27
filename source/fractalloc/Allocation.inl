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
   ///   @param bytes - the number of allocated bytes                         
   ///   @param pool_alignment - the pool alignment                           
   LANGULUS(ALWAYS_INLINED)
   Allocation::Allocation(pot_t bytes, pot_t pool_alignment) noexcept {
      mPoolAlignment = pool_alignment.bit;
      mSize = bytes.bit;
   }

   LANGULUS(ALWAYS_INLINED)
   auto Allocation::GetNextFreeEntry() const has_assumptions -> Allocation* {
      LglsAssumeDev(mReferences == 0,
         "Can't get next free entry from entry in use");
      return mNextFreeEntryFinder
         ? const_cast<Allocation*>(this - mNextFreeEntryFinder)
         : nullptr;
   }

   inline void Allocation::SetNextFreeEntry(Allocation const* a) has_assumptions {
      LglsAssumeDev(mReferences == 0,
         "Can't set next free entry of entry in use");
      mNextFreeEntryFinder = static_cast<int32_t>(this - a);
   }

   inline void Allocation::ResetNextFreeEntry() has_assumptions {
      LglsAssumeDev(mReferences == 0,
         "Can't reset next free entry of entry in use");
      mNextFreeEntryFinder = 0;
   }

   /// Get the pool this allocation belongs to                                
   /// Pools are always aligned, so all we have to do is mask out 'this'      
   LANGULUS(ALWAYS_INLINED)
   auto Allocation::GetPool() const noexcept -> Pool const* {
      return reinterpret_cast<Pool const*>(
         reinterpret_cast<uintptr_t>(this) & ~((uintptr_t{1} << mPoolAlignment) - uintptr_t{1})
      );
   }

   LANGULUS(ALWAYS_INLINED)
   auto Allocation::GetUses() const noexcept -> int32_t {
      return mReferences;
   }

   /// Get the user bytes                                                     
   ///   @return the byte size of usable memory region                        
   LANGULUS(ALWAYS_INLINED)
   pot_t Allocation::GetSize() const noexcept {
      pot_t result; result.bit = mSize;
      return result;
   }

   /// Return the aligned start of usable block memory (const)                
   ///   @return aligned pointer to the entry's memory                        
   LANGULUS(ALWAYS_INLINED)
   uint8_t* Allocation::GetBlockStart() const noexcept {
      const auto pool = GetPool();
      const size_t offset = this - pool->GetAllocationData();
      return GetPool()->GetClientData() + GetPool()->GetMinAllocation() * offset;
   }
   
   /// Check if memory address is inside this entry                           
   ///   @param address - address to check if inside this entry               
   ///   @return true if address is inside                                    
   LANGULUS(ALWAYS_INLINED)
   bool Allocation::Contains(const void* address) const noexcept {
      const auto a = reinterpret_cast<uintptr_t>(address);
      const auto blockStart = reinterpret_cast<uintptr_t>(GetBlockStart());
      return a >= blockStart and a < blockStart + static_cast<uintptr_t>(GetSize());
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
