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

   /// Get the next entry in the free entry chain                             
   ///   @attention assumes allocation has been freed                         
   LANGULUS(ALWAYS_INLINED)
   auto Allocation::GetNextFreeEntry() const has_assumptions -> Allocation* {
      LglsAssumeDev(mReferences == 0,
         "Can't get next free entry from entry in use");
      return mNextFreeEntryFinder
         ? const_cast<Allocation*>(this - mNextFreeEntryFinder)
         : nullptr;
   }

   /// Set the next entry in the free entry chain                             
   ///   @attention assumes allocation has been freed                         
   LANGULUS(ALWAYS_INLINED)
   void Allocation::SetNextFreeEntry(Allocation const* a) has_assumptions {
      LglsAssumeDevAndOptimize(a,
         "If next entry is nullptr, use ResetNextFreeEntry instead");
      LglsAssumeDevAndOptimize(mReferences == 0,
         "Can't set next free entry if this entry is in use");
      LglsAssumeDevAndOptimize(a->mReferences == 0,
         "Can't set next free entry if next entry is in use");
      const intptr_t diff = this - a;
      LglsAssumeDev(diff >= ::std::numeric_limits<int32_t>::min()
                and diff <= ::std::numeric_limits<int32_t>::max(),
         "Entry difference is too big to fit in 32bit integer");
      
      mNextFreeEntryFinder = static_cast<int32_t>(this - a);
      
      LglsAssumeDev(GetNextFreeEntry() == a,
         "Next free entry isn't properly calculated from relative offset");
   }

   /// Reset the next entry in the free entry chain                           
   ///   @attention assumes allocation has been freed                         
   LANGULUS(ALWAYS_INLINED)
   void Allocation::ResetNextFreeEntry() has_assumptions {
      LglsAssumeDev(mReferences == 0,
         "Can't reset next free entry if this entry is in use");
      mNextFreeEntryFinder = 0;
   }

   /// Get the pool this allocation belongs to                                
   /// Pools are always aligned, so all we have to do is mask out 'this'      
   LANGULUS(ALWAYS_INLINED)
   auto Allocation::GetPool() const has_assumptions -> Pool const* {
      LglsAssumeDev(mReferences != 0, "Can't get pool if entry isn't in use");
      return reinterpret_cast<Pool const*>(
         reinterpret_cast<uintptr_t>(this) & ~((uintptr_t{1} << mPoolAlignment) - uintptr_t{1})
      );
   }

   /// Get the number of references                                           
   LANGULUS(ALWAYS_INLINED)
   auto Allocation::GetUses() const noexcept -> int32_t {
      return mReferences;
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

   /// Get the user bytes                                                     
   ///   @return the byte size of usable memory region                        
   LANGULUS(ALWAYS_INLINED)
   pot_t Allocation::GetSize() const has_assumptions {
      LglsAssumeDev(mReferences != 0,
         "Can't get size if entry isn't in use");
      pot_t result; result.bit = mSize;
      return result;
   }

   /// Return the aligned start of usable block memory (const)                
   ///   @return aligned pointer to the entry's memory                        
   LANGULUS(ALWAYS_INLINED)
   uint8_t* Allocation::GetBlockStart() const has_assumptions {
      LglsAssumeDev(mReferences != 0,
         "Can't get block start if entry isn't in use");
      const auto pool = GetPool();
      const size_t offset = this - pool->GetAllocationData();
      return pool->GetClientData() + pool->GetMinAllocation() * offset;
   }
   
   /// Check if memory address is inside this entry                           
   ///   @param address - address to check if inside this entry               
   ///   @return true if address is inside                                    
   LANGULUS(ALWAYS_INLINED)
   bool Allocation::Contains(const void* address) const has_assumptions {
      LglsAssumeDev(mReferences != 0,
         "Can't check if entry contains memory if entry isn't in use");
      const auto a = reinterpret_cast<uintptr_t>(address);
      const auto blockStart = reinterpret_cast<uintptr_t>(GetBlockStart());
      return a >= blockStart and a < blockStart + static_cast<uintptr_t>(GetSize());
   }
}
