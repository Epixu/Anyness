///                                                                           
/// Langulus::Fractalloc                                                      
/// Copyright (c) 2015 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include <Langulus/Core.hpp>
#include <Langulus/Utils/Pot.hpp>

#if not LANGULUS_FEATURE(MANAGED_MEMORY)
   #error "This file shouldn't be included if MANAGED_MEMORY is disabled"
#endif


namespace Langulus::Fractalloc
{
   struct Pool;

   ///                                                                        
   ///   Memory allocation                                                    
   ///                                                                        
   struct Allocation {
   private:
      // The number of references to this memory.                       
      // Most often used, so first for immediate access.                
      int32_t mReferences = 1;

      // This has two states depending on mReferences:                  
      // If mReferences > 0, the struct is used                         
      // If mReferences == 0, mNextFreeEntryFinder is used              
      union {
         struct {
            #if LANGULUS_FEATURE(MEMORY_STATISTICS)
               // Acts like a timestamp of when the allocation happened 
               uint64_t mStep;
            #endif
            
            // Used to find the pool pointer by rounding 'this'         
            // Represented as a bit number                              
            uint8_t mPoolAlignment;
            // Allocated bytes usable by client                         
            // Represented as a bit number                              
            uint8_t mSize;
         };
         int32_t mNextFreeEntryFinder;
      };

   public:
      Allocation() = delete;
      Allocation(const Allocation&) = delete;
      Allocation(Allocation&&) = delete;

      Allocation(pot_t size, pot_t pool_alignment) noexcept;
      
      auto GetUses() const noexcept -> int32_t;
      void Keep(int32_t = 1) noexcept;
      void Free(int32_t = 1) noexcept;
      
      auto GetSize() const has_assumptions -> pot_t;
      auto GetBlockStart() const has_assumptions -> uint8_t*;
      auto Contains(const void*) const has_assumptions -> bool;

   protected:
   IF_LANGULUS_TESTING(public:)
      friend struct Pool;
      friend struct Allocator;
      
      auto GetNextFreeEntry() const has_assumptions -> Allocation*;
      void SetNextFreeEntry(Allocation const*) has_assumptions;
      void ResetNextFreeEntry() has_assumptions;
      auto GetPool() const has_assumptions -> Pool const*;
   };
}
