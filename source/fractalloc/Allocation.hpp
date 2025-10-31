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
   struct alignas(Alignment) Allocation {
   protected:
      friend struct Pool;
      friend struct Allocator;

      // The number of references to this memory.                       
      // Most often used, so first for immediate access.                
      int32_t mReferences = 1;

      // This has two states depending on mReferences:                  
      // If mReferences > 0, the first struct is used                   
      // If mReferences == 0, the second struct is used                 
      union {
         struct {
            #if LANGULUS_FEATURE(MEMORY_STATISTICS)
               // Acts like a timestamp of when the allocation happened 
               uint64_t mStep;
            #endif

            // Used to find the pool pointer by rounding 'this'         
            pot_t mPoolSize;
            // Allocated bytes usable by client                         
            pot_t mSize;
            // Data alignment                                           
            pot_t mAlignment;
         };
         struct {
            int32_t mNextFreeEntryFinder;
         };
      };

   public:
       Allocation() = delete;
       Allocation(const Allocation&) = delete;
       Allocation(Allocation&&) = delete;
      ~Allocation() = delete;

      explicit Allocation(pot_t size, Pool const*) has_assumptions;
      
      static size_t Cost(pot_t alignment) noexcept;

      auto GetPool() const noexcept -> Pool const*;
      auto GetUses() const noexcept { return mReferences; }
      auto GetBackendSize() const noexcept -> size_t;
      auto GetFrontendSize() const noexcept -> size_t;
      auto GetBlockStart() const noexcept -> uint8_t*;
      auto GetBlockEnd() const noexcept -> uint8_t const*;
      bool Contains(const void*) const noexcept;
      void Keep(int32_t = 1) noexcept;
      void Free(int32_t = 1) noexcept;
   };
}
