///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include <Langulus/Core.hpp>
#include <Langulus/Utils/Pot.hpp>

#if LANGULUS_FEATURE(MANAGED_MEMORY)
   #error "This file shouldn't be included if MANAGED_MEMORY is enabled"
#endif


namespace Langulus::Unmanaged
{
   struct Allocator;

   ///                                                                        
   ///   Memory allocation                                                    
   ///                                                                        
   struct Allocation {
   protected:
      friend struct Allocator;
      
      // The number of references to this memory.                       
      // Most often used, so first for immediate access.                
      int32_t mReferences = 1;

      #if LANGULUS_FEATURE(MEMORY_STATISTICS)
         // Acts like a timestamp of when the allocation happened       
         uint64_t mStep;
      #endif

      // Allocated bytes usable by client                               
      pot_t mSize;
      // The alignment of the contained data                            
      pot_t mAlignment;

   public:
      Allocation() = delete;
      Allocation(const Allocation&) = delete;
      Allocation(Allocation&&) = delete;
      Allocation(pot_t alignment, pot_t size) noexcept;
      
      auto GetUses() const noexcept { return mReferences; }
      auto GetSize() const noexcept -> pot_t;
      auto GetBlockStart() const noexcept -> uint8_t*;
      bool Contains(const void*) const noexcept;
      void Keep(int32_t = 1) noexcept;
      void Free(int32_t = 1) noexcept;
   };
}
