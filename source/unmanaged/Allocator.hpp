///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include <Langulus/Core.hpp>

#if LANGULUS_FEATURE(MANAGED_MEMORY)
   #error "This file shouldn't be included if MANAGED_MEMORY is enabled"
#endif

#include "Allocation.hpp"
#include "../rtti/MetaData.hpp"
#include <Langulus/Assume.hpp>
#include "Allocation.inl"


namespace Langulus::Unmanaged
{
   using RTTI::DMeta;

   /// Each allocation has the following order:                               
   /// [sizeof(Allocation)][padding for client data][client bytes...]         
   ///   @param size - the number of client bytes to allocate                 
   ///   @param align - the alignment of the data                             
   ///   @return a newly allocated memory that is correctly aligned           
   inline Allocation* AlignedAllocate(pot_t size, pot_t align) has_assumptions {
      if (align < alignof(Allocation))
         align = alignof(Allocation);
      
      const size_t padding = Allocation::Cost(align);
      const size_t backendSize = padding + static_cast<size_t>(align > size ? align : size);
      #if LANGULUS_COMPILER(MSVC) or LANGULUS_COMPILER(CLANG_CL)
         const auto entry = _aligned_malloc(backendSize, static_cast<size_t>(align));
      #else
         const auto entry = ::std::aligned_alloc(static_cast<size_t>(align), backendSize);
      #endif
      
      if (not entry)
         return nullptr;

      new (entry) Allocation {align, size};
      return reinterpret_cast<Allocation*>(entry);
   }
   
   ///                                                                        
   /// A mockup of a memory manager.                                          
   /// Just uses malloc.                                                      
   ///                                                                        
   struct Allocator {
      /// No state when MANAGED_MEMORY feature is disabled                    
      struct State {
         consteval bool Assert() const noexcept { return true; }
      };

      LANGULUS(INLINED)
      static auto Allocate(pot_t alignment, pot_t size) has_assumptions
      -> Allocation* {
         return AlignedAllocate(size, alignment);
      }

      LANGULUS(INLINED)
      static auto Reallocate(pot_t size, Allocation* previous) has_assumptions
      -> Allocation* {
         LglsAssumeDev(previous,
            "Reallocating nullptr");
         LglsAssumeDev(size != previous->mSize,
            "Reallocation suboptimal - size is same as previous");
         LglsAssumeDev(previous->mReferences,
            "Deallocating an unused allocation");

         (void) previous;
         return Allocate(previous->mAlignment, size);
      }

      LANGULUS(INLINED)
      static void Deallocate(Allocation* entry) has_assumptions {
         LglsAssumeDev(entry,
            "Deallocating nullptr");
         LglsAssumeDev(entry->mReferences,
            "Deallocating an unused allocation");
         LglsAssumeDev(entry->mReferences == 1,
            "Deallocating an allocation used from multiple places");
         
         #if LANGULUS_COMPILER(MSVC) or LANGULUS_COMPILER(CLANG_CL)
            _aligned_free(entry);
         #else
            ::std::free(entry);
         #endif
      }

      static consteval bool CollectGarbage() { return false; }

      #if LANGULUS_FEATURE(MEMORY_STATISTICS)
         static consteval void DumpPools() {}
      #endif
   };
}
