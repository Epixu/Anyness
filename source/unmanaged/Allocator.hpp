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

   /// MSVC will likely never support std::aligned_alloc, so we use           
   /// a custom portable routine that's almost the same                       
   /// https://stackoverflow.com/questions/62962839                           
   ///                                                                        
   /// Each allocation has the following prefixed bytes:                      
   /// [allocation padding][allocation][client padding][client bytes...]      
   ///                                                                        
   ///   @param size - the number of client bytes to allocate                 
   ///   @param align - the alignment of the data                             
   ///   @return a newly allocated memory that is correctly aligned           
   inline Allocation* AlignedAllocate(size_t size, size_t align) has_assumptions {
      if (align < Alignment)
         align = Alignment;
      
      // We don't know what kind of alignment malloc() will return, so  
      // add some additional bytes in order to move pointer if needed   
      const size_t padding = Align(sizeof(Allocation) + Alignment, align);
      const size_t backendSize = padding + (align > size ? align : size);
      const auto base = static_cast<uint8_t*>(malloc(backendSize));
      if (not base)
         return nullptr;

      const auto aligned_base = Align(base, alignof(Allocation));
      new (aligned_base) Allocation {align, size, reinterpret_cast<MallocHandle*>(base)};
      return reinterpret_cast<Allocation*>(aligned_base);
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
      static auto Allocate(size_t alignment, size_t size) has_assumptions -> Allocation* {
         LglsAssumeDev(size, "Zero allocation is not allowed");
         return AlignedAllocate(size, alignment);
      }

      LANGULUS(INLINED)
      static auto Reallocate(size_t size, Allocation* previous) has_assumptions -> Allocation* {
         LglsAssumeDev(previous,
            "Reallocating nullptr");
         LglsAssumeDev(size != previous->GetFrontendSize(),
            "Reallocation suboptimal - size is same as previous");
         LglsAssumeDev(size,
            "Zero reallocation is not allowed - deallocate instead");
         LglsAssumeDev(previous->mReferences,
            "Deallocating an unused allocation");

         (void) previous;
         return Allocate(previous->mAlignment, size);
      }

      LANGULUS(INLINED)
      static void Deallocate(Allocation* entry) has_assumptions {
         LglsAssumeDev(entry,
            "Deallocating nullptr");
         LglsAssumeDev(entry->GetFrontendSize(),
            "Deallocating an empty allocation");
         LglsAssumeDev(entry->mReferences,
            "Deallocating an unused allocation");
         LglsAssumeDev(entry->mReferences == 1,
            "Deallocating an allocation used from multiple places");

         free(entry->mMallocHandle);
      }

      static consteval bool CollectGarbage() noexcept {
         return false;
      }

      #if LANGULUS_FEATURE(MEMORY_STATISTICS)
         static consteval void DumpPools() noexcept {}
      #endif
   };
}
