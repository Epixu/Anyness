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

#include "../fractalloc/Allocation.hpp"
#include "../rtti/MetaData.hpp"
#include <Langulus/Assume.hpp>


namespace Langulus::Unmanaged
{
   using RTTI::DMeta;
   using Fractalloc::Allocation;
   using Fractalloc::MallocHandle;

   /// MSVC will likely never support std::aligned_alloc, so we use           
   /// a custom portable routine that's almost the same                       
   /// https://stackoverflow.com/questions/62962839                           
   ///                                                                        
   /// Each allocation has the following prefixed bytes:                      
   /// [padding][T::GetSize()][client bytes...]                               
   ///                                                                        
   ///   @param size - the number of client bytes to allocate                 
   ///   @return a newly allocated memory that is correctly aligned           
   inline Allocation* AlignedAllocate(size_t size) has_assumptions {
      const auto finalSize = Allocation::GetNewAllocationSize(size) + Alignment;
      const auto base = static_cast<MallocHandle*>(malloc(finalSize));
      if (not base)
         return nullptr;

      // Align pointer to the alignment LANGULUS was built with         
      auto ptr = reinterpret_cast<Allocation*>(
         (reinterpret_cast<uintptr_t>(base) + Alignment)
         & ~(Alignment - uintptr_t {1})
      );

      // Place the entry there                                          
      new (ptr) Allocation {size, base};
      return ptr;
   }
   
   ///                                                                        
   /// A mockup of a memory manager                                           
   /// Just uses malloc                                                       
   ///                                                                        
   struct Allocator {
      /// No state when MANAGED_MEMORY feature is disabled                    
      struct State {
         consteval bool Assert() const noexcept { return true; }
      };

      LANGULUS(INLINED)
      static auto Allocate(DMeta, size_t size) has_assumptions -> Allocation* {
         LglsAssumeDev(size, "Zero allocation is not allowed");
         return AlignedAllocate(size);
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
         return Allocate(nullptr, size);
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

      static constexpr auto Find(DMeta, const void*) noexcept -> const Allocation* {
         return nullptr;
      }

      static constexpr bool CheckAuthority(DMeta, const void*) noexcept {
         return false;
      }

      static consteval bool CollectGarbage() noexcept {
         return false;
      }

      #if LANGULUS_FEATURE(MEMORY_STATISTICS)
         static consteval void DumpPools() noexcept {}
      #endif
   };
}
