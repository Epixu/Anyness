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


namespace Langulus::Unmanaged
{
   using RTTI::DMeta;

   /// MSVC will likely never support std::aligned_alloc, so we use           
   /// a custom portable routine that's almost the same                       
   /// https://stackoverflow.com/questions/62962839                           
   ///                                                                        
   /// Each allocation has the following prefixed bytes:                      
   /// [alignment][padding][allocation][padding][client bytes...]             
   ///                                                                        
   ///   @param size - the number of client bytes to allocate                 
   ///   @return a newly allocated memory that is correctly aligned           
   inline Allocation* AlignedAllocate(size_t size, size_t align) has_assumptions {
      if (align < Alignment)
         align = Alignment;
      
      const size_t padding = Align(sizeof(Allocation), align);
      const size_t backendSize = padding + (align > size ? align : size);
      const auto base = static_cast<uint8_t*>(malloc(backendSize));
      if (not base)
         return nullptr;

      // Place the allocation meta data after it                        
      const auto base_with_offset = Align(base, alignof(Allocation));
      new (base_with_offset) Allocation {align, size, reinterpret_cast<MallocHandle*>(base)};
      return reinterpret_cast<Allocation*>(base_with_offset);
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
      static auto Allocate(DMeta type, size_t size) has_assumptions -> Allocation* {
         LglsAssumeDev(type, "Type must be provided");
         LglsAssumeDev(size, "Zero allocation is not allowed");
         return AlignedAllocate(type.GetAlignment(), size);
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
