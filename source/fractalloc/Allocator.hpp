///                                                                           
/// Langulus::Fractalloc                                                      
/// Copyright (c) 2015 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include <Langulus/Core.hpp>
#include "../rtti/MetaData.hpp"
#include "Allocation.hpp"
#include "Pool.hpp"

#if not LANGULUS_FEATURE(MANAGED_MEMORY)
   #error "This file shouldn't be included if MANAGED_MEMORY is disabled"
#endif

#if defined(LANGULUS_EXPORT_ALL) or defined(LANGULUS_EXPORT_FRACTALLOC)
   #define LANGULUS_API_FRACTALLOC() LANGULUS_EXPORT()
#else
   #define LANGULUS_API_FRACTALLOC() LANGULUS_IMPORT()
#endif

#if LANGULUS_FEATURE(MEMORY_STATISTICS)
   #include "Statistics.hpp"
#endif


namespace Langulus::Fractalloc
{
   struct Pool;
   using RTTI::DMeta;

   #if not LANGULUS_FEATURE(MEMORY_STATISTICS)
      struct State {
         consteval bool Assert() const noexcept { return true; }
      };
   #endif

   ///                                                                        
   ///   Memory allocator                                                     
   ///                                                                        
   /// Basically an overcomplicated wrapper for malloc/free. Manages pools.   
   struct Allocator {
      Allocator() = delete;
      
      LANGULUS_API(FRACTALLOC)
      static auto Allocate(DMeta, pot_t) has_assumptions -> Allocation*;
      
      template<class T>
      static auto AllocatePacked(DMeta type, pot_t size) has_assumptions -> TAllocation<T>* {
         static_assert(sizeof(TAllocation<T>) == sizeof(Allocation));
         auto a = AllocatePackedInner(T::PoolBits, T::EntryBits, T::OffsetBits, type, size);
         return reinterpret_cast<TAllocation<T>*>(a);
      }

      LANGULUS_API(FRACTALLOC)
      static auto Reallocate(DMeta, pot_t, Allocation*) has_assumptions -> Allocation*;

      template<class T>
      static auto ReallocatePacked(DMeta type, pot_t size, T* prev) has_assumptions -> TAllocation<T>* {
         static_assert(sizeof(TAllocation<T>) == sizeof(Allocation));
         auto a = ReallocatePackedInner(T::PoolBits, T::EntryBits, T::OffsetBits, type, size, reinterpret_cast<Allocation*>(prev));
         return reinterpret_cast<TAllocation<T>*>(a);
      }

      LANGULUS_API(FRACTALLOC)
      static void Deallocate(Allocation*) has_assumptions;
      
      template<class T>
      static void DeallocatePacked(TAllocation<T>* prev) has_assumptions {
         static_assert(sizeof(TAllocation<T>) == sizeof(Allocation));
         Deallocate(reinterpret_cast<Allocation*>(prev));
      }

      template<class T>
      static auto UnpackPointer(T const& ptr) has_assumptions -> typename T::Type* {
         void* a = UnpackPointerInner(ptr.mPool, ptr.mEntry, ptr.mOffset);
         return static_cast<typename T::Type*>(a);
      }

      LANGULUS_API(FRACTALLOC)
      static auto Find(const void*) has_assumptions -> Allocation const*;

      LANGULUS_API(FRACTALLOC)
      static bool CheckAuthority(const void*) has_assumptions;

      LANGULUS_API(FRACTALLOC)
      static auto AllocatePool(DMeta, pot_t) has_assumptions -> Pool*;

      LANGULUS_API(FRACTALLOC)
      static void DeallocatePool(Pool*) has_assumptions;

      LANGULUS_API(FRACTALLOC)
      static bool CollectGarbage();

      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
         LANGULUS_API(FRACTALLOC)
         static size_t CheckBoundary(const Token&) noexcept;
      #endif

      #if LANGULUS_FEATURE(MEMORY_STATISTICS)
         LANGULUS_API(FRACTALLOC)
         static auto GetStatistics() noexcept -> const Statistics&;

         LANGULUS_API(FRACTALLOC)
         static void DumpPools() noexcept;

         LANGULUS_API(FRACTALLOC)
         static void Diff(const Statistics&) noexcept;
         
         LANGULUS_API(FRACTALLOC)
         static bool IntegrityCheck();
      
      private:
         LANGULUS_API(FRACTALLOC)
         static void DumpPool(DMeta type, size_t id, const Pool*) noexcept;
            
         LANGULUS_API(FRACTALLOC)
         static bool IntegrityCheckChain(const Pool*);
      #endif
      
   private:
      LANGULUS_API(FRACTALLOC)
      static auto AllocatePackedInner(
         size_t pool_budget,
         size_t entry_budget,
         size_t element_budget,
         DMeta, pot_t
      ) has_assumptions -> Allocation*;
      
      LANGULUS_API(FRACTALLOC)
      static auto ReallocatePackedInner(
         size_t pool_budget,
         size_t entry_budget,
         size_t element_budget,
         DMeta, pot_t, Allocation*
      ) has_assumptions -> Allocation*;

      LANGULUS_API(FRACTALLOC)
      static void* UnpackPointerInner(
         size_t poolId,
         size_t entryId,
         size_t elementId
      ) has_assumptions;

      LANGULUS_API(FRACTALLOC)
      static auto CollectGarbageChain(Pool*) -> Pool*;
   };   
}
