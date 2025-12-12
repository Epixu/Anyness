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
   /// The lowest-level memory management interface.                          
   /// Basically an overcomplicated wrapper for malloc/free.                  
   ///                                                                        
   struct Allocator {
      Allocator() = delete;
      
      LANGULUS_API(FRACTALLOC)
      static auto Allocate(DMeta, pot_t) has_assumptions -> Allocation*;

      LANGULUS_API(FRACTALLOC)
      static auto Reallocate(DMeta, pot_t, Allocation*) has_assumptions -> Allocation*;

      LANGULUS_API(FRACTALLOC)
      static void Deallocate(Allocation*) has_assumptions;

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
      #endif
      
   private:
      #if LANGULUS_FEATURE(MEMORY_STATISTICS)
      LANGULUS_API(FRACTALLOC)
      static void DumpPool(DMeta type, size_t id, const Pool*) noexcept;
         
      LANGULUS_API(FRACTALLOC)
      static bool IntegrityCheckChain(const Pool*);
      #endif

      LANGULUS_API(FRACTALLOC)
      static auto CollectGarbageChain(Pool*) -> Pool*;
   };   
}

//#include "Allocation.inl"
