///                                                                           
/// Langulus::Fractalloc                                                      
/// Copyright (c) 2015 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#if not LANGULUS_FEATURE(MANAGED_MEMORY)
#error "This file shouldn't be included if MANAGED_MEMORY is disabled"
#endif

#include "Pool.hpp"
#include <unordered_set>
#include <optional>

#if defined(LANGULUS_EXPORT_ALL) or defined(LANGULUS_EXPORT_FRACTALLOC)
   #define LANGULUS_API_FRACTALLOC() LANGULUS_EXPORT()
#else
   #define LANGULUS_API_FRACTALLOC() LANGULUS_IMPORT()
#endif


namespace Langulus::Fractalloc
{

   ///                                                                        
   ///   Memory allocator                                                     
   ///                                                                        
   /// The lowest-level memory management interface                           
   /// Basically an overcomplicated wrapper for malloc/free                   
   ///                                                                        
   struct Allocator {
      #if LANGULUS_FEATURE(MEMORY_STATISTICS)
         ///                                                                  
         /// Structure for keeping track of allocations                       
         ///                                                                  
         struct Statistics {
            // The real allocated bytes, provided by malloc in backend  
            Size mBytesAllocatedByBackend {};
            // The bytes allocated by the frontend                      
            Size mBytesAllocatedByFrontend {};
            // Number of registered entries                             
            Size mEntries {};
            // Number of registered pools                               
            Size mPools {};
            // Increases with each call to State::Assert, used to       
            // diff pools                                               
            Size mStep {};

            #if LANGULUS_FEATURE(MANAGED_REFLECTION)
               // Number of registered meta datas                       
               Size mDataDefinitions {};
               // Number of registered meta traits                      
               Size mTraitDefinitions {};
               // Number of registered meta verbs                       
               Size mVerbDefinitions {};
            #endif

            bool operator == (const Statistics&) const has_assumptions;

            void AddPool(const Pool*) has_assumptions;
            void DelPool(const Pool*) has_assumptions;
         };
      
         ///                                                                  
         /// Structure that holds a single memory manager state, used for     
         /// comparing states in order to detect leaks while testing          
         ///                                                                  
         struct State {
         private:
            // The previous state                                       
            ::std::optional<Statistics> mState;

         public:
            LANGULUS_API(FRACTALLOC) bool Assert();
         };

      private:
         // The current memory manager statistics                       
         Statistics mStatistics {};
      #else
         /// No state when MEMORY_STATISTICS feature is disabled              
         struct State {
            consteval bool Assert() const noexcept { return true; }
         };
      #endif

   private:
      // Default pool chain                                             
      Pool* mMainPoolChain {};
      // The last succesfull Find() result in default pool chain        
      mutable const Pool* mLastFoundPool {};

      // Pool chains for types that use PoolTactic::Size                
      static constexpr Size SizeBuckets = sizeof(Size) * 8;
      Pool* mSizePoolChain[SizeBuckets] {};

      // A set of types, that are currently in use                      
      // Used to detect if a shared object is safe to be unloaded       
      // MUST BE BY POINTER, because there can be multiple definitions  
      ::std::unordered_set<const DMeta*> mInstantiatedTypes;

   private:
      #if LANGULUS_FEATURE(MEMORY_STATISTICS)
         LANGULUS_API(FRACTALLOC)
         static void DumpPool(Size, const Pool*) noexcept;
         
         LANGULUS_API(FRACTALLOC)
         bool IntegrityCheckChain(const Pool*);
      #endif

      LANGULUS_API(FRACTALLOC)
      void CollectGarbageChain(Pool*&);

      auto FindInChain(const void*, const Pool*) const has_assumptions -> const Allocation*;
      bool ContainedInChain(const void*, const Pool*) const has_assumptions;

      static void DumpAllocation(DMeta hint, const Pool*, const Allocation*) noexcept;

   public:
      LANGULUS_API(FRACTALLOC)
      static auto Allocate(DMeta, Size) has_assumptions -> Allocation*;

      LANGULUS_API(FRACTALLOC)
      static auto Reallocate(Size, Allocation*) has_assumptions-> Allocation*;

      LANGULUS_API(FRACTALLOC)
      static void Deallocate(Allocation*) has_assumptions;

      LANGULUS_API(FRACTALLOC)
      static const Allocation* Find(DMeta, const void*) has_assumptions;

      LANGULUS_API(FRACTALLOC)
      static bool CheckAuthority(DMeta, const void*) has_assumptions;

      LANGULUS_API(FRACTALLOC)
      static Pool* AllocatePool(DMeta, Size) has_assumptions;

      LANGULUS_API(FRACTALLOC)
      static void DeallocatePool(Pool*) has_assumptions;

      LANGULUS_API(FRACTALLOC)
      static bool CollectGarbage();

      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
         LANGULUS_API(FRACTALLOC)
         static Size CheckBoundary(const Token&) noexcept;
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
   };


   ///                                                                        
   ///   The global memory manager instance                                   
   ///                                                                        
   LANGULUS_API(FRACTALLOC) extern Allocator Instance;

} // namespace Langulus::Fractalloc

#include "Allocation.inl"
#include "Pool.inl"
