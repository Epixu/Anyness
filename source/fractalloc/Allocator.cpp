///                                                                           
/// Langulus::Fractalloc                                                      
/// Copyright (c) 2015 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "Allocator.hpp"
#include "Pool.inl"
#include <unordered_map>
#include <map>
#include <ranges>

#if 0
   #include <Langulus/Logger/EnableVerbose.hpp>
#else
   #include <Langulus/Logger/NoVerbose.hpp>
#endif


namespace
{
   #if LANGULUS_FEATURE(MEMORY_STATISTICS)
      /// Memory statistics                                                   
      Langulus::Fractalloc::Statistics gStatistics {};
   #endif

   /// Default pool chain.                                                    
   /// Won't accept types that have size or alignment larger than Alignment.  
   Langulus::Fractalloc::Pool* gMainPoolChain {};
   
   /// The last succesfull Find() result in default pool chain                
   Langulus::Fractalloc::Pool const* gLastFoundPool {};

   /// Pool chains for types that use PoolTactic::Size and have alignment     
   /// smaller or equal to Langulus::Alignment.                               
   constexpr size_t SizeBuckets = sizeof(size_t) * 8;
   Langulus::Fractalloc::Pool* gSizePoolChain[SizeBuckets] {};
   
   /// The set of types that are currently in use and their corresponding     
   /// pool chains. Used to detect if a shared object is safe to be unloaded. 
   /// Also used to pack/unpack pointers.                                     
   ::std::unordered_map<Langulus::RTTI::DMeta, Langulus::Fractalloc::PoolBank> gTypePoolChain;

   /// The set of all pools. Used to quickly determine if a pointer resides   
   /// inside the memory manager.                                             
   ::std::unordered_map<uintptr_t, Langulus::Fractalloc::Pool*> gPools;

   /// Used to mask pointers in order to determine whether memory belongs to  
   /// us or not. Updated on each new allocated pool.                         
   uintptr_t gPossiblePoolMemorySpace = 0;
}

namespace Langulus::Fractalloc
{   
   /// Helper structure for keeping track of free pool IDs                    
   struct PoolBank {
      using Pool = Langulus::Fractalloc::Pool;
      Pool* unindexed = nullptr;
      ::std::map<unsigned, Pool*> indexed;
      unsigned lastId = 0;
      unsigned freeIds = 0;

      /// Link a new pool, adding it to the unindexed chain, as well as       
      /// giving it a unique ID                                               
      void LinkPool(Pool* pool) {
         // Bring the new pool in front, so that next allocation is     
         // placed in it, as it is most likely still empty.             
         pool->mNext = unindexed;
         unindexed = pool;
         
         // Give the new pool a unique ID as well, so that packed       
         // pointers can utilize it.                                    
         for (unsigned reused = 1; reused < lastId and freeIds; ++reused) {
            // Always try reusing IDs                                   
            if (not indexed.contains(reused)) {
               pool->mID = reused;
               indexed[reused] = pool;
               --freeIds;
               return;
            }
         }

         pool->mID = ++lastId;
         indexed[lastId] = pool;
      }

      /// Unlink a pool (usually before destroying it), from the unindexed    
      /// chain, as well as the ID map                                        
      void UnlinkPool(Pool* pool) {
         if (unindexed == pool)
            unindexed = pool->mNext;

         if (lastId == pool->mID) {
            indexed.erase(pool->mID);
            if (not indexed.empty()) {
               const auto prevLastId = lastId;
               lastId = indexed.rbegin()->first;
               freeIds -= prevLastId - lastId;
            }
            else lastId = freeIds = 0;
            return;
         }

         indexed.erase(pool->mID);
         ++freeIds;
      }
   };
   
   /// Each pool allocation has the following structure:                      
   /// [pool data][alignment][allocation data][alignment][client bytes...]    
   ///   @param type - the pooled type                                        
   ///   @param size - the number of client bytes to allocate                 
   ///   @return a newly allocated memory that is correctly aligned           
   Pool* AlignedAllocate(const DMeta& type, pot_t size) has_assumptions {
      LglsAssumeDev(type,
         "Invalid type");
      LglsAssumeDev(size >= type.GetSize(),
         "Pool can't contain a single instance of provided type");
      
      const pot_t  data_align = type.GetAlignment();
      const pot_t  data_minal = type.GetMinAllocation();
      const size_t size_int = static_cast<size_t>(size);
      const size_t pool_cost = Pool::Cost(data_align, data_minal, size);
      const size_t pool_alignment = ::std::bit_ceil(pool_cost);
      const size_t backendSize = pool_cost + size_int;
      #if LANGULUS_COMPILER(MSVC) or LANGULUS_COMPILER(CLANG_CL)
         const auto pool = _aligned_malloc(backendSize, pool_alignment);
      #else
         const auto pool = ::std::aligned_alloc(pool_alignment, backendSize);
      #endif

      if (not pool)
         return nullptr;

      new (pool) Pool {data_align, data_minal, pot_t(pool_alignment), size};
      auto typed_pool = static_cast<Pool*>(pool);

      // Add all pointer masks that point to the pool                   
      auto ptr = reinterpret_cast<uintptr_t>(pool);
      const auto ptrEnd = reinterpret_cast<uintptr_t>(typed_pool->GetClientData())
                        + static_cast<uintptr_t>(typed_pool->GetAllocatedByBackend());
      const auto ptrStep = ptr & -ptr;
      gPools[ptr] = typed_pool;
      gPossiblePoolMemorySpace |= ptr;
      
      while (ptr + ptrStep < ptrEnd) {
         ptr += ptrStep;
         gPools[ptr] = typed_pool;
         gPossiblePoolMemorySpace |= ptr;
      }      
      
      return typed_pool;
   }

   /// Allocate a memory entry                                                
   ///   @attention doesn't call any constructors                             
   ///   @attention doesn't throw - check if return is nullptr                
   ///   @attention assumes meta data is valid                                
   ///   @param meta - meta data for finding the proper pool                  
   ///   @param size - the number of bytes to allocate                        
   ///   @return the allocation, or nullptr if out of memory                  
   auto Allocator::Allocate(DMeta meta, pot_t size) has_assumptions -> Allocation* {
      // Decide pool chain based on meta data                           
      LglsAssumeDev(meta, "Invalid meta data");
      Pool* pool = nullptr;
      const auto poolTactic = meta.GetPoolTactic();
      if (meta) {
         switch (poolTactic) {
         case PoolTactic::Size:
            pool = gSizePoolChain[FastLog2(meta.GetSize())];
            break;
         case PoolTactic::Type: {
            auto found = gTypePoolChain.find(meta);
            if (found != gTypePoolChain.end())
               pool = found->second.unindexed;
            break;
         }
         case PoolTactic::Main:
            pool = gMainPoolChain;
            break;
         }
      }
      else pool = gMainPoolChain;

      //	Attempt to place allocation in the chosen chain                
      unsigned pool_misses = 0;
      Allocation* entry = nullptr;
      while (pool) {
         entry = pool->Allocate(size);
         if (entry)
            break;
         pool = pool->mNext;
         ++pool_misses;
      }

      if (entry) {
         // We're done                                                  
         #if LANGULUS_FEATURE(MEMORY_STATISTICS)
            gStatistics.mEntries += 1;
            gStatistics.mBytesAllocatedByFrontend += static_cast<size_t>(entry->GetSize());
            LglsAssumeDev(
               gStatistics.mBytesAllocatedByFrontend <= gStatistics.mBytesAllocatedByBackend,
               "Impossible amount of frontend allocation"
            );
         #endif
         return entry;
      }

      //                                                                
      // If reached, chosen pool chain can't contain the memory.        
      // Allocate a new pool and add it at the front of the chain.      
      // Make new pool bigger, depending on how many pool misses we had.
      // Many pool misses indicate that type is hot and used often.     
      const pot_t new_pool_size = meta.GetMinPoolsize() << (pool_misses / 2);
      pool = AllocatePool(meta, size < new_pool_size ? new_pool_size : size);
      if (not pool)
         return nullptr;

      LglsVerbose(
         "Fractalloc: ", Logger::Cyan, "New pool ", Logger::Hex(pool),
         " of size ", Logger::Size {static_cast<size_t>(pool->GetAllocatedByBackend())}
      );

      // Place allocation in the new pool. This is guaranteed to work.  
      entry = pool->Allocate(size);

      // Time to update the pool chain with the new pool.               
      switch (poolTactic) {
      case PoolTactic::Size: {
         // If we're using the size pool chain, bring the new pool in   
         // front, so that next allocation is placed in it, as it is    
         // most likely still empty.                                    
         auto& sizeChain = gSizePoolChain[FastLog2(meta.GetSize())];
         pool->mNext = sizeChain;
         sizeChain = pool;
         break;
      }
      case PoolTactic::Type: {
         // Get the pool bank corresponding to the meta data. If such   
         // doesn't exist, it will be implicitly created.               
         auto& poolBank = gTypePoolChain[meta];
         poolBank.LinkPool(pool);
         break;
      }
      case PoolTactic::Main:
         pool->mNext = gMainPoolChain;
         gMainPoolChain = pool;
         break;
      }

      IF_LANGULUS_MEMORY_STATISTICS(gStatistics.AddPool(pool));
      return entry;
   }
   
#if LANGULUS_FEATURE(MEMORY_STATISTICS)
   /// Get allocator statistics                                               
   ///   @return a reference to the statistics structure                      
   auto Allocator::GetStatistics() noexcept -> const Statistics& {
      return gStatistics;
   }
   
   /// Dump a single pool                                                     
   ///   @param type - pool type                                              
   ///   @param id - pool id                                                  
   ///   @param pool - the pool to dump                                       
   void Allocator::DumpPool(DMeta type, size_t id, const Pool* pool) noexcept {
      const auto scope = Logger::InfoScoped(
         Logger::PushCyan, Logger::Underline, "Pool #", id, " at ",
         Logger::Hex(pool), Logger::Pop
      );

      Logger::Line("In use/reserved: ", 
         Logger::PushGreen, Logger::Size {pool->mAllocatedByFrontend}, Logger::Pop,
         '/',
         Logger::PushRed, Logger::Size {static_cast<size_t>(pool->mAllocatedByBackend)}, Logger::Pop
      );

      Logger::Line("Min/Current/Max threshold: ",
         Logger::PushGreen, Logger::Size {static_cast<size_t>(pool->mThresholdMin)}, Logger::Pop,
         '/',
         Logger::PushYellow, Logger::Size {static_cast<size_t>(pool->mThresholdMax)}, Logger::Pop,
         '/',
         Logger::PushRed, Logger::Size {static_cast<size_t>(pool->mAllocatedByBackend)}, Logger::Pop
      );

      if (type) {
         Logger::Line("Associated type: `",
            type.GetCppName(), "`, of size ", type.GetSize());
      }

      if (pool->mNextEntry) {
         const auto escope = Logger::Section("Entries: ",
            Logger::PushGreen, pool->mNextEntry, Logger::Pop
         );

         size_t consecutiveEmpties = 0;
         size_t ecounter = 0;
         do {
            const auto entry = pool->AllocationFromIndex(ecounter);
            if (entry->mReferences) {
               if (consecutiveEmpties) {
                  if (consecutiveEmpties == 1)
                     Logger::Line(Logger::Red, ecounter-1, "] ", "unused entry");
                  else
                     Logger::Line(Logger::Red, ecounter - consecutiveEmpties, '-', ecounter-1, "] ",
                        consecutiveEmpties, " unused entries");
                  consecutiveEmpties = 0;
               }

               Logger::Line(
                  Logger::Green, ecounter, "] ", Logger::Hex(entry), " ",
                  Logger::Size {static_cast<size_t>(entry->GetSize())}, ", ",
                  entry->mReferences, " references: `"
               );

               auto raw = entry->GetBlockStart();
               for (size_t i = 0; i < ::std::min(size_t {16}, static_cast<size_t>(entry->GetSize())); ++i) {
                  if (::isprint(raw[i]))
                     Logger::Append(static_cast<char>(raw[i]));
                  else
                     Logger::Append('?');
               }

               if (entry->GetSize() > 16u)
                  Logger::Append("...`");
               else
                  Logger::Append('`');
            }
            else ++consecutiveEmpties;
         }
         while (++ecounter < pool->mNextEntry);

         if (consecutiveEmpties) {
            if (consecutiveEmpties == 1)
               Logger::Line(Logger::Red, ecounter-1, "] ", "unused entry");
            else
               Logger::Line(Logger::Red, ecounter - consecutiveEmpties, '-', ecounter-1, "] ",
                  consecutiveEmpties, " unused entries");
            consecutiveEmpties = 0;
         }
      }
   }

   /// Dump all currently allocated pools and entries, useful to locate leaks 
   void Allocator::DumpPools() noexcept {
      auto section = Logger::InfoScoped("MANAGED MEMORY POOL DUMP");

      // Dump default pool chain                                        
      if (gMainPoolChain) {
         const auto scope = Logger::InfoScoped(Logger::Purple, "MAIN POOL CHAIN: ");
         size_t counter = 0;
         auto pool = gMainPoolChain;
         while (pool) {
            DumpPool({}, counter, pool);
            pool = pool->mNext;
            ++counter;
         }
      }

      // Dump every size pool chain                                     
      for (size_t size = 0; size < sizeof(size_t) * 8; ++size) {
         if (not gSizePoolChain[size])
            continue;

         const auto scope = Logger::InfoScoped(Logger::Purple, 
            "SIZE POOL CHAIN FOR ", Logger::Red, Logger::Size {1ul << size},
            Logger::Purple, ": "
         );

         size_t counter = 0;
         auto pool = gSizePoolChain[size];
         while (pool) {
            DumpPool({}, counter, pool);
            pool = pool->mNext;
            ++counter;
         }
      }
      
      // Dump every type pool chain                                     
      for (auto& type : gTypePoolChain) {
         #if LANGULUS_FEATURE(MANAGED_REFLECTION)
            const auto scope = Logger::InfoScoped(Logger::Purple, 
               "TYPE POOL CHAIN FOR `", Logger::Red, type.first.GetCppName(), 
               Logger::Purple, "` (boundaries: "
            );
            
            if (type.first.GetBoundaries().empty())
               Logger::Append("MAIN");
            else for (auto& boundary : type.first.GetBoundaries())
               Logger::Append(boundary, ' ');
            
            Logger::Append(Logger::Purple, "): ");
         #else
            const auto scope = Logger::InfoScoped(Logger::Purple, 
               "TYPE POOL CHAIN FOR `", Logger::Red, type.first.GetCppName(), 
               Logger::Purple, '`'
            );
         #endif

         for (auto& val : type.second.indexed | std::views::values)
            DumpPool(type.first, val->mID, val);
      }
   }

   /// Compare two statistics snapshots, and find the difference              
   void Allocator::Diff(const Statistics& with) noexcept {
      auto section = Logger::InfoScoped("MANAGED MEMORY DIFF");
      auto& stats = GetStatistics();

      if (stats.mBytesAllocatedByBackend != with.mBytesAllocatedByBackend) {
         Logger::Info(Logger::Purple,
            "Allocated byte difference: ",
            static_cast<int>(stats.mBytesAllocatedByBackend) - static_cast<int>(with.mBytesAllocatedByBackend));
      }

      if (stats.mBytesAllocatedByFrontend != with.mBytesAllocatedByFrontend) {
         Logger::Info(Logger::Purple,
            "Used byte difference: ",
            static_cast<int>(stats.mBytesAllocatedByFrontend) - static_cast<int>(with.mBytesAllocatedByFrontend));
      }

   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      if (stats.mDataDefinitions != with.mDataDefinitions) {
         const auto scope = Logger::InfoScoped(Logger::Purple,
            "Data definitions difference: ",
            static_cast<int>(stats.mDataDefinitions) - static_cast<int>(with.mDataDefinitions)
         );
      }
   #endif

      if (stats.mPools != with.mPools) {
         const auto scope = Logger::InfoScoped(Logger::Purple,
            "Pool difference: ", static_cast<int>(stats.mPools) - static_cast<int>(with.mPools)
         );

         // Diff default pool chain                                     
         if (gMainPoolChain) {
            size_t counter = 0;
            auto pool = gMainPoolChain;
            while (pool) {
               if (pool->mStep > with.mStep) {
                  Logger::Info(Logger::Purple, "Default pool: ");
                  DumpPool({}, counter, pool);
               }
               pool = pool->mNext;
               ++counter;
            }
         }

         // Dump every size pool chain                                  
         for (size_t size = 0; size < sizeof(size_t) * 8; ++size) {
            if (not gSizePoolChain[size])
               continue;

            size_t counter = 0;
            auto pool = gSizePoolChain[size];
            while (pool) {
               if (pool->mStep > with.mStep) {
                  Logger::Info(Logger::Purple, "Size ", Logger::Size {1ul << size}, " pool: ");
                  DumpPool({}, counter, pool);
               }
               pool = pool->mNext;
               ++counter;
            }
         }

         // Dump every type pool chain                                  
         for (auto& type : gTypePoolChain) {
            for (auto& val : type.second.indexed | std::views::values) {
               if (val->mStep <= with.mStep)
                  continue;

               Logger::Info(Logger::Purple, "Type ", type.first.GetCppName(), " pool: ");
               #if LANGULUS_FEATURE(MANAGED_REFLECTION)
                  Logger::Info(Logger::Purple, "(boundaries: ");
                  if (type.first.GetBoundaries().empty())
                     Logger::Append("MAIN");
                  else for (auto& boundary : type.first.GetBoundaries())
                     Logger::Append(boundary, ' ');
                  Logger::Append(')');
               #endif
               DumpPool(type.first, val->mID, val);
            }
         }
      }

      if (stats.mEntries != with.mEntries) {
         const auto scope = Logger::InfoScoped(Logger::Purple,
            "Entries difference: ", int(stats.mEntries) - int(with.mEntries)
         );
      }

   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      if (stats.mTraitDefinitions != with.mTraitDefinitions) {
         const auto scope = Logger::InfoScoped(Logger::Purple,
            "Trait definitions difference: ",
            int(stats.mTraitDefinitions) - int(with.mTraitDefinitions)
         );
      }

      if (stats.mVerbDefinitions != with.mVerbDefinitions) {
         const auto scope = Logger::InfoScoped(Logger::Purple,
            "Verb definitions difference: ",
            int(stats.mVerbDefinitions) - int(with.mVerbDefinitions)
         );
      }
   #endif
   }
   
   /// Integrity check a pool chain                                           
   ///   @param pool - [in/out] the start of the chain                        
   ///   @return true if all checks passed                                    
   bool Allocator::IntegrityCheckChain(const Pool* pool) {
      while (pool) {
         if (pool->IsInUse()) {
            size_t validAllocations = 0;
            size_t validBytes = 0;
            for (size_t i = 0; i < pool->mNextEntry; ++i) {
               auto allocation = pool->AllocationFromIndex(i);
               if (allocation->mReferences) {
                  if (allocation->mReferences > 100000) {
                     Logger::Warning(
                        "Fractalloc: Suspicious reference count in allocation ",
                        Logger::Hex(allocation), " of size ", allocation->GetSize(),
                        " in pool ", Logger::Hex(pool), ", entry ", i, "/", pool->mNextEntry
                     );
                  }

                  ++validAllocations;
                  validBytes += static_cast<size_t>(allocation->GetSize());
               }
            }

            //TODO also check if negative memory space contains a predefined pattern,
            // in order to detect writing outside boundaries

            bool failure = false;
            if (validAllocations != pool->mValidEntries) {
               Logger::Error("Fractalloc: Valid entry mismatch: found ",
                  validAllocations, " entries, but ",
                  pool->mValidEntries, " were actually registered in pool ",
                  Logger::Hex(pool)
               );
               failure = true;
            }

            if (validBytes != pool->mAllocatedByFrontend) {
               Logger::Error("Fractalloc: Valid byte usage mismatch: found ",
                  validBytes, " bytes in use, but ",
                  pool->mAllocatedByFrontend, " were actually registered in pool ",
                  Logger::Hex(pool)
               );
               failure = true;
            }

            if (failure)
               return false;
         }

         pool = pool->mNext;
      }

      return true;
   }
   
   /// Integrity checks                                                       
   ///   @return true if no memory errors occured                             
   bool Allocator::IntegrityCheck() {
      // Integrity check the default chain                              
      if (gMainPoolChain) {
         LglsVerbose("Integrity check: gMainPoolChain...");
         if (not IntegrityCheckChain(gMainPoolChain))
            return false;
      }

      // Integrity check all size chains                                
      [[maybe_unused]] int size = 1;
      for (auto& sizeChain : gSizePoolChain) {
         if (sizeChain) {
            LglsVerbose("Integrity check: gSizePoolChain #", size++, "...");
            if (not IntegrityCheckChain(sizeChain))
               return false;
         }
      }
      
      // Integrity check all type chains                                
      for (auto& type : gTypePoolChain) {
         for (auto& val : type.second.indexed | std::views::values) {
            LglsVerbose("Integrity check for type ", type.first.GetName(), "...");
            if (not IntegrityCheckChain(val))
               return false;
         }
      }

      return true;
   }
#endif

   /// Reallocate a memory entry                                              
   ///   @attention never calls any constructors                              
   ///   @attention never copies any data                                     
   ///   @attention never deallocates previous entry                          
   ///   @attention returned entry might be different from the previous       
   ///   @attention doesn't throw - check if return is nullptr                
   ///   @param type - the type of the allocation                             
   ///   @param size - the number of bytes to allocate                        
   ///   @param previous - the previous memory entry                          
   ///   @return the reallocated memory entry, or nullptr if out of memory    
   auto Allocator::Reallocate(DMeta type, pot_t size, Allocation* previous)
   has_assumptions -> Allocation* {
      LglsAssumeDevAndOptimize(previous,
         "Reallocating nullptr");
      LglsAssumeDev(size != previous->GetSize(),
         "Reallocation suboptimal - size is same as previous");
      LglsAssumeDevAndOptimize(previous->mReferences,
         "Reallocating an unused allocation");
      LglsAssumeDevAndOptimize(previous->mReferences == 1,
         "Reallocating allocation used from multiple places");

      // New size is bigger, precautions must be taken                  
      [[maybe_unused]] const auto oldSize = static_cast<size_t>(previous->GetSize());
      auto pool = const_cast<Pool*>(previous->GetPool());
      if (pool->Reallocate(previous, size)) {
         #if LANGULUS_FEATURE(MEMORY_STATISTICS)
            auto& stats = gStatistics;
            stats.mBytesAllocatedByFrontend -= oldSize;
            stats.mBytesAllocatedByFrontend += static_cast<size_t>(previous->GetSize());
            LglsAssumeDev(
               stats.mBytesAllocatedByFrontend <= stats.mBytesAllocatedByBackend,
               "Impossible amount of frontend allocation"
            );
         #endif

         LglsVerbose(
            "Fractalloc: ", Logger::Yellow, "Allocation ", Logger::Hex(previous),
            " was reallocated from ", Logger::Size {oldSize}, " to ",
            Logger::Size {static_cast<size_t>(previous->GetSize())}
         );
         return previous;
      }

      // If this is reached we have a collision, so new entry is made   
      return Allocate(type, size);
   }
   
   /// Deallocate a memory allocation                                         
   ///   @attention assumes entry is a valid entry under jurisdiction         
   ///   @attention doesn't call any destructors                              
   ///   @param entry - the memory entry to deallocate                        
   void Allocator::Deallocate(Allocation* entry) has_assumptions {
      LglsAssumeDevAndOptimize(entry,
         "Deallocating nullptr");
      LglsAssumeDevAndOptimize(entry->mReferences,
         "Deallocating an unused allocation");
      LglsAssumeDevAndOptimize(entry->mReferences == 1,
         "Deallocating an allocation used from multiple places");

      [[maybe_unused]] const auto backupSize = static_cast<size_t>(entry->GetSize());
      LglsVerbose(
         "Fractalloc: ", Logger::Red, "Allocation ", Logger::Hex(entry),
         " of size ", Logger::Size {backupSize}, " was deallocated (had ",
         entry->mReferences, " references)"
      );

      auto pool = const_cast<Pool*>(entry->GetPool());
      pool->Deallocate(entry);

      #if LANGULUS_FEATURE(MEMORY_STATISTICS)
         auto& stats = gStatistics;
         stats.mBytesAllocatedByFrontend -= backupSize;
         stats.mEntries -= 1;
      #endif
   }

   /// Allocate a pool of custom size                                         
   ///   @attention the pool must be deallocated with DeallocatePool          
   ///   @param type - meta data to associate pool with                       
   ///   @param size - the client requested size of the pool (in bytes)       
   ///   @return a pointer to the new pool                                    
   Pool* Allocator::AllocatePool(DMeta type, pot_t size) has_assumptions {
      return AlignedAllocate(type, size);
   }

   /// Deallocate a pool                                                      
   ///   @attention doesn't call any destructors                              
   ///   @attention entries inside are no longer valid after this             
   ///   @attention assumes pool is a valid pointer                           
   ///   @param pool - the pool to deallocate                                 
   void Allocator::DeallocatePool(Pool* pool) has_assumptions {
      LglsAssumeDevAndOptimize(pool, "Nullptr provided");
      if (gLastFoundPool == pool)
         gLastFoundPool = nullptr;

      // Remove all pointer masks that map to the pool                  
      auto ptr = reinterpret_cast<uintptr_t>(pool);
      const auto ptrEnd = reinterpret_cast<uintptr_t>(pool->GetClientData())
                        + static_cast<uintptr_t>(pool->GetAllocatedByBackend());
      const auto ptrStep = ptr & -ptr;
      gPools.erase(ptr);
      while (ptr + ptrStep < ptrEnd) {
         ptr += ptrStep;
         gPools.erase(ptr);
      }

      #if LANGULUS_COMPILER(MSVC) or LANGULUS_COMPILER(CLANG_CL)
         _aligned_free(pool);
      #else
         ::std::free(pool);
      #endif
   }

   /// Trims and eventually deallocates all unused pools in a chain           
   ///   @param chainStart - the start of the chain                           
   ///   @return the new start of the chain                                   
   Pool* Allocator::CollectGarbageChain(Pool* chainStart) {
      Pool* prev = nullptr;
      Pool* pool = chainStart;
      while (pool) {
         if (pool->IsInUse()) {
            // Pool is in use, just trim it and move on                 
            pool->Trim();
            prev = pool;
            pool = pool->mNext;
            continue;
         }

         // If reached, the pool is not in use and is deleted           
         IF_LANGULUS_MEMORY_STATISTICS(gStatistics.DelPool(pool));
         const auto next = pool->mNext;
         LglsVerbose(
            "Fractalloc: ", Logger::DarkCyan, "Pool ", Logger::Hex(pool),
            " of size ", Logger::Size {static_cast<size_t>(pool->GetAllocatedByBackend())},
            " was deallocated"
         );
         
         if (chainStart == pool)
            chainStart = next;
         
         DeallocatePool(pool);
         pool = next;         
         if (prev)
            prev->mNext = pool;
      }
      
      return chainStart;
   }
   
   /// Deallocates all unused pools                                           
   ///   @return true if there's at least one pool remaining allocated        
   bool Allocator::CollectGarbage() {
      bool result = false;
      gLastFoundPool = nullptr;

      // Cleanup the main chain                                         
      gMainPoolChain = CollectGarbageChain(gMainPoolChain);
      if (gMainPoolChain)
         result = true;

      // Cleanup all size chains                                        
      for (auto& sizeChain : gSizePoolChain) {
         sizeChain = CollectGarbageChain(sizeChain);
         if (sizeChain)
            result = true;
      }

      // Cleanup all type chains                                        
      for (auto t = gTypePoolChain.begin(); t != gTypePoolChain.end();) {
         Pool* prev = nullptr;
         Pool* pool = t->second.unindexed;
         while (pool) {
            if (pool->IsInUse()) {
               // Pool is in use, just trim it and move on              
               pool->Trim();
               prev = pool;
               pool = pool->mNext;
               continue;
            }

            // If reached, the pool is not in use and is deleted        
            IF_LANGULUS_MEMORY_STATISTICS(gStatistics.DelPool(pool));
            const auto next = pool->mNext;
            LglsVerbose(
               "Fractalloc: ", Logger::DarkCyan, "Typed pool ", Logger::Hex(pool),
               " of size ", Logger::Size {static_cast<size_t>(pool->GetAllocatedByBackend())},
               " was deallocated"
            );
            
            t->second.UnlinkPool(pool);
            DeallocatePool(pool);
            pool = next;         
            if (prev)
               prev->mNext = pool;
         }

         // Also discard the type chain if no pools remain in it        
         if (not t->second.unindexed)
            t = gTypePoolChain.erase(t);
         else
            ++t;
      }

      return result or not gTypePoolChain.empty();
   }
   
#if LANGULUS_FEATURE(MANAGED_REFLECTION)
   /// Check RTTI boundary for allocated pools.                               
   /// Useful to decide when shared library is no longer used and is ready    
   /// to be unloaded. Best used after a call to CollectGarbage.              
   ///   @param boundary - the boundary name                                  
   ///   @return the number of pools                                          
   size_t Allocator::CheckBoundary(const Token& boundary) noexcept {
      const ::std::string b {boundary};
      size_t count = 0;
      for (const auto& type : gTypePoolChain) {
         if (type.first.GetBoundaries().contains(b))
            count += type.second.indexed.size();
      }
      return count;
   }
#endif

   /// Find a memory entry from pointer.                                      
   /// Allows us to safely interface unknown memory, possibly reusing it.     
   ///   @param memory memory pointer                                         
   ///   @attention assumes memory is a valid pointer                         
   ///   @return the memory entry that contains the memory pointer, or        
   ///      nullptr if memory is not ours, or entry is not in use             
   auto Allocator::Find(const void* memory) has_assumptions -> const Allocation* {
      LglsAssumeDevAndOptimize(memory, "Nullptr provided");
      
      // Check the last pool that found something (hot region)          
      if (gLastFoundPool) {
         if (auto found = gLastFoundPool->Find(memory))
            return found;
      }

      // Mask out the pointer in order to locate the owning pool        
      uintptr_t test = reinterpret_cast<uintptr_t>(memory) & gPossiblePoolMemorySpace;
      while (test) {
         auto found = gPools.find(test);
         if (found != gPools.end()) {
            gLastFoundPool = found->second;
            return found->second->Find(memory);
         }

         // Continue shrinking the mask until pointer becomes zero      
         test &= ~(-test);                // Flips only the lowest bit  
      }

      // If reached, then memory is out of jurisdiction                 
      return nullptr;
   }

   /// Check if memory is owned by the memory manager.                        
   /// Unlike Allocator::Find, this doesn't check if memory is currently used 
   /// but returns true, as long as the required pool is still available.     
   ///   @attention assumes memory is a valid pointer                         
   ///   @param memory - memory pointer                                       
   ///   @return true if we own the memory                                    
   bool Allocator::CheckAuthority(const void* memory) has_assumptions {
      LglsAssumeDevAndOptimize(memory, "Nullptr provided");

      // Check the last pool that found something (hot region)          
      if (gLastFoundPool and gLastFoundPool->ContainsData(memory))
         return true;
      
      // Mask out the pointer in order to locate the owning pool        
      uintptr_t test = reinterpret_cast<uintptr_t>(memory) & gPossiblePoolMemorySpace;
      while (test) {
         auto found = gPools.find(test);
         if (found != gPools.end()) {
            gLastFoundPool = found->second;
            return found->second->ContainsData(memory);
         }

         // Continue shrinking the mask until pointer becomes zero      
         test &= ~(-test);                // Flips only the lowest bit  
      }

      // If reached, then memory is out of jurisdiction                 
      return false;
   }
}
