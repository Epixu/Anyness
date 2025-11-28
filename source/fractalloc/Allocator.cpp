///                                                                           
/// Langulus::Fractalloc                                                      
/// Copyright (c) 2015 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "Allocator.hpp"
#include "Pool.inl"
#include "Allocation.inl"

#if 0
   #include <Langulus/Logger/EnableVerbose.hpp>
#else
   #include <Langulus/Logger/NoVerbose.hpp>
#endif


namespace Langulus::Fractalloc
{
   /// Each allocation has the following order:                               
   /// [sizeof(Pool)][padding for client data][client bytes...]               
   ///   @param type - the pooled type                                        
   ///   @param size - the number of client bytes to allocate                 
   ///   @return a newly allocated memory that is correctly aligned           
   Pool* AlignedAllocate(const DMeta& type, pot_t size) has_assumptions {
      LglsAssumeDev(type,
         "Invalid type");
      LglsAssumeDev(size >= type.GetSize(),
         "Pool can't contain a single instance of provided type");
      
      const size_t size_int = static_cast<size_t>(size);
      const size_t pool_cost = Pool::Cost(type, size);
      const size_t pool_alignment = ::std::bit_ceil(pool_cost);
      const size_t backendSize = pool_cost + size_int;
      #if LANGULUS_COMPILER(MSVC) or LANGULUS_COMPILER(CLANG_CL)
         const auto pool = _aligned_malloc(backendSize, pool_alignment);
      #else
         const auto pool = ::std::aligned_alloc(pool_alignment, backendSize);
      #endif

      if (not pool)
         return nullptr;

      new (pool) Pool {type, pot_t(pool_alignment), size};
      return static_cast<Pool*>(pool);
   }

   /// Global allocator interface                                             
   Allocator Instance {};

   /// Allocate a memory entry                                                
   ///   @attention doesn't call any constructors                             
   ///   @attention doesn't throw - check if return is nullptr                
   ///   @attention assumes hint is valid                                     
   ///   @attention assumes size is not zero                                  
   ///   @param hint - meta data to associate pool with                       
   ///   @param size - the number of bytes to allocate                        
   ///   @return the allocation, or nullptr if out of memory                  
   auto Allocator::Allocate(DMeta hint, pot_t size) has_assumptions -> Allocation* {
      LglsAssumeDev(hint, "Invalid hint");

      // Decide pool chain based on hint                                
      Pool* pool = nullptr;
      if (hint) {
         switch (hint.GetPoolTactic()) {
         case PoolTactic::Size:
            pool = Instance.mSizePoolChain[FastLog2(hint.GetSize())];
            break;
         case PoolTactic::Type:
            pool = hint.GetPoolchain();
            break;
         case PoolTactic::Main:
            pool = Instance.mMainPoolChain;
            break;
         }
      }
      else pool = Instance.mMainPoolChain;

      //	Attempt to place allocation in the default chain               
      Allocation* entry = nullptr;
      while (pool) {
         entry = pool->Allocate(size);
         if (entry)
            break;
         pool = pool->mNext;
      }

      if (entry) {
         #if LANGULUS_FEATURE(MEMORY_STATISTICS)
            auto& stats = Instance.mStatistics;
            stats.mEntries += 1;
            stats.mBytesAllocatedByFrontend += static_cast<size_t>(entry->GetSize());
            LglsAssumeDev(
               stats.mBytesAllocatedByFrontend <= stats.mBytesAllocatedByBackend,
               "Impossible amount of frontend allocation"
            );
         #endif

         return entry;
      }

      // If reached, pool chain can't contain the memory.               
      // Allocate a new pool and add it at the front of hinted chain.   
      pool = AllocatePool(hint, size < hint.GetMinPoolsize() ? hint.GetMinPoolsize() : size);
      if (not pool)
         return nullptr;

      LglsVerbose(
         "Fractalloc: ", Logger::Cyan, "New pool ", Logger::Hex(pool),
         " of size ", Logger::Size {static_cast<size_t>(pool->GetAllocatedByBackend())}
      );

      entry = pool->Allocate(size);

      if (hint) {
         switch (hint.GetPoolTactic()) {
         case PoolTactic::Size: {
            auto& sizeChain = Instance.mSizePoolChain[FastLog2(hint.GetSize())];
            pool->mNext = sizeChain;
            sizeChain = pool;
            break;
         }
         case PoolTactic::Type: {
            auto relevantPool = hint.GetPoolchain();
            pool->mNext = relevantPool;
            hint.SetPoolchain(pool);
            Instance.mInstantiatedTypes.insert(hint);
            break;
         }
         case PoolTactic::Main:
            pool->mNext = Instance.mMainPoolChain;
            Instance.mMainPoolChain = pool;
            break;
         }
      }
      else {
         pool->mNext = Instance.mMainPoolChain;
         Instance.mMainPoolChain = pool;
      }

      IF_LANGULUS_MEMORY_STATISTICS(Instance.mStatistics.AddPool(pool));
      return entry;
   }

   /// Reallocate a memory entry                                              
   ///   @attention never calls any constructors                              
   ///   @attention never copies any data                                     
   ///   @attention never deallocates previous entry                          
   ///   @attention returned entry might be different from the previous       
   ///   @attention doesn't throw - check if return is nullptr                
   ///   @param size - the number of bytes to allocate                        
   ///   @param previous - the previous memory entry                          
   ///   @return the reallocated memory entry, or nullptr if out of memory    
   auto Allocator::Reallocate(pot_t size, Allocation* previous) has_assumptions -> Allocation* {
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
            auto& stats = Instance.mStatistics;
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

      // If this is reached, we have a collision, so new entry is made  
      return Allocate(pool->mMeta, size);
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
         auto& stats = Instance.mStatistics;
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
      #if LANGULUS_COMPILER(MSVC) or LANGULUS_COMPILER(CLANG_CL)
         _aligned_free(pool);
      #else
         ::std::free(pool);
      #endif
   }

   /// Deallocates all unused pools in a chain                                
   ///   @param chainStart - [in/out] the start of the chain                  
   Pool* Allocator::CollectGarbageChain(Pool* chainStart) {
      // Delete all unused pools in the beginning                       
      while (chainStart) {
         if (chainStart->IsInUse()) {
            chainStart->Trim();
            break;
         }

         IF_LANGULUS_MEMORY_STATISTICS(mStatistics.DelPool(chainStart));
         auto next = chainStart->mNext;
         LglsVerbose(
            "Fractalloc: ", Logger::DarkCyan, "Pool ", Logger::Hex(chainStart),
            " of size ", Logger::Size {static_cast<size_t>(chainStart->GetAllocatedByBackend())},
            " was deallocated"
         );
         DeallocatePool(chainStart);
         chainStart = next;
      }

      if (not chainStart)
         return nullptr; // All pools in the chain are deleted          

      // Delete all remaining unused pools, chaining the rest together  
      auto prev = chainStart;
      auto pool = chainStart->mNext;
      while (pool) {
         if (pool->IsInUse()) {
            pool->Trim();
            prev = pool;
            pool = pool->mNext;
            continue;
         }

         IF_LANGULUS_MEMORY_STATISTICS(mStatistics.DelPool(pool));
         const auto next = pool->mNext;
         LglsVerbose(
            "Fractalloc: ", Logger::DarkCyan, "Pool ", Logger::Hex(pool),
            " of size ", Logger::Size {static_cast<size_t>(pool->GetAllocatedByBackend())},
            " was deallocated"
         );
         DeallocatePool(pool);
         prev->mNext = next;
         pool = next;
      }
      
      return chainStart;
   }
   
   /// Deallocates all unused pools                                           
   ///   @return true if there's at least one pool remaining allocated        
   bool Allocator::CollectGarbage() {
      bool result = false;
      Instance.mLastFoundPool = nullptr;

      // Cleanup the main chain                                         
      Instance.mMainPoolChain = Instance.CollectGarbageChain(Instance.mMainPoolChain);
      if (Instance.mMainPoolChain)
         result = true;

      // Cleanup all size chains                                        
      for (auto& sizeChain : Instance.mSizePoolChain) {
         sizeChain = Instance.CollectGarbageChain(sizeChain);
         if (sizeChain)
            result = true;
      }

      // Cleanup all type chains                                        
      auto& types = Instance.mInstantiatedTypes;
      for (auto typeChain =  types.begin(); typeChain != types.end();) {
         auto newPoolchain = Instance.CollectGarbageChain(typeChain->GetPoolchain());

         // Also discard the type if no pools remain                    
         if (not newPoolchain) {
            typeChain->SetPoolchain(nullptr);
            typeChain = types.erase(typeChain);
         }
         else {
            typeChain->SetPoolchain(newPoolchain);
            ++typeChain;
            result = true;
         }
      }

      return result;
   }
   
#if LANGULUS_FEATURE(MANAGED_REFLECTION)
   /// Check RTTI boundary for allocated pools                                
   /// Useful to decide when shared library is no longer used and is ready    
   /// to be unloaded. Use it after a call to CollectGarbage                  
   ///   @param boundary - the boundary name                                  
   ///   @return the number of pools                                          
   size_t Allocator::CheckBoundary(const Token& boundary) noexcept {
      size_t count = 0;
      for (const auto& type : Instance.mInstantiatedTypes) {
         if (not type.GetBoundaries().contains(::std::string{boundary}))
            continue;
         
         auto pool = type.GetPoolchain();
         while (pool) {
            ++count;
            pool = pool->mNext;
         }
      }
      return count;
   }
#endif

   /// Search in a pool chain                                                 
   ///   @param memory - memory pointer                                       
   ///   @param pool - start of the pool chain                                
   ///   @return the memory entry that contains the memory pointer, or        
   ///           nullptr if memory is not ours, its entry is no longer used   
   const Allocation* Allocator::FindInChain(const void* memory, const Pool* pool) const has_assumptions {
      while (pool) {
         if (auto found = pool->Find(memory)) {
            mLastFoundPool = pool;
            return found;
         }

         // Continue inside the poolchain                               
         pool = pool->mNext;
      }

      return nullptr;
   }
   
   /// Search if memory is contained inside a pool chain                      
   ///   @param memory - memory pointer                                       
   ///   @param pool - start of the pool chain                                
   ///   @return true if we have authority over the memory                    
   bool Allocator::ContainedInChain(const void* memory, const Pool* pool) const has_assumptions {
      while (pool) {
         if (pool->ContainsData(memory))
            return true;

         // Continue inside the poolchain                               
         pool = pool->mNext;
      }

      return false;
   }

   /// Find a memory entry from pointer                                       
   /// Allows us to safely interface unknown memory, possibly reusing it      
   /// Optimized for consecutive searches in near memory                      
   ///   @param hint - the type of data to search for (optional)              
   ///                 always provide hint for optimal performance            
   ///   @param memory - memory pointer                                       
   ///   @return the memory entry that contains the memory pointer, or        
   ///           nullptr if memory is not ours, its entry is no longer used   
   auto Allocator::Find(DMeta hint, const void* memory) has_assumptions -> const Allocation* {
      // Scan the last pool that found something (hot region)           
      //TODO consider a whole stack of those?
      if (Instance.mLastFoundPool) {
         if (auto found = Instance.mLastFoundPool->Find(memory))
            return found;
      }

      // Decide pool chains, based on hint                              
      const Allocation* result;
      if (hint) {
         switch (hint.GetPoolTactic()) {
         case PoolTactic::Size: {
            // Hint is sized, so check in size pool chain first         
            const auto sizebucket = FastLog2(hint.GetSize());
            result = Instance.FindInChain(memory, Instance.mSizePoolChain[sizebucket]);
            if (result)
               return result;

            // Then check default pool chain                            
            // (pointer could be a member of default-pooled type)       
            result = Instance.FindInChain(memory, Instance.mMainPoolChain);
            if (result)
               return result;

            // Check all typed pool chains                              
            // (pointer could be a member of type-pooled type)          
            for (auto& type : Instance.mInstantiatedTypes) {
               result = Instance.FindInChain(memory, type.GetPoolchain());
               if (result)
                  return result;
            }

            // Finally, check all other size pool chains                
            // (pointer could be a member of differently sized type)    
            for (size_t i = 0; i < sizebucket; ++i) {
               result = Instance.FindInChain(memory, Instance.mSizePoolChain[i]);
               if (result)
                  return result;
            }
            for (size_t i = sizebucket + 1; i < SizeBuckets; ++i) {
               result = Instance.FindInChain(memory, Instance.mSizePoolChain[i]);
               if (result)
                  return result;
            }
         } return nullptr;

         case PoolTactic::Type: {
            // Hint is typed, so check in its typed pool chain first    
            result = Instance.FindInChain(memory, hint.GetPoolchain());
            if (result)
               return result;

            // Then check default pool chain                            
            // (pointer could be a member of default-pooled type)       
            result = Instance.FindInChain(memory, Instance.mMainPoolChain);
            if (result)
               return result;

            // Check all size pool chains                               
            // (pointer could be a member of a size-pooled type)        
            for (auto& sizepool : Instance.mSizePoolChain) {
               result = Instance.FindInChain(memory, sizepool);
               if (result)
                  return result;
            }

            // Finally, check all type pool chains                      
            // (pointer could be a member of a type-pooled type)        
            for (auto& typepool : Instance.mInstantiatedTypes) {
               if (typepool == hint)
                  continue;

               result = Instance.FindInChain(memory, typepool.GetPoolchain());
               if (result)
                  return result;
            }

         } return nullptr;

         case PoolTactic::Main:
            break;
         }
      }

      // If reached, either no hint is provided, or PoolTactic::Main    
      // Check main pool chain                                          
      result = Instance.FindInChain(memory, Instance.mMainPoolChain);
      if (result)
         return result;

      // Check all size pool chains                                     
      // (pointer could be a member of a size-pooled type)              
      for (auto& sizepool : Instance.mSizePoolChain) {
         result = Instance.FindInChain(memory, sizepool);
         if (result)
            return result;
      }

      // Finally, check all type pool chains                            
      // (pointer could be a member of a type-pooled type)              
      for (auto& typepool : Instance.mInstantiatedTypes) {
         result = Instance.FindInChain(memory, typepool.GetPoolchain());
         if (result)
            return result;
      }

      // If reahced, then memory is guaranteed to not be ours           
      return nullptr;
   }

   /// Check if memory is owned by the memory manager                         
   /// Unlike Allocator::Find, this doesn't check if memory is currently used 
   /// but returns true, as long as the required pool is still available      
   ///   @attention assumes memory is a valid pointer                         
   ///   @param hint - the type of data to search for (optional)              
   ///   @param memory - memory pointer                                       
   ///   @return true if we own the memory                                    
   bool Allocator::CheckAuthority(DMeta hint, const void* memory) has_assumptions {
      LglsAssumeDevAndOptimize(memory, "Nullptr provided");

      // Scan the last pool that found something (hot region)           
      //TODO consider a whole stack of those?
      if (Instance.mLastFoundPool) {
         if (auto found = Instance.mLastFoundPool->Find(memory))
            return found;
      }

      // Decide pool chains, based on hint                              
      if (hint) {
         switch (hint.GetPoolTactic()) {
         case PoolTactic::Size: {
            // Hint is sized, so check in size pool chain first         
            const auto sizebucket = FastLog2(hint.GetSize());
            if (Instance.ContainedInChain(memory, Instance.mSizePoolChain[sizebucket]))
               return true;

            // Then check default pool chain                            
            // (pointer could be a member of default-pooled type)       
            if (Instance.ContainedInChain(memory, Instance.mMainPoolChain))
               return true;

            // Check all typed pool chains                              
            // (pointer could be a member of type-pooled type)          
            for (auto& type : Instance.mInstantiatedTypes) {
               if (Instance.ContainedInChain(memory, type.GetPoolchain()))
                  return true;
            }

            // Finally, check all other size pool chains                
            // (pointer could be a member of differently sized type)    
            for (size_t i = 0; i < sizebucket; ++i) {
               if (Instance.ContainedInChain(memory, Instance.mSizePoolChain[i]))
                  return true;
            }
            for (size_t i = sizebucket + 1; i < SizeBuckets; ++i) {
               if (Instance.ContainedInChain(memory, Instance.mSizePoolChain[i]))
                  return true;
            }
         } return false;

         case PoolTactic::Type:
            // Hint is typed, so check in its typed pool chain first    
            if (Instance.ContainedInChain(memory, hint.GetPoolchain()))
               return true;

            // Then check default pool chain                            
            // (pointer could be a member of default-pooled type)       
            if (Instance.ContainedInChain(memory, Instance.mMainPoolChain))
               return true;

            // Check all size pool chains                               
            // (pointer could be a member of a size-pooled type)        
            for (auto& sizepool : Instance.mSizePoolChain) {
               if (Instance.ContainedInChain(memory, sizepool))
                  return true;
            }

            // Finally, check all type pool chains                      
            // (pointer could be a member of a type-pooled type)        
            for (auto& typepool : Instance.mInstantiatedTypes) {
               if (typepool == hint)
                  continue;

               if (Instance.ContainedInChain(memory, typepool.GetPoolchain()))
                  return true;
            }
            return false;

         case PoolTactic::Main:
            break;
         }
      }

      // If reached, either no hint is provided, or PoolTactic::Main    
      // Check main pool chain                                          
      if (Instance.ContainedInChain(memory, Instance.mMainPoolChain))
         return true;

      // Check all size pool chains                                     
      // (pointer could be a member of a size-pooled type)              
      for (auto& sizepool : Instance.mSizePoolChain) {
         if (Instance.ContainedInChain(memory, sizepool))
            return true;
      }

      // Finally, check all type pool chains                            
      // (pointer could be a member of a type-pooled type)              
      for (auto& typepool : Instance.mInstantiatedTypes) {
         if (Instance.ContainedInChain(memory, typepool.GetPoolchain()))
            return true;
      }

      return false;
   }
   
#if LANGULUS_FEATURE(MEMORY_STATISTICS)
   bool Allocator::Statistics::operator == (const Statistics& rhs) const has_assumptions {
      LglsAssumeDevAndOptimize(
         mBytesAllocatedByFrontend <= mBytesAllocatedByBackend,
         "Impossible amount of frontend allocation"
      );

      return mBytesAllocatedByBackend == rhs.mBytesAllocatedByBackend
         and mBytesAllocatedByFrontend == rhs.mBytesAllocatedByFrontend
         and mEntries == rhs.mEntries
         and mPools == rhs.mPools
      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
         and mDataDefinitions == rhs.mDataDefinitions
         and mTraitDefinitions == rhs.mTraitDefinitions
         and mVerbDefinitions == rhs.mVerbDefinitions
      #endif
      ;
   }

   /// Check for memory leaks, by retrieving the new memory manager state     
   /// and comparing it against this one                                      
   ///   @return true if no functional difference between the states          
   bool Allocator::State::Assert() {
      CollectGarbage();

      if (not IntegrityCheck()) {
         Logger::Error("Memory integrity check failure");
         return false;
      }

      if (mState.has_value()) {
         if (mState != GetStatistics()) {
            // Assertion failure                                        
            DumpPools();
            Diff(mState.value());
            mState = GetStatistics();
            ++Instance.mStatistics.mStep;
            Logger::Error("Memory state mismatch");
            return false;
         }
      }

      // All is fine                                                    
      mState = GetStatistics();
      ++Instance.mStatistics.mStep;
      return true;
   }
   
   /// Get allocator statistics                                               
   ///   @return a reference to the statistics structure                      
   auto Allocator::GetStatistics() noexcept -> const Statistics& {
      return Instance.mStatistics;
   }

   /// Dump a single pool                                                     
   ///   @param id - pool id                                                  
   ///   @param pool - the pool to dump                                       
   void Allocator::DumpPool(size_t id, const Pool* pool) noexcept {
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

      if (pool->mMeta) {
         Logger::Line("Associated type: `",
            pool->mMeta.GetCppName(), "`, of size ", pool->mMeta.GetSize());
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
      if (Instance.mMainPoolChain) {
         const auto scope = Logger::InfoScoped(Logger::Purple, "MAIN POOL CHAIN: ");
         size_t counter = 0;
         auto pool = Instance.mMainPoolChain;
         while (pool) {
            DumpPool(counter, pool);
            pool = pool->mNext;
            ++counter;
         }
      }

      // Dump every size pool chain                                     
      for (size_t size = 0; size < sizeof(size_t) * 8; ++size) {
         if (not Instance.mSizePoolChain[size])
            continue;

         const auto scope = Logger::InfoScoped(Logger::Purple, 
            "SIZE POOL CHAIN FOR ", Logger::Red, Logger::Size {1ul << size},
            Logger::Purple, ": "
         );

         size_t counter = 0;
         auto pool = Instance.mSizePoolChain[size];
         while (pool) {
            DumpPool(counter, pool);
            pool = pool->mNext;
            ++counter;
         }
      }
      
      // Dump every type pool chain                                     
      for (auto& type : Instance.mInstantiatedTypes) {
         auto pool = type.GetPoolchain();
         if (not pool)
            continue;

         #if LANGULUS_FEATURE(MANAGED_REFLECTION)
            const auto scope = Logger::InfoScoped(Logger::Purple, 
               "TYPE POOL CHAIN FOR `", Logger::Red, type.GetCppName(), 
               Logger::Purple, "` (boundaries: "
            );
         
            if (type.GetBoundaries().empty())
               Logger::Append("MAIN");
            else for (auto& boundary : type.GetBoundaries())
               Logger::Append(boundary, ' ');
         
            Logger::Append(Logger::Purple, "): ");
         #else
            const auto scope = Logger::InfoScoped(Logger::Purple, 
               "TYPE POOL CHAIN FOR `", Logger::Red, type.GetCppName(), 
               Logger::Purple, '`'
            );
         #endif

         size_t counter = 0;
         while (pool) {
            DumpPool(counter, pool);
            pool = pool->mNext;
            ++counter;
         }
      }
   }

   /// Compare two statistics snapshots, and find the difference              
   void Allocator::Diff(const Statistics& with) noexcept {
      auto section = Logger::InfoScoped("MANAGED MEMORY DIFF");
      auto& stats = Instance.mStatistics;

      if (stats.mBytesAllocatedByBackend != with.mBytesAllocatedByBackend) {
         Logger::Info(Logger::Purple,
            "Allocated byte difference: ",
            int(stats.mBytesAllocatedByBackend) - int(with.mBytesAllocatedByBackend));
      }

      if (stats.mBytesAllocatedByFrontend != with.mBytesAllocatedByFrontend) {
         Logger::Info(Logger::Purple,
            "Used byte difference: ",
            int(stats.mBytesAllocatedByFrontend) - int(with.mBytesAllocatedByFrontend));
      }

   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      if (stats.mDataDefinitions != with.mDataDefinitions) {
         const auto scope = Logger::InfoScoped(Logger::Purple,
            "Data definitions difference: ",
            int(stats.mDataDefinitions) - int(with.mDataDefinitions)
         );
      }
   #endif

      if (stats.mPools != with.mPools) {
         const auto scope = Logger::InfoScoped(Logger::Purple,
            "Pool difference: ", int(stats.mPools) - int(with.mPools)
         );

         // Diff default pool chain                                     
         if (Instance.mMainPoolChain) {
            size_t counter = 0;
            auto pool = Instance.mMainPoolChain;
            while (pool) {
               if (pool->mStep > with.mStep) {
                  Logger::Info(Logger::Purple, "Default pool: ");
                  DumpPool(counter, pool);
               }
               pool = pool->mNext;
               ++counter;
            }
         }

         // Dump every size pool chain                                  
         for (size_t size = 0; size < sizeof(size_t) * 8; ++size) {
            if (not Instance.mSizePoolChain[size])
               continue;

            size_t counter = 0;
            auto pool = Instance.mSizePoolChain[size];
            while (pool) {
               if (pool->mStep > with.mStep) {
                  Logger::Info(Logger::Purple, "Size ", Logger::Size {1ul << size}, " pool: ");
                  DumpPool(counter, pool);
               }
               pool = pool->mNext;
               ++counter;
            }
         }

         // Dump every type pool chain                                  
         for (auto& type : Instance.mInstantiatedTypes) {
            auto pool = type.GetPoolchain();
            if (not pool)
               continue;

            size_t counter = 0;
            while (pool) {
               if (pool->mStep > with.mStep) {
                  Logger::Info(Logger::Purple, "Type ", type.GetCppName(), " pool: ");
                  #if LANGULUS_FEATURE(MANAGED_REFLECTION)
                     Logger::Info(Logger::Purple, "(boundaries: ");
                     if (type.GetBoundaries().empty())
                        Logger::Append("MAIN");
                     else for (auto& boundary : type.GetBoundaries())
                        Logger::Append(boundary, ' ');
                     Logger::Append(')');
                  #endif
                  DumpPool(counter, pool);
               }
               pool = pool->mNext;
               ++counter;
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

   /// Account for a newly allocated pool                                     
   ///   @param pool - the pool to account for                                
   void Allocator::Statistics::AddPool(const Pool* pool) IF_UNSAFE(noexcept) {
      mBytesAllocatedByBackend  += pool->GetTotalSize();
      mBytesAllocatedByFrontend += pool->GetAllocatedByFrontend();
      LglsAssumeDevAndOptimize(
         mBytesAllocatedByFrontend <= mBytesAllocatedByBackend,
         "Impossible amount of frontend allocation"
      );
      ++mPools;
      ++mEntries;
   }
   
   /// Account for a removed pool                                             
   ///   @param pool - the pool to account for                                
   void Allocator::Statistics::DelPool(const Pool* pool) IF_UNSAFE(noexcept) {
      LglsAssumeDev(
         mBytesAllocatedByBackend >= pool->GetTotalSize(),
         "Impossible amount of backend allocation"
      );
      mBytesAllocatedByBackend -= pool->GetTotalSize();
      --mPools;
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
      if (Instance.mMainPoolChain) {
         LglsVerbose("Integrity check: mMainPoolChain...");
         if (not Instance.IntegrityCheckChain(Instance.mMainPoolChain))
            return false;
      }

      // Integrity check all size chains                                
      [[maybe_unused]] int size = 1;
      for (auto& sizeChain : Instance.mSizePoolChain) {
         if (sizeChain) {
            LglsVerbose("Integrity check: mSizePoolChain #", size++, "...");
            if (not Instance.IntegrityCheckChain(sizeChain))
               return false;
         }
      }
      
      // Integrity check all type chains                                
      for (auto& typeChain : Instance.mInstantiatedTypes) {
         if (auto relevantPool = typeChain.GetPoolchain()) {
            LglsVerbose("Integrity check for type ", typeChain.GetName(), "...");
            if (not Instance.IntegrityCheckChain(relevantPool))
               return false;
         }
      }

      return true;
   }
#endif
}
