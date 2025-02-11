///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include <RTTI/Meta.hpp>


namespace Langulus::Anyness
{

   using RTTI::AllocationRequest;
   using RTTI::DMeta;
   using RTTI::CMeta;
   using RTTI::TMeta;

   using Pool = void;
   
   template<class T>
   concept AllocationPrimitive = requires(T a) { 
      {T::GetNewAllocationSize(0)} -> CT::Unsigned;
   };


   ///                                                                        
   ///   Memory allocation                                                    
   ///                                                                        
   /// This is a single allocation record                                     
   ///                                                                        
   struct Allocation final {
   #if LANGULUS_FEATURE(MANAGED_MEMORY)
      friend class Pool;
   #endif
   friend struct Allocator;
   protected:
      // Allocated bytes for this chunk                                 
      Offset mAllocatedBytes;
      // The number of references to this memory                        
      Count mReferences;
      union {
         // This pointer has two uses, depending on mReferences         
         // If mReferences > 0, it refers to the pool that owns the     
         //    allocation, or	handle for std::free() if MANAGED_MEMORY  
         //    feature is not enabled                                   
         // If mReferences == 0, it refers to the next free entry to be 
         //    reused                                                   
         Pool* mPool;
         Allocation* mNextFreeEntry;
      };

      // Acts like a timestamp of when the allocation happened          
      #if LANGULUS_FEATURE(MEMORY_STATISTICS)
         Count mStep;
      #endif

   public:
      Allocation() = delete;
      Allocation(const Allocation&) = delete;
      Allocation(Allocation&&) = delete;
      ~Allocation() = delete;

      constexpr Allocation(Offset, Pool*) noexcept;

      NOD() static constexpr Offset GetSize() noexcept;
      NOD() static constexpr Offset GetNewAllocationSize(Offset) noexcept;
      NOD() static constexpr Offset GetMinAllocation() noexcept;

      NOD() constexpr Count GetUses() const noexcept;
      NOD() Byte* GetBlockStart() const noexcept;
      NOD() Byte const* GetBlockEnd() const noexcept;
      NOD() constexpr Offset GetTotalSize() const noexcept;
      NOD() constexpr Offset GetAllocatedSize() const noexcept;
      NOD() bool Contains(const void*) const noexcept;
      NOD() bool CollisionFree(const Allocation&) const noexcept;

      template<class T>
      NOD() T* As() const noexcept;

      constexpr void Keep() noexcept;
      constexpr void Keep(Count) noexcept;
      constexpr void Free() noexcept;
      constexpr void Free(Count) noexcept;
   };

} // namespace Langulus::Anyness

#include "Allocation.inl"