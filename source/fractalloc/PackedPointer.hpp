///                                                                           
/// Langulus::Fractalloc                                                      
/// Copyright (c) 2015 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include <Langulus/Core.hpp>

#if not LANGULUS_FEATURE(MANAGED_MEMORY) or not LANGULUS_FEATURE(MANAGED_REFLECTION)
   #error "This file shouldn't be included if MANAGED_MEMORY or MANAGED_REFLECTION are disabled"
#endif


namespace Langulus::Fractalloc
{
   ///                                                                        
   ///   A flexible packed pointer                                            
   ///                                                                        
   /// You can tune the bits used to represent pool ID, entry ID and element  
   /// offset. The IDs corresponding to these values are unique for each T,   
   /// so there's no way one type of packed pointer steal budget from the     
   /// others. The allocator will attempt to satisfy your constraints at all  
   /// cost, until unable to do so.                                           
   /// Packing pointers is most useful on 64bit builds, and is by default     
   /// reduced to 32bits, however it can also be used as a security measure   
   /// to obfuscate pointers. You can create custom pointers that are even    
   /// smaller.                                                               
   ///   @tparam T the type behind the pointer                                
   ///   @tparam POOL_BITS if you have 8bit pool id, this means that you      
   ///      can access at most 255 pools. If the first 255 pools have already 
   ///      been filled, the allocator will simply deny your request. If not, 
   ///      it will use only the first 255 slots for your allocation request. 
   ///   @tparam ENTRY_BITS if you have 8bit entry representation, then you   
   ///      will support only up to 256 entries. This means that the memory   
   ///      manager will search in the allowed pools, if it can allocate one  
   ///      of the first 256 entries in order to satisfy your request. If it  
   ///      is unable to do so, your request will be denied.                  
   ///   @tparam OFFSET_BITS the offset bits show how many elements you can   
   ///      move from the start of the entry to the right. The byte offset is 
   ///      calculated as `sizeof(T) * offset`.                               
   template<class T, unsigned POOL_BITS = 8, unsigned ENTRY_BITS = 16, unsigned OFFSET_BITS = 8>
   struct PackedPointer {
      using CTTI_Sparse = Yes<>;
      
      static constexpr unsigned PoolBits   = POOL_BITS;
      static constexpr unsigned EntryBits  = ENTRY_BITS;
      static constexpr unsigned OffsetBits = OFFSET_BITS;
      static constexpr unsigned TotalBits  = PoolBits + EntryBits + OffsetBits;

   private:
      union {
         struct {
            unsigned mPool : PoolBits;
            unsigned mEntry : EntryBits;
            unsigned mOffset : OffsetBits;
         };
         unsigned mAll : TotalBits;
      };

   public:
      constexpr PackedPointer() noexcept
         : mAll(0) {}
      
      constexpr PackedPointer(Allocation* entry) noexcept {
         if (not entry) {
            mAll = 0;
            return;
         }

         mPool = entry->GetPool()->GetIndex();
         mEntry = entry->GetIndex();
         mOffset = 0;
      }
   };
}
