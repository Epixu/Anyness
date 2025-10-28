///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Allocation.hpp"

#if LANGULUS_FEATURE(MANAGED_MEMORY)
   #error "This file shouldn't be included if MANAGED_MEMORY is enabled"
#endif

namespace Langulus::Unmanaged
{
   /// Initialize an allocation                                               
   ///   @attention this constructor relies that the allocation is placed in  
   ///      the beginning of a heap allocation of size GetNewAllocationSize() 
   ///   @param alignment - data alignment                                    
   ///   @param size - the number of allocated bytes                          
   ///   @param handle - the handle used to call free() with                  
   LANGULUS(ALWAYS_INLINED)
   Allocation::Allocation(size_t alignment, size_t size, MallocHandle* handle) noexcept
      : mAllocatedBytes {size}
      , mAlignment      {alignment}
      , mMallocHandle   {handle} {}

   /// User bytes + the header size                                           
   ///   @return the byte size of the entry plus the usable region after it   
   LANGULUS(ALWAYS_INLINED)
   size_t Allocation::GetBackendSize() const noexcept {
      return Align(sizeof(Allocation), mAlignment) + mAllocatedBytes;
   }

   /// Get the user bytes                                                     
   ///   @return the byte size of usable memory region                        
   LANGULUS(ALWAYS_INLINED)
   size_t Allocation::GetFrontendSize() const noexcept {
      return mAllocatedBytes;
   }

   /// Return the aligned start of usable block memory (const)                
   ///   @return aligned pointer to the entry's memory                        
   LANGULUS(ALWAYS_INLINED)
   uint8_t* Allocation::GetBlockStart() const noexcept {
      const auto entryStart = reinterpret_cast<const uint8_t*>(this);
      return const_cast<uint8_t*>(entryStart) + Align(sizeof(Allocation), mAlignment);
   }

   /// Return the end of usable block memory (always const)                   
   ///   @return aligned pointer to the entry's memory end                    
   LANGULUS(ALWAYS_INLINED)
   uint8_t const* Allocation::GetBlockEnd() const noexcept {
      return GetBlockStart() + mAllocatedBytes;
   }
   
   /// Check if memory address is inside this entry                           
   ///   @param address - address to check if inside this entry               
   ///   @return true if address is inside                                    
   LANGULUS(ALWAYS_INLINED)
   bool Allocation::Contains(const void* address) const noexcept {
      const auto a = static_cast<const uint8_t*>(address);
      const auto blockStart = GetBlockStart();
      return a >= blockStart and a < blockStart + mAllocatedBytes;
   }

   /// Reference the entry 'c' times                                          
   ///   @param c - the number of references to add                           
   LANGULUS(ALWAYS_INLINED)
   void Allocation::Keep(int c) noexcept {
      mReferences += c;
   }

   /// Dereference the entry 'c' times                                        
   ///   @param c - the number of references to remove                        
   LANGULUS(ALWAYS_INLINED)
   void Allocation::Free(int c) noexcept {
      mReferences -= c;
   }
}
