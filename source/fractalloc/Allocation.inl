///                                                                           
/// Langulus::Fractalloc                                                      
/// Copyright (c) 2015 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Allocation.hpp"


namespace Langulus::Fractalloc
{

#if LANGULUS_FEATURE(MANAGED_MEMORY)
   /// Initialize an allocation                                               
   ///   @attention this constructor relies that the allocation is placed in  
   ///      the beginning of a heap allocation of size GetNewAllocationSize() 
   ///   @param bytes - the number of allocated bytes                         
   ///   @param pool - the pool this allocation belongs to                    
   LANGULUS(ALWAYS_INLINED)
   Allocation::Allocation(size_t bytes, Pool* pool) noexcept
      : mAllocatedBytes {bytes}
      , mPool           {pool} {}
#else
   /// Initialize an allocation                                               
   ///   @attention this constructor relies that the allocation is placed in  
   ///      the beginning of a heap allocation of size GetNewAllocationSize() 
   ///   @param bytes - the number of allocated bytes                         
   ///   @param handle - the handle used to call free() with                  
   LANGULUS(ALWAYS_INLINED)
   Allocation::Allocation(Size bytes, MallocHandle* handle) noexcept
      : mAllocatedBytes {bytes}
      , mMallocHandle   {handle} {}
#endif

   /// Get the minimum possible allocation, header included                   
   ///   @return the byte size                                                
   consteval size_t Allocation::GetMinAllocation() noexcept {
      return sizeof(Allocation) + Alignment;
   }

   /// Get the size required for a new entry                                  
   /// The layout is: [sizeof(Allocation)][size]                     
   ///   @param size - the usable number of bytes required                    
   ///   @return the byte size for a new Allocation, including padding        
   LANGULUS(ALWAYS_INLINED)
   size_t Allocation::GetNewAllocationSize(size_t size) noexcept {
      constexpr auto minimum = GetMinAllocation();
      const auto proposed = sizeof(Allocation) + size;
      return proposed > minimum ? proposed : minimum;
   }

   /// User bytes + the header size                                           
   ///   @return the byte size of the entry plus the usable region after it   
   LANGULUS(ALWAYS_INLINED)
   size_t Allocation::GetBackendSize() const noexcept {
      return sizeof(Allocation) + mAllocatedBytes;
   }

   /// Get the user bytes                                                     
   ///   @return the byte size of usable memory region                        
   LANGULUS(ALWAYS_INLINED)
   size_t Allocation::GetFrontendSize() const noexcept {
      return mAllocatedBytes;
   }

   /// Return the aligned start of usable block memory (const)                
   ///   @return pointer to the entry's memory                                
   LANGULUS(ALWAYS_INLINED)
   uint8_t* Allocation::GetBlockStart() const noexcept {
      const auto entryStart = reinterpret_cast<const uint8_t*>(this);
      return const_cast<uint8_t*>(entryStart + sizeof(Allocation));
   }

   /// Return the end of usable block memory (always const)                   
   ///   @return pointer to the entry's memory end                            
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

} // namespace Langulus::Fractalloc
