///                                                                           
/// Langulus::Anyness                                                         
/// Copyright(C) 2012 Dimo Markov <langulusteam@gmail.com>                    
///                                                                           
/// Distributed under GNU General Public License v3+                          
/// See LICENSE file, or https://www.gnu.org/licenses                         
///                                                                           
#pragma once
#include "Block.hpp"

namespace Langulus::Anyness
{

   /// Get a size based on reflected allocation page and count                
   ///   @param count - the number of elements to request                     
   ///   @return both the provided byte size and reserved count               
   LANGULUS(ALWAYSINLINE)
   RTTI::AllocationRequest Block::RequestSize(const Count& count) const SAFETY_NOEXCEPT() {
      LANGULUS_ASSUME(DevAssumes, IsTyped(),
         "Requesting allocation size for an untyped container");
      return mType->RequestSize(count);
   }

   /// Reserve a number of elements without initializing them                 
   /// If reserved data is smaller than currently initialized count, the      
   /// excess elements will be destroyed                                      
   ///   @param count - number of elements to reserve                         
   LANGULUS(ALWAYSINLINE)
   void Block::Reserve(Count count) {
      if (count < mCount)
         AllocateLess(count);
      else 
         AllocateMore(count);
   }
   
   /// Allocate a number of elements, relying on the type of the container    
   ///   @attention assumes a valid and non-abstract type, if dense           
   ///   @tparam CREATE - true to call constructors and set count             
   ///   @tparam SETSIZE - true to set count, despite not constructing        
   ///   @param elements - number of elements to allocate                     
   template<bool CREATE, bool SETSIZE>
   void Block::AllocateMore(Count elements) {
      LANGULUS_ASSUME(DevAssumes, mType,
         "Invalid type");
      LANGULUS_ASSERT(!mType->mIsAbstract || mType->mIsSparse, Allocate,
         "Abstract dense type");

      if (mReserved >= elements) {
         // Required memory is already available                        
         if constexpr (CREATE) {
            // But is not yet initialized, so initialize it             
            if (mCount < elements) {
               const auto count = elements - mCount;
               CropInner(mCount, count)
                  .CallUnknownDefaultConstructors(count);
            }
         }
      }
      else AllocateInner<CREATE>(elements);

      if constexpr (CREATE || SETSIZE)
         mCount = elements;
   }

   /// Shrink the block, depending on currently reserved	elements             
   /// Initialized elements on the back will be destroyed                     
   ///   @attention assumes 'elements' is smaller than the current reserve    
   ///   @param elements - number of elements to allocate                     
   LANGULUS(ALWAYSINLINE)
   void Block::AllocateLess(Count elements) {
      LANGULUS_ASSUME(DevAssumes, elements < mReserved,
         "Bad element count");
      LANGULUS_ASSUME(DevAssumes, mType,
         "Invalid type");

      if (mCount > elements) {
         // Destroy back entries on smaller allocation                  
         // Allowed even when container is static and out of            
         // jurisdiction, as in that case this acts as a simple count   
         // decrease, and no destructors shall be called                
         Trim(elements);
      }

      // Shrink the memory block                                        
      #if LANGULUS_FEATURE(MANAGED_MEMORY)
         const auto request = RequestSize(elements);
         if (mType->mIsSparse) {
            // Move entry data to its new place                         
            ::std::memmove(
               mRawSparse + request.mElementCount,
               GetEntries(),
               sizeof(Pointer) * mCount
            );
         }
         mEntry = Inner::Allocator::Reallocate(request.mByteSize, mEntry);
         mReserved = request.mElementCount;
      #endif
   }

   /// Clone all elements inside this memory block, preserving hierarchy and  
   /// density, but removing size constraints and constness                   
   /// If we already have jurisdiction, then nothing happens                  
   LANGULUS(ALWAYSINLINE)
   void Block::TakeAuthority() {
      if (mEntry || !mRaw) {
         // We already own this memory, or there's nothing to own       
         return;
      }

      // Clone everything and overwrite this block                      
      Block clone {*this};
      clone.AllocateFresh(RequestSize(mCount));
      clone.CallUnknownSemanticConstructors(mCount, Langulus::Clone(*this));
      Free();
      ::std::memcpy(this, &clone, sizeof(Block));
   }

   /// Allocate a number of elements, relying on the type of the container    
   ///   @attention assumes a valid and non-abstract type, if dense           
   ///   @tparam CREATE - true to call constructors and set count             
   ///   @param elements - number of elements to allocate                     
   template<bool CREATE>
   void Block::AllocateInner(const Count& elements) {
      LANGULUS_ASSERT(mType, Allocate,
         "Invalid type");
      LANGULUS_ASSERT(!mType->mIsAbstract || IsSparse(), Allocate,
         "Abstract dense type");

      // Retrieve the required byte size                                
      const auto request = RequestSize(elements);
      
      // Allocate/reallocate                                            
      if (mEntry) {
         // Reallocate                                                  
         Block previousBlock {*this};
         if (mEntry->GetUses() == 1) {
            // Memory is used only once and it is safe to move it       
            // Make note, that Allocator::Reallocate doesn't copy       
            // anything, it doesn't use realloc for various reasons, so 
            // we still have to call move construction for all elements 
            // if entry moved (enabling MANAGED_MEMORY feature          
            // significantly reduces the possiblity for a move)         
            // Also, make sure to free the previous mEntry if moved     
            #if LANGULUS_FEATURE(MANAGED_MEMORY)
               // Sparse containers have additional memory allocated    
               // for each pointer's entry                              
               mEntry = Inner::Allocator::Reallocate(
                  request.mByteSize * (mType->mIsSparse ? 2 : 1), 
                  mEntry
               );
            #else
               mEntry = Inner::Allocator::Reallocate(request.mByteSize, mEntry);
            #endif
            LANGULUS_ASSERT(mEntry, Allocate, "Out of memory");
            mReserved = request.mElementCount;

            if (mEntry != previousBlock.mEntry) {
               // Memory moved, and we should call abandon-construction 
               // We're moving to a new allocation, so no reverse needed
               mRaw = mEntry->GetBlockStart();
               CallUnknownSemanticConstructors(previousBlock.mCount, 
                  Abandon(previousBlock));
            }
            else {
               // Memory didn't move, but reserved count changed        
               if (mType->mIsSparse) {
                  // Move entry data to its new place                   
                  ::std::memmove(
                     GetEntries(), previousBlock.GetEntries(),
                     sizeof(Pointer) * mCount
                  );
               }
            }
         }
         else {
            // Memory is used from multiple locations, and we must      
            // copy the memory for this block - we can't move it!       
            AllocateFresh(request);
            CallUnknownSemanticConstructors(previousBlock.mCount, 
               Langulus::Copy(previousBlock));
            previousBlock.Free();
         }
      }
      else AllocateFresh(request);

      if constexpr (CREATE) {
         // Default-construct the rest                                  
         const auto count = elements - mCount;
         CropInner(mCount, count)
            .CallUnknownDefaultConstructors(count);
         mCount = elements;
      }
   }

   /// Allocate a fresh allocation (inner function)                           
   ///   @attention changes entry, memory and reserve count                   
   ///   @param request - request to fulfill                                  
   LANGULUS(ALWAYSINLINE)
   void Block::AllocateFresh(const RTTI::AllocationRequest& request) {
      #if LANGULUS_FEATURE(MANAGED_MEMORY)
         // Sparse containers have additional memory allocated          
         // for each pointer's entry                                    
         mEntry = Inner::Allocator::Allocate(
            request.mByteSize * (mType->mIsSparse ? 2 : 1)
         );
      #else
         mEntry = Inner::Allocator::Allocate(request.mByteSize);
      #endif
      LANGULUS_ASSERT(mEntry, Allocate, "Out of memory");
      mRaw = mEntry->GetBlockStart();
      mReserved = request.mElementCount;
   }
   
   /// Reference memory block if we own it                                    
   ///   @param times - number of references to add                           
   LANGULUS(ALWAYSINLINE)
   void Block::Reference(const Count& times) const noexcept {
      if (mEntry)
         mEntry->Keep(times);
   }
   
   /// Reference memory block once                                            
   LANGULUS(ALWAYSINLINE)
   void Block::Keep() const noexcept {
      Reference(1);
   }
         
   /// Dereference memory block                                               
   ///   @attention doesn't affect count, state, and reserve                  
   ///   @tparam DESTROY - whether to call destructors on full dereference    
   ///   @param times - number of references to subtract                      
   template<bool DESTROY>
   void Block::Dereference(const Count& times) {
      if (!mEntry)
         return;

      LANGULUS_ASSUME(DevAssumes, 
         mEntry->GetUses() >= times, "Bad memory dereferencing");

      if (mEntry->GetUses() == times) {
         // Destroy all elements and deallocate the entry               
         if constexpr (DESTROY)
            CallUnknownDestructors();
         Inner::Allocator::Deallocate(mEntry);
         mEntry = nullptr;
         return;
      }

      mEntry->Free(times);
      mEntry = nullptr;
      return;
   }

   /// Dereference memory block once and destroy all elements if data was     
   /// fully dereferenced                                                     
   LANGULUS(ALWAYSINLINE)
   void Block::Free() {
      return Dereference<true>(1);
   }
   
   /// Sets the currently interfaced memory                                   
   ///   @attention for internal use only, use only if you know what you're   
   ///              doing!                                                    
   LANGULUS(ALWAYSINLINE)
   void Block::SetMemory(const DataState& state
      , DMeta meta
      , Count count
      , const void* raw
   ) SAFETY_NOEXCEPT() {
      SetMemory(
         state + DataState::Constant, meta, count, 
         const_cast<void*>(raw)
      );
   }

   /// Sets the currently interfaced memory                                   
   ///   @attention for internal use only, use only if you know what you're   
   ///              doing!                                                    
   LANGULUS(ALWAYSINLINE)
   void Block::SetMemory(const DataState& state
      , DMeta meta
      , Count count
      , void* raw
   ) SAFETY_NOEXCEPT() {
      SetMemory(
         state, meta, count, 
         const_cast<void*>(raw),
         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            Inner::Allocator::Find(meta, raw)
         #else
            nullptr
         #endif
      );
   }

   /// Sets the currently interfaced memory                                   
   ///   @attention for internal used only, use only if you know what you're  
   ///              doing!                                                    
   LANGULUS(ALWAYSINLINE)
   SAFETY_CONSTEXPR()
   void Block::SetMemory(const DataState& state
      , DMeta meta
      , Count count
      , const void* raw
      , Inner::Allocation* entry
   ) {
      SetMemory(
         state + DataState::Constant, meta, count, 
         const_cast<void*>(raw), entry
      );
   }

   /// Sets the currently interfaces memory                                   
   ///   @attention for internal used only, use only if you know what you're  
   ///              doing!                                                    
   LANGULUS(ALWAYSINLINE)
   SAFETY_CONSTEXPR()
   void Block::SetMemory(const DataState& state
      , DMeta meta
      , Count count
      , void* raw
      , Inner::Allocation* entry
   ) {
      LANGULUS_ASSUME(DevAssumes, raw != nullptr,
         "Invalid data pointer");
      LANGULUS_ASSUME(DevAssumes, meta != nullptr,
         "Invalid data type");
      LANGULUS_ASSUME(DevAssumes, !meta->mIsSparse,
         "Sparse raw data initialization is not allowed");

      mRaw = static_cast<Byte*>(raw);
      mState = state;
      mCount = count;
      mReserved = count;
      mType = meta;
      mEntry = entry;
   }

} // namespace Langulus::Anyness