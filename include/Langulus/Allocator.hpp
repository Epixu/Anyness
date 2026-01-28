///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Core.hpp"

#if LANGULUS_FEATURE(MANAGED_MEMORY)
   #include "../source/fractalloc/Allocator.hpp"
   #include "../source/fractalloc/PackedPointer.hpp"
#else
   #include "../source/unmanaged/Allocator.hpp"
#endif


namespace Langulus
{
   #if LANGULUS_FEATURE(MANAGED_MEMORY)
      using Fractalloc::Allocation;
      using Fractalloc::Allocator;
      using MemoryState = Fractalloc::State;
      
      template<class T>
      using PackedPtr = Fractalloc::PackedPointer<T>;

      /// Unpack a pointer from heap                                          
      ///   @param T type of the pointer the heap points to                   
      ///   @attention assumes T is sparse                                    
      ///   @param nextT type of data T points to after being dereferenced    
      ///   @param ptrToPackedPtr raw pointer to a pointer of type T          
      ///   @attention assumes ptrToPackedPtr is valid                        
      ///   @return the unpacked raw pointer, pointing to an instance of nextT
      LANGULUS(INLINED)
      void const* UnpackPointer(RTTI::DMeta const& T, RTTI::DMeta const& nextT, void const* ptrToPackedPtr) assumptious {
         LglsAssumeDev(T.IsSparse(), "T must be sparse");
         LglsAssumeDevAndOptimize(ptrToPackedPtr, "Invalid ptrToPackedPtr");
         const auto ptrSpec = T.GetPointerSpecification();
         if (ptrSpec.IsPacked()) {
            uintptr_t derefSrc = 0;
            memcpy(&derefSrc, ptrToPackedPtr, ptrSpec.GetTotalBytes());
            return Allocator::UnpackPointer(ptrSpec, nextT, derefSrc);
         }
         return *static_cast<void const* const*>(ptrToPackedPtr);
      }
   #else
      using Unmanaged::Allocation;
      using Unmanaged::Allocator;
      
      struct MemoryState {
         consteval bool Assert() const noexcept { return true; }
      };

      /// Packed pointers are disabled when managed memory is disabled.       
      /// This just dereferences the heap pointer.                            
      LANGULUS(INLINED)
      void const* UnpackPointer(RTTI::DMeta const&, RTTI::DMeta const&, void const* ptr) assumptious {
         LglsAssumeDevAndOptimize(ptr, "Invalid pointer");
         return *static_cast<void const* const*>(ptr);
      }
   #endif
   
   /// Can be a packed pointer                                                
   using AllocationPtr = Allocation const*;

   /// Can be a packed pointer                                                
   using EntryPtr = AllocationPtr const*;
}
