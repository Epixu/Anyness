///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
#include <Langulus/TypeOf.hpp>


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Interfaces a heap allocation                                           
   /// Adds a pointer member to the raw byte memory                           
   /// The pointer is not allowed to move on reallocation, and instead        
   /// multiple allocations are chained together                              
   ///   @tparam ID - multiple heap interfaces are supported                  
   ///                                                                        
   template<unsigned ID = 0>
   struct HeapImmovable {
      using CTTI_Component = Yes<>;
      static constexpr bool HeapAllocated = true;
      static constexpr bool HeapCanBeNull = true;

   protected:
      using Byte = ::std::uint8_t;

      // A heap of heaps - the inner ones are immovable                 
      Byte** mHeap = nullptr;
      // Number of allocated heaps - each new heap is twice as big      
      ::std::uint8_t mHeapCount = 0;

      // The start of the reusable chain, in the first heap that has    
      // a free cell                                                    
      Byte* mReusable = nullptr;

   public:
   #if LANGULUS(TESTING)
      auto GetReusable()      const noexcept { return mReusable;  }
      auto GetFrames()        const noexcept { return mHeapCount; }
      auto GetFrame(int idx)  const noexcept { return mHeap[idx]; }
   #endif
   };
}
