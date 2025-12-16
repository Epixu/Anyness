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
   ///   @tparam ID multiple heap interfaces are supported                    
   template<unsigned ID>
   struct HeapImmovable {
      using CTTI_Component = Yes<>;
      static constexpr unsigned Id = ID;
      static constexpr int  ComponentPrecedence = -2000;
      static constexpr bool HeapAllocated = true;
      static constexpr bool HeapCanBeNull = true;

   protected:
      using Byte = uint8_t;

      // A heap of heaps - the inner ones are immovable                 
      Byte** mHeap;
      // Number of allocated heaps - each new heap is twice as big      
      uint8_t mHeapCount;

      // The start of the reusable chain, in the first heap that has    
      // a free cell                                                    
      Byte* mReusable;

   public:
   #if LANGULUS(TESTING)
      auto GetReusable()      const noexcept { return mReusable;  }
      auto GetFrames()        const noexcept { return mHeapCount; }
      auto GetFrame(int idx)  const noexcept { return mHeap[idx]; }
   #endif
   };
}
