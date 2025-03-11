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
   protected:
      using Byte = ::std::uint8_t;

      // A heap of heaps - the inner ones are immovable                 
      Byte** mHeap = nullptr;

      // The start of the reusable chain, in the first heap that has    
      // a free cell                                                    
      Byte* mReusable = nullptr;

   public:
      using CTTI_Component = Yes;

      /// Get a direct access to the heap memory                              
      ///   @returns the memory pointer                                       
      template<CT::Container C>
      constexpr auto GetRaw(this C&& self) noexcept {
         using T = TypeOf<C>;
         if constexpr (CT::Mutable<C>)
            return static_cast<const T*>(self.mHeap);
         else
            return static_cast<      T*>(self.mHeap);
      }

   #if LANGULUS(TESTING)
      auto GetReusable() const noexcept { return mReusable; }
      auto GetFrames()   const noexcept { return mHeap;     }
   #endif
   };

} // namespace Langulus::Anyness::Component
