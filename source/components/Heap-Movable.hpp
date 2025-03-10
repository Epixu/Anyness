#pragma once
#include "../Container.hpp"
#include <Langulus/TypeOf.hpp>


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Interfaces a heap allocation                                           
   /// Adds a pointer member to the raw byte memory                           
   /// The pointer is allowed to move on reallocation                         
   ///   @tparam ID - multiple heap interfaces are supported                  
   ///                                                                        
   template<unsigned ID = 0>
   struct HeapMovable {
   protected:
      using Byte = ::std::uint8_t;

      // The raw pointer                                                
      Byte* mHeap = nullptr;

   public:
      using CTTI_Component = Yes;

      /// Get a direct access to the heap memory                              
      ///   @returns the memory pointer                                       
      template<CT::Container C>
      constexpr auto GetRaw(this C&& self) noexcept {
         using T = TypeOf<C>;
         if constexpr (CT::Mutable<C>) return static_cast<const T*>(self.mHeap);
         else                          return static_cast<      T*>(self.mHeap);
      }
   };

} // namespace Langulus::Anyness::Component
