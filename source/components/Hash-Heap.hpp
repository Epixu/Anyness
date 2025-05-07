#pragma once
#include <Langulus/HashOf.hpp>


namespace Langulus::Anyness::Component
{

   /// Stores a precomputed hash inside the heap with the given ID            
   /// The hash is calculated using the data inside the given heap ID         
   /// The hash is recomputed if GetHash() is invoked when stored hash is 0   
   ///   @tparam ID - the heap ID                                             
   ///   @tparam H  - the hash type used                                      
   template<unsigned ID = 0, class H = Hash>
   struct HashHeap {
      using CTTI_Component = Yes;
      static constexpr unsigned HeapID = ID;
      static constexpr unsigned HeapHeaderSize = sizeof(H);

      /// Get the hash, and recompute it if zero                              
      ///   @return the hash                                                  
      template<CT::Container C>
      H GetHash(this const C& self) noexcept {
         constexpr unsigned heapOffset = C::template GetHeapHeaderOffset<HashHeap>();
         auto cached = reinterpret_cast<const H*>(self.GetAllocation()->GetBlockStart() + heapOffset);
         //TODO recompute hash
      }
   };

} // namespace Langulus::Anyness::Component
