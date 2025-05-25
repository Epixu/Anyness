#pragma once
#include "Hash-Emergent.hpp"


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Stores a precomputed hash inside the heap with the given ID            
   /// The hash is calculated using the data inside the given heap ID         
   /// The hash is recomputed if GetHash() is invoked when stored hash is 0   
   ///   @tparam ID - the heap ID                                             
   ///   @tparam H  - the hash type used                                      
   ///                                                                        
   template<unsigned ID = 0, class H = Hash>
   struct HashHeap : HashEmergent<ID, H> {
      static constexpr unsigned HeapID = ID;
      static constexpr unsigned HeapHeaderSize = sizeof(H);

      /// Get the hash, but never recompute it                                
      template<CT::Container C>
      const H& GetHashNoRecompute(this const C& self) noexcept {
         constexpr unsigned heapOffset = C::template GetHeapHeaderOffset<HashHeap>();
         return *static_cast<const H*>(self.GetAllocation()->GetBlockStart() + heapOffset);
      }

      /// Get the hash, recompute it if uninitialized                         
      template<CT::Container C>
      H GetHash(this const C& self) noexcept {
         auto& cached = self.GetHashNoRecompute();
         if (not cached)
            const_cast<H&>(cached) = self.HashRecompute();
         return cached;
      }
   };

} // namespace Langulus::Anyness::Component
