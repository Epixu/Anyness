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

      /// Get the hash, and recompute it if zero                              
      ///   @return the hash                                                  
      template<class Self>
      H GetHash(this const Self& self) noexcept {
         return self.GetHeap<ID>().GetElement<H, ID>();
         //TODO recompute hash
      }
   };

} // namespace Langulus::Anyness::Component
