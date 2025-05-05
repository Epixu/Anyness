#pragma once
#include <Langulus/HashOf.hpp>


namespace Langulus::Anyness::Component
{

   /// Stores a precomputed hash inside the stack with the given ID           
   /// The hash is calculated using the data from the given heap/stack SOURCE 
   /// The hash is recomputed if GetHash() is invoked when stored hash is 0   
   ///   @tparam ID - the stack ID for storing the hash                       
   ///   @tparam SOURCE - the stack/heap ID for data                          
   ///   @tparam H - the hash type used                                       
   template<unsigned ID = 0, unsigned SOURCE = 0, class H = Hash>
   struct HashStack {
   private:
      H mHash;

   public:
      using CTTI_Component = Yes;

      /// Get the hash, and recompute it if zero                              
      ///   @return the hash                                                  
      auto GetHash() const noexcept { return mHash; }
   };

} // namespace Langulus::Anyness::Component
