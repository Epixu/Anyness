#pragma once
#include "Hash-Emergent.hpp"


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Stores a precomputed hash on the stack                                 
   /// The hash is calculated using the data from the given heap/stack ID     
   /// The hash is recomputed if GetHash() is invoked when stored hash is 0   
   ///   @tparam ID - the stack/heap source for data                          
   ///   @tparam H - the hash type used                                       
   ///                                                                        
   template<unsigned ID = 0, class H = Hash>
   struct HashStack : HashEmergent<ID, H> {
   private:
      H mHash;

   public:
      /// Get the hash, but never recompute it                                
      const H& GetHashNoRecompute() const noexcept {
         return mHash;
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
