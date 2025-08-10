///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
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
   template<unsigned ID = 0, class H = Hash>
   struct HashStack : HashEmergent<ID, H> {
   private:
      H mHash;
      
   public:
      /// Reset the hash. It will be recomputed on next comparison            
      void ResetHash() noexcept { mHash = {}; }

      /// Get the hash, recompute it if uninitialized                         
      template<CT::Container C>
      H GetHash(this const C& self) noexcept {
         auto& cached = self.GetHashNoRecompute();
         if (not cached)
            const_cast<H&>(cached) = self.HashRecompute();
         return cached;
      }

   protected:
      template<unsigned>
      friend struct HeapMovable;
      
      /// Set the hash directily (for internal use)                           
      void SetHash(H hash) noexcept { mHash = hash; }
      
      /// Get the hash, but never recompute it                                
      const H& GetHashNoRecompute() const noexcept { return mHash; }
   };
}
