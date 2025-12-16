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
   /// Stores a precomputed hash inside the heap with the given ID            
   /// The hash is calculated using the data inside the given heap ID         
   /// The hash is recomputed if GetHash() is invoked when stored hash is 0   
   ///   @tparam ID the heap ID                                               
   ///   @tparam H the hash type used                                         
   template<unsigned ID = 0, class H = Hash>
   struct HashHeap : HashEmergent<ID, H> {
      using HeapRequest = H;

      /// Get the hash, but never recompute it                                
      template<CT::Container C>
      H const& GetHashNoRecompute(this const C& self) noexcept {
         return self.template AccessHeap<HashHeap>();
      }

      /// Get the hash, recompute it if uninitialized                         
      template<CT::Container C>
      H GetHash(this const C& self) noexcept {
         H const& cached = self.GetHashNoRecompute();
         if (not cached)
            const_cast<H&>(cached) = self.HashRecompute();
         return cached;
      }
   };
}
