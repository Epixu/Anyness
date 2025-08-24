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
      static constexpr int StackSize = sizeof(H);
      
      /// Reset the hash. It will be recomputed on next comparison            
      void ResetHash(this auto& self) noexcept {
         self.SetHashInner(0);
      }

      /// Get the hash, recompute it if uninitialized                         
      H GetHash(this auto const& self) noexcept {
         auto& cached = self.GetHashInner();
         if (not cached)
            const_cast<H&>(cached) = self.HashRecompute();
         return cached;
      }

   protected:
      template<unsigned>
      friend struct HeapMovable;
      
      /// Get hash (inner) - will not recompute it                            
      constexpr auto& GetHashInner(this auto const& self) noexcept {
         return *reinterpret_cast<H const*>(
            self.mStack + self.template StackOffset<HashStack>
         );
      }
      
      /// Set the hash (inner)                                                
      constexpr void SetHashInner(this auto& self, H h) noexcept {
         const_cast<H&>(self.GetHashInner()) = h;
      }
   };
}
