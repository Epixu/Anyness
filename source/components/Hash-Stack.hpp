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
   /// Stores a precomputed hash on the stack.                                
   /// The hash is calculated using the data from the given heap/stack ID.    
   /// The hash is recomputed if GetHash() is invoked when stored hash is 0.  
   ///   @tparam ID the stack/heap source for data                            
   ///   @tparam H the hash type used                                         
   template<unsigned ID = 0, class H = Hash>
   struct HashStack : HashEmergent<ID, H> {
      using StackRequest = H;
      
      /// Reset the hash. It will be recomputed on next comparison.           
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
      constexpr auto& GetHashInner(this auto&& self) noexcept {
         return self.template AccessStack<HashStack>();
      }
      
      /// Set the hash (inner)                                                
      constexpr void SetHashInner(this auto& self, H h) noexcept {
         self.GetHashInner() = h;
      }

      /// Hash is default-initialized to 1, because that's a universal value  
      /// for an empty container. Prevents rehash until something is pushed.  
      constexpr void ConstructDefault(this auto& self) noexcept {
         self.SetHashInner(1);
      }
      
      /// Transfer from any kind of container, respecting intents             
      ///   @attention this is noop when constructing from deep intents,      
      ///      since element constructors might throw and stuff be partially  
      ///      inserted. In those cases, hash is set by the heap components.  
      ///   @param intent the intent and container to transfer from           
      template<CT::Intent I> requires CT::Container<I>
      void ConstructFrom(this auto& self, I&& intent) {
         if constexpr (I::IsShallow() and not CT::Copied<I>) {
            decltype(auto) from = FWD(intent.what);
            self.SetHashInner(from.GetHashInner());
            if constexpr (I::ResetsOnMove())
               from.SetHashInner(1);
         }
      }
   };
}
