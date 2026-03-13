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
   /// Stores a precomputed hash inside the heap with the given ID.           
   /// The hash is calculated using the data inside the given heap/stack ID.  
   /// The hash is recomputed if GetHash() is invoked when stored hash is 0.  
   ///   @attention since hash is stored on the heap, it is recomputed as     
   ///      emergent when we have no ownership of that heap.                  
   ///   @tparam ID the heap ID                                               
   ///   @tparam H the hash type used                                         
   template<Cid ID = 0, class H = Hash>
   struct HashHeap : HashEmergent<ID, H> {
      using HeapRequest = H;

      /// Reset the hash. It will be recomputed on next comparison.           
      void ResetHash(this auto& self) noexcept {
         self.SetHashInner(self.IsEmpty() ? 1 : 0);
      }

      /// Get the hash, recompute it if uninitialized or of we don't own it.  
      H GetHash(this auto const& self) assumptious {
         if (self.IsEmpty())
            return H{1};
         else if (self.GetUses() == 0)
            return self.HashRecompute();

         auto heap = self.template AccessHeap<HashHeap>();
         LglsAssumeDev(heap, "Invalid heap");
         if (not *heap)
            const_cast<H&>(*heap) = self.HashRecompute();
         return *heap;
      }

   protected:
      template<Cid, uint, uint, CT::Sparse> friend struct HeapMovable;
                                            friend struct Conversion;

      /// Get hash (inner) - will not recompute it                            
      constexpr auto GetHashInner(this auto&& self) noexcept -> H const {
         if (self.IsEmpty())
            return H {1};

         auto heap = self.template AccessHeap<HashHeap>();
         return heap ? *heap : H {0};
      }
      
      /// Set the hash (inner)                                                
      constexpr void SetHashInner(this auto& self, H h) noexcept {
         if (self.IsEmpty() or self.GetUses() == 0)
            return;

         auto heap = self.template AccessHeap<HashHeap>();
         LglsAssumeDev(heap, "Invalid heap");
         const_cast<H&>(*heap) = h;
      }
      
      /// Transfer from any kind of container, respecting intents             
      ///   @attention this is noop when constructing from deep intents,      
      ///      since element constructors might throw and stuff be partially  
      ///      inserted. In those cases, hash is set by the heap components.  
      ///   @param intent the intent and container to transfer from           
      template<CT::Intent I> requires CT::Container<I>
      void ConstructFrom(this auto& self, I&& intent) {
         if constexpr (not CT::Copied<I> and not CT::Cloned<I>) {
            decltype(auto) from = LglsFwd(intent.what);
            self.SetHashInner(from.GetHashInner());
            if constexpr (I::ResetsOnMove())
               if_available(from.SetHashInner(1));
         }
      }
   };
}
