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
   /// The hash is recomputed if GetHash() is invoked when stored hash is 0.  
   ///   @attention since hash is stored on the heap, it is recomputed as     
   ///      emergent when we have no ownership of that heap, because we will  
   ///      not be allowed to write that hash value down and cache it. This   
   ///      means that disowned containers will suffer a big performance cost 
   ///      every time they're hashed.                                        
   ///   @tparam ID the provider ID whose data will be hashed                 
   ///   @tparam H the hash type used                                         
   ///   @tparam SHARED additional provider IDs that are hashed together.     
   ///      They will all share the same cached hash variable.                
   template<Cid ID, class H, Cid...SHARED>
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

         const auto heap = self.template AccessHeap<HashHeap>();
         LglsAssumeDevAndOptimize( heap, "Invalid heap");
         if (not *heap)
            const_cast<H&>(*heap) = self.HashRecompute();
         return *heap;
      }

   protected:
      template<Cid, uint, uint, CT::HeapEntry...> friend struct HeapMovable;
      template<Cid>                               friend struct Conversion;

      /// Get hash (inner) - will never recompute it                          
      constexpr auto GetHashInner(this auto&& self) noexcept -> H const {
         if (self.IsEmpty())
            return H {1};
         else if (self.GetUses() == 0)
            return H {0};

         const auto heap = self.template AccessHeap<HashHeap>();
         return heap ? *heap : H {0};
      }
      
      /// Set the hash (inner)                                                
      ///   @attention will not work for disowned containers                  
      constexpr void SetHashInner(this auto& self, H h) noexcept {
         if (self.IsEmpty() or self.GetUses() == 0)
            return;

         const auto heap = self.template AccessHeap<HashHeap>();
         LglsAssumeDev(heap, "Invalid heap");
         const_cast<H&>(*heap) = h;
      }
      
      /// Transfer from any kind of container, respecting intents             
      ///   @attention this is noop when constructing from deep intents,      
      ///      since element constructors might throw and stuff be partially  
      ///      inserted. In those cases, hash is set by the heap provider.    
      ///   @attention nothing is transferred when disowned, because hash     
      ///      must be kept in heap memory relative to the allocation         
      ///   @param intent the intent and container to transfer from           
      template<CT::Intent I> requires CT::Container<I>
      void ConstructFrom(this auto& self, I&& intent) {
         if constexpr (not CT::Copied<I> and not CT::Cloned<I> and not CT::Disowned<I>) {
            decltype(auto) from = LglsFwd(intent.what);
            // Notice only the inner hash gets copied, to avoid         
            // precomputation if rhs doesn't cache it. It will be       
            // recomputed on demand on comparison either way.           
            self.SetHashInner(from.GetHashInner());
            if constexpr (I::ResetsOnMove()) {
               if_available(from.SetHashInner(1));
            }
         }
      }
   };
}
