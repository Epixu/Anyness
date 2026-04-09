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
   /// The hash is recomputed if GetHash() is invoked when stored hash is 0.  
   ///   @tparam ID the provider ID whose data will be hashed                 
   ///   @tparam H the hash type used                                         
   ///   @tparam SHARED additional provider IDs that are hashed together.     
   ///      They will all share the same cached hash variable.                
   template<Cid ID, class H, Cid...SHARED>
   struct HashStack : HashEmergent<ID, H> {
      using StackRequest = H;
      
      static constexpr Cid Id = ID;

      /// Reset the hash. It will be recomputed on next comparison.           
      template<Cid SID = ID>
      void ResetHash(this auto& self) noexcept {
         static_assert(SID == ID or ((SID == SHARED) or ...));
         self.template SetHashInner<SID>(self.template IsEmpty<SID>() ? 1 : 0);
      }

      /// Get the hash, recompute it if uninitialized                         
      template<Cid SID = ID>
      H GetHash(this auto const& self) noexcept {
         static_assert(SID == ID or ((SID == SHARED) or ...));
         auto& cached = self.template GetHashInner<SID>();
         if (not cached)
            const_cast<H&>(cached) = self.template HashRecompute<SID>();
         return cached;
      }

   protected:
      template<Cid, uint, uint, CT::HeapEntry...> friend struct HeapMovable;
      template<Cid, Cid...>                       friend struct Conversion;

      /// Get hash (inner) - will not recompute it                            
      template<Cid SID = ID>
      constexpr auto& GetHashInner(this auto&& self) noexcept {
         static_assert(SID == ID or ((SID == SHARED) or ...));
         return self.template AccessStack<HashStack>();
      }
      
      /// Set the hash (inner)                                                
      template<Cid SID = ID>
      constexpr void SetHashInner(this auto& self, H h) noexcept {
         static_assert(SID == ID or ((SID == SHARED) or ...));
         self.template GetHashInner<SID>() = h;
      }

      /// Hash is default-initialized to 1, because that's a universal value  
      /// for an empty container. Prevents rehash until something is pushed.  
      template<Cid SID = ID>
      constexpr void ConstructDefault(this auto& self) noexcept {
         static_assert(SID == ID or ((SID == SHARED) or ...));
         self.template SetHashInner<SID>(1);
      }
      
      /// Transfer from any kind of container, respecting intents             
      ///   @attention this is noop when constructing from deep intents,      
      ///      since element constructors might throw and stuff be partially  
      ///      inserted. In those cases, hash is set by the heap components.  
      ///   @param intent the intent and container to transfer from           
      template<CT::Intent I> requires CT::Container<I>
      void ConstructFrom(this auto& self, I&& intent) {
         if constexpr (not CT::Copied<I> and not CT::Cloned<I>
         and requires { intent.what.GetHashInner(); }) {
            decltype(auto) from = LglsFwd(intent.what);
            // Notice only the inner hash gets copied, to avoid         
            // precomputation if rhs doesn't cache it. It will be       
            // recomputed on demand on comparison either way.           
            self.SetHashInner(from.GetHashInner());
            if constexpr (I::ResetsOnMove())
               if_available(from.SetHashInner(1));
         }
      }
   };
}
