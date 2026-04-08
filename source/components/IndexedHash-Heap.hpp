///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Indexed-Common-Hashed.hpp"


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Provides random element access by hashing a value of the provided ID.  
   /// Uses a modified Robin Hood algorithm to reuse table space and minimize 
   /// reallocations. Uses multiple cascading tables in order to minimze      
   /// moving things around when rehashing. Doesn't keep a local pointer to   
   /// the hash table, and instead recalculates it on demand from the heap.   
   ///   @attention keeping the hash table on the heap disallows disownment   
   ///   @tparam ID the provider we're indexing                               
   ///   @tparam HASH type of the hash                                        
   ///   @tparam SHARED providers that share the same indexing scheme         
   template<Cid ID, class HASH, Cid...SHARED>
   struct IndexedHashHeap : IndexedCommonHashed<ID, HASH, SHARED...> {
      using TableType        = typename IndexedCommonHashed<ID, HASH>::TableType;
      using HeapRequest      = PerElement<TableType>;
      using IteratorCategory = typename IndexedCommonHashed<ID, HASH>::IteratorCategory;

      /// Get the start of the hash table                                     
      constexpr auto GetHashTable(this auto const& self) noexcept -> TableType const* {
         return self.template AccessHeap<IndexedHashHeap>();
      }

      /// Get the end of the hash table                                       
      constexpr auto GetHashTableEnd(this auto const& self) noexcept -> TableType const* {
         return self.GetHashTable() + self.GetReserved();
      }

   protected:
      template<Cid, uint, uint, CT::HeapEntry...> friend struct HeapMovable;
      template<Cid, Cid...>         friend struct IndexedCommon;
      template<Cid, class, Cid...>  friend struct IndexedCommonHashed;
      template<Cid, class>          friend struct Merging;
      template<Cid, Cid...>         friend struct Removal;

      /// Get the start of the hash table (inner)                             
      constexpr auto* GetHashTableInner(this auto&& self) noexcept {
         return self.template AccessHeap<IndexedHashHeap>();
      }

      /// This method is called upon allocation to nullify table              
      constexpr void ConstructHeapRequest(this auto& self) noexcept {
         self.ResetHashTable();
      }

      /// This method is called to erase the hash table                       
      constexpr void ResetHashTable(this auto& self) noexcept {
         memset(self.GetHashTableInner(), 0, self.GetReserved() * sizeof(TableType));
      }
   };
}
