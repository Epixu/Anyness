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
   /// movement on rehash. Keeps a local pointer to the hash table for faster 
   /// and more cache-friendly access. That also allows for disownment.       
   ///   @tparam ID the provider we're indexing                               
   ///   @tparam HASH type of the hash                                        
   ///   @tparam SHARED providers that share the same indexing scheme         
   template<Cid ID, class HASH, Cid...SHARED>
   struct IndexedHashStack : IndexedCommonHashed<ID, HASH, SHARED...> {
      using TableType        = typename IndexedCommonHashed<ID, HASH>::TableType;
      using HeapRequest      = PerElement<TableType>;
      using StackRequest     = TableType*;
      using IteratorCategory = typename IndexedCommonHashed<ID, HASH>::IteratorCategory;

      /// Get the start of the hash table                                     
      template<Cid SID = ID>
      constexpr auto GetHashTable(this auto const& self) noexcept -> TableType const* {
         static_assert(SID == ID or ((SID == SHARED) or ...));
         return self.template GetHashTableInner<SID>();
      }

      /// Get the end of the hash table                                       
      template<Cid SID = ID>
      constexpr auto GetHashTableEnd(this auto const& self) noexcept -> TableType const* {
         static_assert(SID == ID or ((SID == SHARED) or ...));
         return self.template GetHashTable<SID>() + self.template GetReserved<SID>();
      }

   protected:
      template<Cid, uint, uint, CT::HeapEntry...> friend struct HeapMovable;
      template<Cid, Cid...>         friend struct IndexedCommon;
      template<Cid, class, Cid...>  friend struct IndexedCommonHashed;
      LglsComMerging(friend);
      template<Cid, Cid...>         friend struct Removal;

      /// Get hash table (inner)                                              
      template<Cid SID = ID>
      constexpr auto& GetHashTableInner(this auto&& self) noexcept {
         static_assert(SID == ID or ((SID == SHARED) or ...));
         return self.template AccessStack<IndexedHashStack>();
      }
      
      /// Set the number of initialized elements                              
      template<Cid SID = ID>
      constexpr void SetHashTableInner(this auto& self, TableType const* c) noexcept {
         static_assert(SID == ID or ((SID == SHARED) or ...));
         self.template GetHashTableInner<SID>() = const_cast<TableType*>(c);
      }

      /// This method is called to erase the hash table                       
      template<Cid SID = ID>
      constexpr void ResetHashTable(this auto& self) noexcept {
         static_assert(SID == ID or ((SID == SHARED) or ...));
         memset(
            self.template GetHashTableInner<SID>(), 0,
            self.template GetReserved<SID>() * sizeof(TableType)
         );
      }

      /// Default-initialize hash table to zero                               
      constexpr void ConstructDefault(this auto& self) noexcept {
         self.SetHashTableInner(nullptr);
      }

      /// This method is called upon allocation to nullify table              
      constexpr void ConstructHeapRequest(this auto& self) noexcept {
         self.SetHashTableInner(self.template AccessHeap<IndexedHashStack>());
         self.ResetHashTable();
      }

      /// Transfer from any kind of container, respecting intents             
      ///   @attention this is noop when constructing from deep intents,      
      ///      since element constructors might throw and stuff be partially  
      ///      inserted. In those cases, table is set by the heap components. 
      ///   @param intent the intent and container to transfer from           
      template<CT::Intent I> requires CT::Container<I>
      void ConstructFrom(this auto& self, I&& intent) {
         if constexpr (not CT::Copied<I> and not CT::Cloned<I>) {
            decltype(auto) from = LglsFwd(intent.what);
            self.SetHashTableInner(from.GetHashTable());
            if constexpr (I::ResetsOnMove()) {
               if_available(from.SetHashTableInner(nullptr));
            }
         }
      }
   };
}
