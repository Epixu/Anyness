///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
#include "Indexed-Common.hpp"
#include <Langulus/HashOf.hpp>


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Provides random element access by hashing a value of the provided ID   
   /// Uses a modified Robin Hood algorithm to reuse table space and minimize 
   /// movement on rehash. Keeps a local pointer to the hash table for faster 
   /// and more cache-friendly access.                                        
   ///   @tparam ID the stack/heap we're indexing                             
   ///   @tparam HASH type of the hash                                        
   template<Cid ID, class HASH = Hash>
   struct IndexedHashStack : IndexedCommon<ID> {
      using TableType        = uint8_t;
      using HeapRequest      = PerElement<TableType>;
      using StackRequest     = TableType*;
      using IteratorCategory = ::std::random_access_iterator_tag;

      /// Get the start of the hash table                                     
      constexpr T GetHashTable(this auto const& self) noexcept {
         return self.GetCountInner();
      }

      /// Get the end of the hash table                                       
      constexpr auto GetHashTableEnd(this auto const& self) noexcept -> TableType const* {
         return self.GetHashTable() + self.GetReserved();
      }

   protected:
      friend struct IndexedCommon<ID>;

      /// Get hash table (inner)                                              
      constexpr auto& GetHashTableInner(this auto&& self) noexcept {
         return self.template AccessStack<IndexedHashStack>();
      }
      
      /// Set the number of initialized elements                              
      constexpr void SetHashTableInner(this auto& self, TableType* c) noexcept {
         self.GetHashTableInner() = c;
      }
      
      /// Default-initialize hash table to zero                               
      constexpr void ConstructDefault(this auto& self) noexcept {
         self.SetHashTableInner(nullptr);
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
