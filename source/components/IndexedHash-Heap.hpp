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
   /// Provides random element access by hashing a value of the provided ID.  
   /// Uses a modified Robin Hood algorithm to reuse table space and minimize 
   /// reallocations. Uses multiple cascading tables in order to minimze      
   /// moving things around when rehashing. Doesn't keep a local pointer to   
   /// the hash table, and instead recalculates it on demand from the heap.   
   ///   @tparam ID the stack/heap we're indexing                             
   ///   @tparam HASH type of the hash                                        
   template<unsigned ID, class HASH>
   struct IndexedHashHeap : IndexedCommon<ID> {
      using TableType        = uint8_t;
      using HeapRequest      = PerElement<TableType>;
      using IteratorCategory = ::std::random_access_iterator_tag;

      static constexpr int  InitialTableSize = 8;
      static constexpr int  TableGrowthFactor = 2;

   protected:
      friend struct IndexedCommon<ID>;

      template<CT::Container C>
      using Count = typename Deref<C>::CountType;

      template<CT::Container C>
      static constexpr auto CountMax = ::std::numeric_limits<Count<C>>::max();

      /*template<unsigned, class>      friend struct Insertion;
      template<unsigned, CT::Sparse> friend struct HeapMovable;

      template<CT::Container C>
      using Deep = typename Deref<C>::DeepType;
      template<CT::Container C>
      using Pick = Tmut<C, typename Deref<C>::PickMut, typename Deref<C>::Pick>;
      template<CT::Container C>
      using PickRange = Tmut<C, typename Deref<C>::PickRangeMut, typename Deref<C>::PickRange>;*/
      
      /// Get the start of the hash table (inner)                             
      constexpr auto* GetHashTableInner(this auto&& self) noexcept {
         return self.template AccessHeap<IndexedHashHeap>();
      }

      /// Browse table, converting contiguous index into table index.         
      /// Table is indexed the following way:                                 
      /// 0-8:  [ ][ ][ ][ ][ ][ ][ ][ ]                                      
      /// 9-24: [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]              
      /// 25-56:[ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]...  
      /// It's a so called cascading table structure, designed this way       
      /// to minimize movement and avoid rehashing when table is resized.     
      /// When an element is sought in this cascading table structure, it is  
      /// sought first in the biggest (last) table, and if not found, the     
      /// previous (smaller) tables are searched using the truncated hash.    
      template<CT::Container C>
      constexpr auto BrowseTable(this C const& self, Count<C> index)
      assumptious -> Count<C> {
         LglsAssumeDev(not self.IsEmpty(), "Container can't be empty");
         LglsAssumeDev(index < self.GetCount(), "Index out of bounds");

         const auto reserved = self.GetReserved();
         if (index < self.GetCount() / 2) {
            // Index is in the lower half, so we begin search from start
            Count<C> counter = 0;
            auto table = self.GetHashTableInner();

            while (counter < reserved) {
               if (*table) {
                  if (index == 0)
                     return counter;
                  --index;
                  ++counter;
               }
               ++table;
            }
         }
         else {
            // Index is in the upper half, so we begin search from end  
            int counter = reserved - 1;
            auto table = self.GetHashTableInner() + reserved;

            while (counter >= 0) {
               if (*table) {
                  if (index == 0)
                     return static_cast<Count<C>>(counter);
                  --index;
                  --counter;
               }
               --table;
            }
         }

         LglsError("Should not be reached");
         return 0;
      }

      /// Convert an index to an offset.                                      
      /// Special indices will be contextualized.                             
      ///   @param index the index to simplify                                
      ///   @return a simple element offset into contiguous memory            
      template<CT::Container C, CT::Index INDEX>
      constexpr auto SimplifyIndex(this C const& self, INDEX index)
      assumptious -> Count<C> {
         LglsAssumeDev(not self.IsEmpty(), "Container can't be empty");

         if constexpr      (::std::same_as<INDEX, Index::Inner::All>)
            static_assert(false, "Index::All can't be used here");
         else if constexpr (::std::same_as<INDEX, Index::Inner::Many>)
            static_assert(false, "Index::Many can't be used here");
         else if constexpr (::std::same_as<INDEX, Index::Inner::Single>)
            static_assert(false, "Index::Single can't be used here");
         else if constexpr (::std::same_as<INDEX, Index::Inner::None>)
            static_assert(false, "Index::None can't be used here");
         else if constexpr (::std::same_as<INDEX, Index::Inner::Mode>)
            static_assert(false, "Index::Mode can't be used here");
         else if constexpr (::std::same_as<INDEX, Index::Inner::Front>)
            return self.BrowseTable(0);
         else if constexpr (::std::same_as<INDEX, Index::Inner::Middle>)
            return self.BrowseTable(self.GetCount() / 2);
         else if constexpr (::std::same_as<INDEX, Index::Inner::Back>)
            return self.BrowseTable(self.GetCount());
         else if constexpr (::std::same_as<INDEX, Index::Inner::Biggest>)
            return self.GetIndexLargest();
         else if constexpr (::std::same_as<INDEX, Index::Inner::Smallest>)
            return self.GetIndexSmallest();
         else if constexpr (::std::same_as<INDEX, Index::Inner::Random>)
            return self.GetIndexRandom();
         else if constexpr (::std::same_as<INDEX, Index::Inner::First>)
            return self.BrowseTable(0);
         else if constexpr (::std::same_as<INDEX, Index::Inner::Last>)
            return self.BrowseTable(self.GetCount() - 1);
         else if constexpr (requires { index.index; }) {
            const auto c = self.GetCount();
            // If index is negative, wrap it around (if in range)       
            if (index.index < 0)
               return self.BrowseTable(c + index.index >= 0 ? c + index.index : CountMax<C>);
            return self.BrowseTable(index.index >= c ? CountMax<C> : index.index);

         }
         else if constexpr (CT::Integer<INDEX>) {
            // Using an integer index explicitly makes a statement,     
            // that you know what you're doing                          
            if constexpr (CT::Signed<INDEX>) {
               LglsAssumeUser(index >= 0,
                  "Integer index is below zero, "
                  "use Index::At for reverse indices instead"
               );
            }
            return self.BrowseTable(index);
         }
         else static_assert(false, "Unsupported index type");
      }
   };
}
