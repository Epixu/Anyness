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
   ///   @attention keeping the hash table on the heap disallows disownment   
   ///   @tparam ID the stack/heap we're indexing                             
   ///   @tparam HASH type of the hash                                        
   template<Cid ID, class HASH>
   struct IndexedHashHeap : IndexedCommon<ID> {
      using TableType        = uint8_t;
      using HeapRequest      = PerElement<TableType>;
      using IteratorCategory = ::std::random_access_iterator_tag;

      /// Get the start of the hash table                                     
      constexpr auto GetHashTable(this auto const& self) noexcept -> TableType const* {
         return self.template AccessHeap<IndexedHashHeap>();
      }

      /// Get the end of the hash table                                       
      constexpr auto GetHashTableEnd(this auto const& self) noexcept -> TableType const* {
         return self.GetHashTable() + self.GetReserved();
      }

   protected:
      friend struct IndexedCommon<ID>;

      template<CT::Container C>
      using Count = typename Deref<C>::CountType;

      template<CT::Container C>
      static constexpr auto CountMax = ::std::numeric_limits<Count<C>>::max();

      /*template<Cid, class>      friend struct Insertion;*/
      template<Cid, uint, uint, CT::Sparse>  friend struct HeapMovable;
      template<Cid, class>                   friend struct Merging;

      /// Get the start of the hash table (inner)                             
      constexpr auto* GetHashTableInner(this auto&& self) noexcept {
         return self.template AccessHeap<IndexedHashHeap>();
      }

      /// This method is called upon allocation to nullify table              
      constexpr void ConstructHeapRequest(this auto& self) noexcept {
         memset(self.GetHashTableInner(), 0, self.GetReserved() * sizeof(TableType));
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
         auto const tableBeg = self.GetHashTableInner();
         auto const tableEnd = tableBeg + reserved;

         if (index <= self.GetCount() / 2) {
            // Index is in the lower half, so we begin search from start
            auto table = tableBeg;
            while (table < tableEnd) {
               if (*table) {
                  if (index == 0)
                     return table - tableBeg;
                  --index;
               }
               ++table;
            }
         }
         else {
            // Index is in the upper half, so we begin search from end  
            auto table = tableEnd - 1;
            while (table >= tableBeg) {
               if (*table) {
                  if (index == 0)
                     return table - tableBeg;
                  --index;
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
            // Using an integer index explicitly makes a statement      
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

      /// Get the offset, based on the provided value's hash. The offset is   
      /// truncated down to the size of the biggest cascading table.          
      /// You have to disable the most significant bit for each other table.  
      ///   @param value - the value to hash                                  
      ///   @return the bucket index                                          
      template<CT::Container C, CT::NoIntent T>
      auto GetOffset(this C const& self, T const& value) noexcept {
         const auto mask = ::std::bit_floor(self.GetReserved()) - 1u;
         return HashOf(value).value & mask;
      }

      /// Rehashes and reinserts each element, optimizing the table           
      ///   @param oldReserve - the old table size                            
      ///   @attention assumes reserve > oldReserve                           
      template<CT::Container C>
      void Rehash(this C& self, const Count<C> oldReserve) {
         LglsAssumeDev(self.GetReserved() > oldReserve,
            "New reserve is not larger than oldReserve");

         auto& count = self.GetCountInner();
         auto handle = self.GetHandle();
         const auto tableBeg = self.GetHashTableInner();
         const auto tableEnd = tableBeg + oldReserve;

         // First run: move elements closer to their new buckets        
         auto table = tableBeg;
         while (table != tableEnd) {
            if (*table) {
               // Rehash and check if hashes match                      
               Count<C> const oldIndex = table - tableBeg;
               Count<C> oldBucket = (oldReserve + oldIndex) - *table + 1;
               Count<C> newBucket = self.GetOffset(handle);

               if (oldBucket < oldReserve or oldBucket - oldReserve != newBucket) {
                  // Move it only if it won't end up in same bucket     
                  if constexpr (CT::TypeErased<C>) {
                     // Move the element to a temporary swapper first   
                     Any swapper {Piecewise, Abandon(handle)};
                     // Destroy the old element                         
                     handle.FreeInner();
                     *table = 0;
                     --count;
                     // Reinsert at the new offset                      
                     self.TableInsert(newBucket, swapper.GetHandle());
                  }
                  else {
                     // Move the element to a temporary swapper first   
                     THandle<Decvq<Deref<TypeOf<C>>>> swapper {Abandon(handle)};
                     // Destroy the old element                         
                     handle.FreeInner();
                     *table = 0;
                     --count;
                     // Reinsert at the new offset                      
                     self.TableInsert(newBucket, swapper);
                  }
               }
            }

            ++handle;
            ++table;
         }

         // Second run: shift elements left whereever possible to fill  
         // any gaps produced by the first run.                         
         self.ShiftEntries();
      }
   
      /// Shift elements left whereever possible                              
      template<CT::Container C>
      void ShiftEntries(this C& self) {
         const auto reserved = self.GetReserved();
         int moves_performed;
         do {
            moves_performed = 0;
            const auto tableBeg = self.GetHashTableInner();
            const auto tableEnd = tableBeg + reserved;

            auto table = tableBeg;
            while (table != tableEnd) {
               if (*table > 1) {
                  // Entry can be moved *table - 1 cells to the left    
                  const Count<C> oldIndex = table - tableBeg;
                  Count<C> newIndex = reserved + oldIndex - *table + 1;
                  if (newIndex >= reserved)
                     newIndex -= reserved;   // Might loop around       

                  TableType attempt = 1;
                  while (tableBeg[newIndex] and attempt < *table) {
                     // Might loop around                               
                     ++newIndex;
                     if (newIndex >= reserved)
                        newIndex -= reserved;
                     ++attempt;
                  }

                  if (not tableBeg[newIndex] and attempt < *table) {
                     // Empty spot found, so move element there         
                     auto handle = self.GetHandle();
                     auto from   = handle + oldIndex;
                     auto to     = handle + newIndex;
                     to.EmplaceWithIntent(Abandon(from));
                     from.DestroyElement();

                     tableBeg[newIndex] = attempt;
                     *table = 0;
                     ++moves_performed;
                  }
               }

               ++table;
            }
         } while (moves_performed);
      }

      /// Table insertion function                                            
      ///   @param start - the starting index                                 
      ///   @param swapper - a swapper to use while trying to insert          
      ///   @return the offset at which pair was inserted                     
      template<CT::Container C, CT::Handle H> 
      auto TableEmplace(this C& self, Count<C> const start, H& swapper)
      -> Count<C> requires CT::NoIntent<H> {
         // Get the starting index based on the key hash                
         const auto reserved = self.GetReserved();
         const auto tableBeg = self.GetHashTableInner();
         const auto tableEnd = tableBeg + reserved;

         TableType attempts = 1;
         auto insertedAt = reserved;
         auto table = tableBeg + start;
         auto handle = self.GetHandle();
         while (*table) {
            const auto index = table - tableBeg;
            if (attempts > *table) {
               // The value we're inserting is closer to bucket, so swap
               (handle + index).SwapInner(swapper);
               ::std::swap(attempts, *table);
               if (insertedAt == reserved)
                  insertedAt = index;
            }

            ++attempts;

            // Wrap around and start from the beginning if we have to   
            if (table < tableEnd - 1) ++table;
            else table = tableBeg;
         }

         // If reached, then empty slot found, so put the value there   
         const auto index = table - tableBeg;
         (handle + index).EmplaceWithIntent(Abandon(swapper));
         if (insertedAt == reserved)
            insertedAt = index;

         *table = attempts;
         //++self.GetCountInner();
         return insertedAt;
      }
   };
}
