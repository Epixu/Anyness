///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Insertion.hpp"
#include "Langulus/Typenav.hpp"
#include "source/Component.hpp"


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Implements merging for containers.                                     
   /// Merging (unlike emplacement) extends the memory space and may move     
   /// things around. It guarantees that nothing gets overwritten.            
   /// Merging (unlike insertion) disallows for duplicated elements.          
   ///   @tparam ID heap we're merging to                                     
   ///   @tparam AS type to serialize as before merging. Useful for byte      
   ///      and text containers. Use void to insert without serialization.    
   ///   @tparam SHARED other providers that share merge behavior             
   template<Cid ID, class AS, Cid...SHARED>
   struct Merging {
      using CTTI_Component = Yes<>;
      using CTTI_ReflectAs = void;
      using Id = Values<ID, SHARED...>;

      static constexpr int  ComponentPrecedence = 3000;
      static constexpr bool Shared = sizeof...(SHARED) > 0;

   private:
      template<CT::Container C>
      using Count = typename C::CountType;
      template<CT::Container C>
      using Deep = typename C::DeepType;

   public:
      /// MARK: Public                                                        
      /// Merging at specific index                                           
      template<class A1, class...AN, CT::IndexedLinearly C>
      auto MergeAt(this C&, CT::Index auto, A1&&, AN&&...)
         -> Count<C> requires CT::RangeInsertable<C, A1, AN...>;

      template<CT::IndexedLinearly C>
      auto MergeRangeAt(this C&, CT::Index auto, CT::Container auto&&)
         -> Count<C>;

      /// Merge one or more elements at the performance-optimal position.     
      /// This usually means at the back of a contiguous container. Supports  
      /// intents and arrays.                                                 
      ///   @param a element or an array of elements (and their intent)       
      ///   @return the number of inserted elements                           
      template<class A, CT::ContainsMany C>
      auto Merge(this C& self, A&& a) -> size_t {
         if constexpr (CT::Contiguous<C> or (not CT::Handle<A> and CT::Mutable<Deint<A>>))
            return self.MergeInner(LglsFwd(a)).itemsInserted;
         else if constexpr (requires { Decay<Deint<A>> {LglsFwd(a)}; }) {
            // Table merge requires a local copy as a swapper           
            Decay<Deint<A>> localCopy {LglsFwd(a)};
            return self.MergeInner(Abandon(localCopy)).itemsInserted;
         }
         else {
            // Table merge requires a local copy as a swapper           
            Decay<Deint<A>> localCopy {DeintCast(a)};
            return self.MergeInner(Abandon(localCopy)).itemsInserted;
         }
      }

      template<CT::Container C>
      auto MergeRange(this C&, CT::Container auto&&) -> Count<C>;
      
   protected:
      /// MARK: Protected                                                     
      /// Helper struct for returning insertion status                        
      struct MergeResult {
         size_t itemsInserted = 0;
         size_t lastInsertedIndex = 0;
      };

      /// Merge a pair at the performance-optimal position.                   
      /// This usually means at the back of a contiguous container.           
      ///   @attention all types need to be set prior to calling this function
      ///   @attention when inserting in a hash table, the item is used as a  
      ///      swapper, and has to be strongly owned from outside this call   
      ///   @param a pair of elements (and its intent) to merge               
      ///   @return 1 if element was inserted, and the position where it was  
      ///      inserted (or found at, if it was already existing)             
      template<class T, CT::ContainsMany C>
      auto MergeInner(this C& self, T&& item) -> MergeResult {
         static_assert(not CT::Array<T>,
            "This inner routine doesn't account for arrays");

         // If this is reached, then types are the same                 
         // Reallocate/branch out                                       
         const size_t lhs_count = self.GetCount();
         const size_t all_count = lhs_count + 1;
         self.BranchOut(all_count);

         // Insert the new elements if they're not contained yet        
         MergeResult result;
         try {
            if constexpr (CT::Contiguous<C>) {
               // Contiguous merge                                      
               if (not self.IsEmpty()) {
                  if (const auto found = self.FindInner(DeintCast(item), 0)) {
                     result.lastInsertedIndex = found - self.GetHandle();
                     return result;
                  }
               }

               auto to = self.GetHandle() + lhs_count;
               Id::ForEach([&]<Cid D>{
                  if constexpr (CT::Copied<IntentOf(item)>)
                     to.template EmplaceWithIntent<D>(Refer(LglsFwd(item)));
                  else
                     to.template EmplaceWithIntent<D>(FWDIntent(item));
               });

               result.lastInsertedIndex = lhs_count;
               ++result.itemsInserted;
            }
            else if constexpr (not Shared) {
               // Hash table merge                                      
               static_assert(not CT::Handle<T> and CT::Mutable<Deint<T>>,
                  "Item needs to be strongly owned and mutable, because "
                  "it will be used as a temporary swapper");
      
               const auto bucket = self.GetOffset(DeintCast(item));
               if (not self.IsEmpty()) {
                  if (const auto found = self.FindInner(DeintCast(item), bucket)) {
                     result.lastInsertedIndex = found - self.GetHandle();
                     return result;
                  }
               }

               result.lastInsertedIndex = self.TableEmplace(bucket, DeintCast(item));
               ++result.itemsInserted;
            }
            else {
               // Hash table merge (multidimensional)                   
               static_assert(not CT::Handle<T> and CT::Mutable<Deint<T>>,
                  "Item needs to be strongly owned and mutable, because "
                  "it will be used as a temporary swapper");
      
               const auto key = DeintCast(item).GetKeyHandle(); //TODO this presumes the key dimension is the one the hash table is associated with
               const auto bucket = self.GetOffset(key);
               if (not self.IsEmpty()) {
                  if (const auto found = self.FindInner(key, bucket)) {
                     result.lastInsertedIndex = found - self.GetHandle();
                     return result;
                  }
               }

               result.lastInsertedIndex = self.TableEmplace(bucket, DeintCast(item));
               ++result.itemsInserted;
            }
         }
         catch (...) {
            // Account for throws inside constructors                   
            self.SetCountInner(lhs_count + result.itemsInserted);
            throw;
         }

         self.SetCountInner(lhs_count + result.itemsInserted);
         return result;
      }
   };
}
