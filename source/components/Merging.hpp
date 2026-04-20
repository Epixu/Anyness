///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Insertion.hpp"


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

      static constexpr Cid  Id = ID;
      static constexpr int  ComponentPrecedence = 3000;
      static constexpr bool Shared = sizeof...(SHARED) > 0;

   private:
      template<CT::Container C>
      using Count = typename C::CountType;
      template<CT::Container C>
      using Deep = typename C::DeepType;

   public:
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
      auto Merge(this C& self, A&& a) -> Count<C> {
         return static_cast<Count<C>>(self.MergeInner(LglsFwd(a)).itemsInserted);
      }

      template<CT::Container C>
      auto MergeRange(this C&, CT::Container auto&&) -> Count<C>;
      
   protected:
      /// Helper struct for returning insertion status                        
      struct MergeResult {
         size_t itemsInserted = 0;
         size_t lastInsertedIndex = 0;
      };

      /// Merge a new element at the performance-optimal position.            
      /// This usually means at the back of a contiguous container. Supports  
      /// intents and arrays.                                                 
      ///   @param a an element or array of elements (and its intent) to merge
      ///   @attention when 'a' is an array, you have to be careful with      
      ///      using MergeResult::lastInsertedIndex, as it will show only the 
      ///      position of the last insertion! Merge elements one-by-one, in  
      ///      order to get the proper offsets.                               
      ///   @return 1 if element was inserted, and the position where it was  
      ///      inserted (or found at, if it was already existing)             
      template<class A, CT::ContainsMany C> requires (not Shared)
      auto MergeInner(this C& self, A&& a) -> MergeResult {
         // Gather the number of all elements and types.                
         // Empty containers can't change type. If one of the type      
         // changes raises a conflict, this function will throw.        
         size_t rhs_count;
         if constexpr (CT::Array<A>) {
            using E = Decvq<Deref<DeextAll<Deint<A>>>>;
            self.template SetType<E>();
            rhs_count = GetAllExtentsOf(a);
         }
         else {
            using E = Decvq<Deref<Deint<A>>>;
            self.template SetType<E>();
            rhs_count = 1;
         }

         // Reallocate/branch out                                       
         const size_t lhs_count = self.GetCount();
         const size_t all_count = lhs_count + rhs_count;
         self.BranchOut(all_count);

         // Insert the new elements if they're not contained yet        
         MergeResult result;
         auto insert = [&]<class E>(E&& item) {
            if constexpr (CT::Contiguous<C>) {
               // Contiguous merge                                      
               const auto found = self.FindInner(DeintCast(item), 0);
               if (found) {
                  result.lastInsertedIndex = found - self.GetHandle();
                  return;
               }

               auto to = self.GetHandle() + lhs_count;
               if constexpr (CT::Copied<IntentOf(item)>)
                  to.EmplaceWithIntent(Refer(LglsFwd(item)));
               else
                  to.EmplaceWithIntent(FWDIntent(item));
            }
            else {
               // Hash table merge                                      
               const auto bucket = self.GetOffset(DeintCast(item));
               const auto found = self.FindInner(DeintCast(item), bucket);
               if (found) {
                  result.lastInsertedIndex = found - self.GetHandle();
                  return;
               }

               // Move the element to a temporary local swapper first   
               THandle<Decvq<Deref<Deint<E>>>> swapper {Piecewise, LglsFwd(item)};
               result.lastInsertedIndex = self.TableEmplace(bucket, swapper);
            }

            ++result.itemsInserted;
         };

         try {
            //TODO actual array insertion
            insert(LglsFwd(a));
         }
         catch (...) {
            // Account for throws inside constructors                   
            self.SetCountInner(lhs_count + result.itemsInserted);
            throw;
         }

         self.SetCountInner(lhs_count + result.itemsInserted);
         return result;
      }

      /// Merge a pair at the performance-optimal position.                   
      /// This usually means at the back of a contiguous container.           
      ///   @param a pair of elements (and its intent) to merge               
      ///   @return 1 if element was inserted, and the position where it was  
      ///      inserted (or found at, if it was already existing)             
      template<class K, class V, CT::ContainsMany C> requires Shared
      auto MergeInner(this C& self, K&& key, V&& val) -> MergeResult {
         static_assert(not CT::Array<K, V>);
         static_assert(not CT::Contiguous<C>);

         if constexpr (CT::Handle<K>)
            self.template SetType<0>(DeintCast(key).GetType());
         else
            self.template SetType<Decvq<Deref<Deint<K>>>, 0>();

         if constexpr (CT::Handle<V>)
            self.template SetType<1>(DeintCast(val).GetType());
         else
            self.template SetType<Decvq<Deref<Deint<V>>>, 1>();

         // If this is reached, then types are the same                 
         // Reallocate/branch out                                       
         const size_t lhs_count = self.GetCount();
         const size_t all_count = lhs_count + 1;
         self.BranchOut(all_count);

         // Insert the new elements if they're not contained yet        
         MergeResult result;
         auto insert = [&self,&result](K&& k, V&& v) {
            // Hash table merge only                                    
            const auto bucket = self.GetOffset(DeintCast(k));
            const auto found = self.FindInner(DeintCast(k), bucket);
            if (found) {
               result.lastInsertedIndex = found - self.GetHandle();
               return;
            }

            // Make a local pair to use as a swapper                    
            TPair swapper {LglsFwd(k), LglsFwd(v)}; //TODO Copy maybe?
            result.lastInsertedIndex = self.TableEmplace(bucket, swapper);
            ++result.itemsInserted;
         };

         try {
            insert(LglsFwd(key), LglsFwd(val));
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
