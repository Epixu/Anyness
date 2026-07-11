///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Component.hpp"
#include "Langulus/CT/Index.hpp"
#include "Langulus/IntentOf.hpp"
#include "Langulus/Typenav.hpp"
#include <cstddef>
#include <vector>


namespace Langulus::Anyness::Component
{
   /// Refers back to this particular component instance through the deduced  
   /// 'this'. Just for convenience. It is #undef-ed at the end of this file. 
   #define ThisCom self.Merging<AS, ID, SHARED...>

   ///                                                                        
   /// Implements merging for containers.                                     
   /// Merging (unlike emplacement) extends the memory space and may move     
   /// things around. It guarantees that nothing gets overwritten.            
   /// Merging (unlike insertion) disallows for duplicated elements.          
   ///   @tparam AS type to serialize as before merging. Useful for byte      
   ///      and text containers. Use void to insert without serialization.    
   ///   @tparam ID, SHARED providers that share the same merge behavior.     
   template<class AS, Cid ID, Cid...SHARED>
   struct Merging {
      using CTTI_Component = Yes<>;
      using CTTI_ReflectAs = void;
      using Id             = Values<ID, SHARED...>;

      static constexpr int  ComponentPrecedence = 3000;
      static constexpr bool Shared = sizeof...(SHARED) > 0;

   private:
      template<CT::Container C>
      using Count = typename C::CountType;
      template<CT::Container C>
      using Deep = typename C::DeepType;

   public:
      /// MARK: MergeAt                                                       
      /// Insert one or more elements at the specified position only if the   
      /// elements don't exist elsewhere. Supports intents and arrays.        
      ///   @param idx the index to insert at                                 
      ///   @param a1, an elements or arrays (and their intents) to merge     
      ///   @return the number of inserted elements (after any conversions)   
      template<class A1, class...AN, CT::IndexedLinearly C>
      auto MergeAt(this C& self, CT::Index auto&& idx, A1&& a1, AN&&...an) -> size_t {
         static_assert(CT::ContainsMany<C>,
            "Container should support multiple elements");

         if constexpr (CT::NotVoid<AS> and not Same<TypeOf<AS>, Deint<A1>, Deint<AN>...>) {
            // Conversion to AS required.                               
            static_assert(Exact<C, AS>, "Serializing insertion type mismatch");
            const size_t initial_count = self.GetCountInner();
            size_t offset = self.SimplifyIndex(idx);
            // ConvertMergeInner uses MergeRangeAt, so any exceptions   
            // will be handled there                                    
            ThisCom::ConvertMergeInner(offset, LglsFwd(a1));
           (ThisCom::ConvertMergeInner(offset, LglsFwd(an)), ...);
            return self.GetCountInner() - initial_count;
         }
         else {
            // No conversion required.                                  
            // Check all types, and gather the number of insertions.    
            ::std::vector<size_t> reduced;
            ThisCom::PrepareForMerging(LglsFwd(a1), reduced);
           (ThisCom::PrepareForMerging(LglsFwd(an), reduced), ...);
            if (reduced.empty())
               return 0;
            
            const size_t lhs_count = self.GetCount();
            if (lhs_count == 0) {
               self.AssertZeroIndex(idx);

               if (not self.IsDisowned() and self.GetUses() == 1) {
                  // This is empty, but preallocated                    
                  TODO();
               }
               else {
                  // This is empty and unallocated                      
                  self.AllocateFresh(reduced.size());
                  auto to = self.GetHandle();
                  size_t const* it = reduced.data();
                  size_t counter = 0;

                  try {
                     MergeInnerLinear(to, it, counter, LglsFwd(a1));
                    (MergeInnerLinear(to, it, counter, LglsFwd(an)), ...);
                  }
                  catch (...) {
                     // Account for throws inside constructors          
                     const size_t inserted = to - self.GetHandle();
                     TODO(); //TODO a gap remains, move things back
                     self.SetCountInner(inserted);
                     throw;
                  }
               }

               self.SetCountInner(reduced.size());
               return reduced.size();  
            }

            // Reallocate/branch out                                    
            const size_t all_count = lhs_count + reduced.size();
            const size_t offset    = self.SimplifyIndex(idx);

            if (not self.IsDisowned() and self.GetUses() == 1) {
               // No need to branch-out                                 
               self.AllocateMore(all_count);
               auto to = self.GetHandle() + offset;
               MakeGap(to, offset, lhs_count, reduced.size());
               size_t const* it = reduced.data();
               size_t counter = 0;

               try {
                  MergeInnerLinear(to, it, counter, LglsFwd(a1));
                 (MergeInnerLinear(to, it, counter, LglsFwd(an)), ...);
               }
               catch (...) {
                  // Account for throws inside constructors             
                  const size_t inserted = to - self.GetHandle();
                  TODO(); //TODO a gap (of only one element!!!) remains, move things back
                  self.SetCountInner(inserted);
                  throw;
               }
            }
            else {
               // We need to branch-out: insert old and new elements    
               // in another container, which we will later swap.       
               C temp {Disown{self}};
               temp.Reserve(all_count);
               auto src = self.GetHandle();
               auto dst = temp.GetHandle();
               size_t const* it = reduced.data();
               size_t counter = 0;

               // Copy original before 'offset'                         
               CopyRegion(src, dst, offset);

               // Copy new elements in the gap                          
               try {
                  MergeInnerLinear(dst, it, counter, LglsFwd(a1));
                 (MergeInnerLinear(dst, it, counter, LglsFwd(an)), ...);
               }
               catch (...) {
                  // Account for throws inside constructors             
                  const size_t inserted = dst - self.GetHandle();
                  TODO(); //TODO a gap remains, move things back
                  self.SetCountInner(inserted);
                  throw;
               }

               // Copy original after 'offset'                          
               CopyRegion(src, dst, lhs_count - offset);

               // Swap                                                  
               self.Swap(temp);
            }

            self.SetCountInner(all_count);
            return reduced.size();
         }
      }

      template<CT::IndexedLinearly C>
      auto MergeRangeAt(this C&, CT::Index auto, CT::Container auto&&)
         -> Count<C>;

      /// MARK: Merge                                                         
      /// Insert one or more elements at the performance-optimal position only
      /// if the elements don't exist elsewhere. This usually means at the    
      /// back of a contiguous container. Supports intents and arrays.        
      ///   @param a1, an elements (and their intents) to insert              
      ///   @return the number of inserted elements (after any conversions)   
      template<class A1, class...AN, class C>
      auto Merge(this C& self, A1&& a1, AN&&...an) -> size_t {
         static_assert(CT::ContainsMany<C>,
            "Container should support multiple elements");

         if constexpr (CT::NotVoid<AS> and not Same<TypeOf<AS>, Deint<A1>, Deint<AN>...>) {
            // Conversion to AS required.                               
            static_assert(Exact<C, AS>, "Serializing insertion type mismatch");
            const size_t initial_count = self.GetCountInner();
            size_t offset = initial_count;
            // ConvertInsertInner uses ConcatAt, so any exceptions will 
            // be handled there                                         
            ThisCom::ConvertMergeInner(offset, LglsFwd(a1));
           (ThisCom::ConvertMergeInner(offset, LglsFwd(an)), ...);
            return offset - initial_count;
         }
         else {
            // No conversion required.                                  
            // Gather the number of all elements and types.             
            // Empty containers can't change type. If one of the type   
            // changes raises a conflict, this function will throw.     
            ::std::vector<size_t> reduced;
            size_t counter = 0;
            ThisCom::PrepareForMerging(LglsFwd(a1), counter, reduced);
           (ThisCom::PrepareForMerging(LglsFwd(an), counter, reduced), ...);
            if (reduced.empty())
               return 0;
            
            // Reallocate/branch out                                    
            const size_t lhs_count = self.GetCount();
            const size_t all_count = lhs_count + reduced.size();
            self.BranchOut(all_count);
            
            // Insert the new                                           
            size_t const* it = reduced.data();
            counter = 0;

            if constexpr (CT::IndexedLinearly<C>) {
               // Insert as a sequence at the back of the memory        
               auto to = self.GetHandle().ForceMutable() + lhs_count;
               try {
                  MergeInnerLinear(to, it, counter, LglsFwd(a1));
                 (MergeInnerLinear(to, it, counter, LglsFwd(an)), ...);
               }
               catch (...) {
                  // Account for throws inside constructors             
                  const ptrdiff_t inserted = it - reduced.data();
                  self.SetCountInner(inserted);
                  throw;
               }
            }
            else {
               // Insert in a hash table                                
               try {
                  ThisCom::MergeInnerTable(it, counter, LglsFwd(a1));
                 (ThisCom::MergeInnerTable(it, counter, LglsFwd(an)), ...);
               }
               catch (...) {
                  // Account for throws inside constructors             
                  const ptrdiff_t inserted = it - reduced.data();
                  self.SetCountInner(inserted);
                  throw;
               }
            }

            self.SetCountInner(all_count);
            return reduced.size();
         }
      }

      template<CT::Container C>
      auto MergeRange(this C&, CT::Container auto&&) -> Count<C>;
      
   protected:
      /// MARK: Protected                                                     
      /// Helper function that gathers the number of elements and types.      
      /// 'a' is checked if already exists either in 'self' or in 'a' itself  
      /// if array. Only non-duplicates will be merged.                       
      ///   @attention operates in all relevant dimensions simultaneously     
      template<class C, class A>
      void PrepareForMerging(this C& self, A&& a, size_t& counter, ::std::vector<size_t>& out_count) {
         if constexpr (CT::Handle<A>) {
            // Inserting handles                                        
            if (self.Contains(DeintCast(a))) {
               ++counter;
               return;
            }

            self.AbsorbType(Copy(a));
            out_count.emplace_back(counter);
            ++counter;
         }
         else if constexpr (CT::Array<A>) {
            // Inserting array                                          
            auto contained_in_array_itself = [&](auto& i) -> bool {
               const ptrdiff_t idx = &i - DeintCast(a);
               for (ptrdiff_t it = 0; it < idx; ++it) {
                  if (DeintCast(a)[it] == i)
                     return true;
               }
               return false;
            };

            for(auto& i : DeintCast(a)) {
               if (self.Contains(i) or contained_in_array_itself(i)) {
                  ++counter;
                  return;
               }
   
               self.DeduceType(i);
               out_count.emplace_back(counter);
               ++counter;
            }
         }
         else {
            // Inserting element                                        
            if (self.Contains(DeintCast(a))) {
               ++counter;
               return;
            }

            self.DeduceType(LglsFwd(a));
            out_count.emplace_back(counter);
            ++counter;
         }
      }
      
      /// Helper struct for returning insertion status                        
      /*struct MergeResult {
         size_t itemsInserted = 0;
         size_t lastInsertedIndex = 0;
      };*/

      /// Merge item at the performance-optimal position.                     
      /// This usually means at the back of a contiguous container.           
      ///   @attention all types need to be set prior to calling this function
      ///   @attention when inserting in a hash table, the item is used as a  
      ///      swapper, and has to be strongly owned from outside this call   
      ///   @param a pair of elements (and its intent) to merge               
      ///   @return 1 if element was inserted, and the position where it was  
      ///      inserted (or found at, if it was already existing)             
      /*template<class T, CT::ContainsMany C>
      auto MergeInner(this C& self, T&& item) -> MergeResult {
         static_assert(not CT::Array<T>,
            "This inner routine doesn't account for arrays");
         static_assert(CT::Handle<T> or not Shared,
            "Multidimensional merge should be done using a handle");

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
            else {
               // Table merge                                           
               size_t bucket;
               if constexpr (not Shared) {
                  // Single dimension                                   
                  bucket = self.GetOffset(DeintCast(item));
                  if (not self.IsEmpty()) {
                     if (const auto found = self.FindInner(DeintCast(item), bucket)) {
                        result.lastInsertedIndex = found - self.GetHandle();
                        return result;
                     }
                  }
               }
               else {
                  // Multidimensional                                   
                  const auto key = DeintCast(item).GetKeyHandle(); //TODO this presumes the key dimension is the one the hash table is associated with
                  bucket = self.GetOffset(key);
                  if (not self.IsEmpty()) {
                     if (const auto found = self.FindInner(key, bucket)) {
                        result.lastInsertedIndex = found - self.GetHandle();
                        return result;
                     }
                  }
               }
            
               if constexpr (CT::Copied<IntentOf(item)>)
                  result.lastInsertedIndex = self.TableEmplace(bucket, Refer(LglsFwd(item)));
               else
                  result.lastInsertedIndex = self.TableEmplace(bucket, FWDIntent(item));

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
      }*/
      
      /// MARK: ConvertMergeInner                                             
      template</**/ class C, class T>
      void ConvertMergeInner(this C& self, size_t& at, T&& a) {
         using I  = IntentOf(a);
         using IT = DeextAll<Deint<I>>;
         static_assert(CT::NotVoid<AS> and not Same<TypeOf<AS>, IT>,
            "Use MergeInnerLinear instead");
      
         AS converted;
         if constexpr (CT::Array<T>) {
            for (size_t i = 0; i < ExtentOf<T>; ++i)
               Langulus::Serialize(DeintCast(a)[i], converted);
         }
         else Langulus::Serialize(DeintCast(a), converted);

         const size_t offset = converted.GetCount();
         ThisCom::MergeRangeAt(at, Abandon {converted});
         at += offset;
      }

      /// MARK: MergeInnerLinear                                              
      /// A deeply unsafe function, that places 'a' at handle 'to'            
      /// and moves handle further. Supports T being a bounded array.         
      /// Does not perform conversion.                                        
      ///   @attention works in all dimensions at once                        
      template<CT::Handle H, class T>
      static void MergeInnerLinear(H& to, size_t const*& it, size_t& counter, T&& a) {
         using I  = IntentOf(a);
         using IT = DeextAll<Deint<T>>;
         static_assert(CT::Void<AS> or Same<TypeOf<AS>, IT>,
            "Use ConvertMergeInner instead");

         if constexpr (CT::Array<T>) {
            for (size_t i = 0; i < ExtentOf<T>; ++i) {
               if (*it == counter) {
                  Id::ForEach([&]<Cid D>{
                     if constexpr (CT::Copied<I>)
                        to.template EmplaceWithIntent<D>(Refer(DeintCast(a)[i]));
                     else
                        to.template EmplaceWithIntent<D>(I::Nest(DeintCast(a)[i]));
                  });

                  ++to;
                  ++it;   
               }
               
               ++counter;
            }
         }
         else {
            if (*it == counter) {
               Id::ForEach([&]<Cid D>{
                  if constexpr (CT::Copied<I>)
                     to.template EmplaceWithIntent<D>(Refer(LglsFwd(a)));
                  else
                     to.template EmplaceWithIntent<D>(FWDIntent(a));
               });

               ++to;
               ++it;   
            }
            
            ++counter;
         }
      }

      /// MARK: MergeInnerTable                                               
      /// Inserts 'a' into a hash map at the appropriate bucket. Supports T   
      /// being a bounded array. Does not perform conversion.                 
      ///   @attention works in all dimensions at once                        
      template<class T>
      void MergeInnerTable(this auto& self, size_t const*& it, size_t& counter, T&& a) {
         using I  = IntentOf(a);
         using IT = DeextAll<Deint<T>>;
         static_assert(CT::Void<AS> or Same<TypeOf<AS>, IT>,
            "Use ConvertMergeInnerTable instead"); //TODO function not implemented yet

         if constexpr (CT::Array<T>) {
            for (size_t i = 0; i < ExtentOf<T>; ++i) {
               if (*it == counter) {
                  decltype(auto) element = DeintCast(a)[i];
                  size_t bucket;

                  if constexpr (not Shared)
                     bucket = self.GetOffset(element);
                  else
                     bucket = self.GetOffset(element.GetKeyHandle()); //TODO this presumes the key dimension is the one the hash table is associated with

                  if constexpr (CT::Copied<I>)
                     self.TableEmplace(bucket, Refer(element));
                  else
                     self.TableEmplace(bucket, I::Nest(element));

                  ++it;
               }
               
               ++counter;
            }
         }
         else {
            if (*it == counter) {
               decltype(auto) element = DeintCast(a);
               size_t bucket;

               if constexpr (not Shared)
                  bucket = self.GetOffset(element);
               else
                  bucket = self.GetOffset(element.GetKeyHandle()); //TODO this presumes the key dimension is the one the hash table is associated with

               if constexpr (CT::Copied<I>)
                  self.TableEmplace(bucket, Refer(LglsFwd(a)));
               else
                  self.TableEmplace(bucket, FWDIntent(a));

               ++it;
            }
            
            ++counter;
         }
      }
   };

   #undef ThisCom
}
