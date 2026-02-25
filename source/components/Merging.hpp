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
   template<unsigned ID, class AS>
   struct Merging {
   private:
      template<CT::Container C>
      using Count = typename C::CountType;
      template<CT::Container C>
      using Deep = typename C::DeepType;

   public:
      using CTTI_Component = Yes<>;
      static constexpr int ComponentPrecedence = 3000;

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
      ///   @param a1, an elements (and their intents) to merge               
      ///   @return the number of merged elements                             
      template<class A1, class...AN, CT::ContainsMany C>
      auto Merge(this C& self, A1&& a1, AN&&...an) -> Count<C> {
         // Gather the number of all elements and types.                
         // Empty containers can't change type. If one of the type      
         // changes raises a conflict, this function will throw.        
         Count<C> rhs_count = 0;
          self.PrepareForMerge(LglsFwd(a1), rhs_count);
         (self.PrepareForMerge(LglsFwd(an), rhs_count), ...);
         if (not rhs_count)
            return 0;

         const Count<C> lhs_count = self.GetCount();
         const Count<C> all_count = lhs_count + rhs_count;
         auto it = IterateHandles(self);

         if (self.GetUses() > 1) {
            // We have to branch out                                    
            const C backup{Abandon{self}};
            self.AllocateFresh(self.RequestHeap(all_count));
            // Set count immediately, so that iterators are valid.      
            self.SetCountInner(all_count);

            // Reinsert the old items                                   
            auto old = IterateHandles(backup).begin();
            auto to = it.begin();
            while (old) {
               to->EmplaceWithIntent(Refer(*old));
               ++old; ++to;
            }
         }
         else {
            self.AllocateMore(all_count);
            // Set count immediately, so that iterators are valid.      
            self.SetCountInner(all_count);
         }

         // Insert the new.                                             
         Count<C> inserted = 0;
         auto insert = [&](auto&& a) {
            if (self.Contains(DeintCast(a)))
               return;

            auto to = it.begin() + lhs_count;
            if constexpr (CT::Copied<IntentOf(a)>)
               to->EmplaceWithIntent(Refer(LglsFwd(a)));
            else
               to->EmplaceWithIntent(FWDIntent(a));
            ++to;
            ++inserted;
         };

         try {
             insert(LglsFwd(a1));
            (insert(LglsFwd(an)), ...);
         }
         catch (...) {
            // Account for throws inside constructors                   
            self.SetCountInner(lhs_count + inserted);
            throw;
         }

         return inserted;
      }

      template<CT::Container C>
      auto MergeRange(this C&, CT::Container auto&&)
         -> Count<C>;
      
   protected:
      /// Helper function that gathers the number of elements and types.      
      /// An incompatible type will result in a throw.                        
      template<CT::Container C, class A>
      void PrepareForMerge(this C& self, A&& a, Count<C>& out_count) {
         if constexpr (CT::Array<A>) {
            using E = Decvq<Deref<DeextAll<Deint<A>>>>;
            self.template SetType<E>();
            out_count += GetAllExtentsOf(a);
         }
         else {
            using E = Decvq<Deref<Deint<A>>>;
            self.template SetType<E>();
            ++out_count;
         }
      }
   };
}
