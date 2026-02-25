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
      template<bool FORCE = true, class A1, class...AN, CT::IndexedLinearly C>
      auto MergeAt(this C&, CT::Index auto, A1&&, AN&&...)
         -> Count<C> requires CT::RangeInsertable<C, A1, AN...>;

      template<bool FORCE = true, CT::IndexedLinearly C>
      auto MergeRangeAt(this C&, CT::Index auto, CT::Container auto&&)
         -> Count<C>;

      /// Merge one or more elements at the performance-optimal position.     
      /// This usually means at the back of a contiguous container. Supports  
      /// intents and arrays.                                                 
      ///   @tparam FORCE if true, the container is allowed to deepen in      
      ///      order to incorporate elements of different types. Otherwise    
      ///      a compile-time or runtime exception will be thrown, if an      
      ///      incompatible type is encountered.                              
      ///   @param a1, an elements (and their intents) to merge               
      ///   @return the number of merged elements                             
      template<bool FORCE = true, class A1, class...AN, CT::ContainsMany C>
      auto Merge(this C& self, A1&& a1, AN&&...an) -> Count<C> {
         // Gather the number of all elements and types.                
         // Empty containers can't change type. If one of the type      
         // changes raises a conflict, this function will throw.        
         bool deepened = false;
         Count<C> rhs_count = 0;
          self.PrepareForInsertion(LglsFwd(a1), rhs_count, deepened);
         (self.PrepareForInsertion(LglsFwd(an), rhs_count, deepened), ...);
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
            if (self.Contains(a))
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

      template<bool FORCE = true, CT::Container C>
      auto MergeRange(this C&, CT::Container auto&&)
         -> Count<C>;
   };
}
