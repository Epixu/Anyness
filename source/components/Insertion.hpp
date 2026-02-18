///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
#include <Langulus/CT/Unfold.hpp>
#include <Langulus/CT/Index.hpp>
#include <Langulus/CT/ReflectAs.hpp>
#include <Langulus/CT/Contiguous.hpp>
#include <Langulus/CT/Nullable.hpp>
#include <Langulus/CT/Defaultable.hpp>


namespace Langulus::CT
{
   /// Check if container's elements are unfold-constructible                 
   ///   @attention type-erased elements are always insertable, and will fail 
   ///      at runtime if not reflected as such                               
   template<class C, class...A>
   concept RangeInsertable = Container<C> and (
      Untyped<C> or UnfoldConstructible<TypeOf<C>, A...>
   );

   namespace Inner
   {
      /// Test whether a container is constructible with the given arguments  
      ///   @tparam C the contained type                                      
      ///   @tparam ...A the arguments to test                                
      ///   @return true if container is constructible using {A...}           
      template<Container C, class...A>
      consteval bool DeepConstructible() noexcept {
         using FA = FirstOf<A...>;
         using SA = IntentOfT<FA>;
         using T  = TypeOf<C>;

         if constexpr (Untyped<C>) {
            // Type-erased containers accept almost any type - they     
            // will report errors at runtime instead, if any            
            return Reflectable<Deint<A>...>;
         }
         else if constexpr (sizeof...(A) == 1 and Container<FA>) {
            // If only one A provided, it HAS to be a container         
            if constexpr (SA::IsShallow()) {
               // Generally, shallow intents are always supported,      
               // but copying will call element constructors, so we     
               // have to check if the contained type supports it       
               if constexpr (Copied<SA>)
                  return ReferConstructible<T>;
               else
                  return true;
            }
            else {
               // Cloning always calls decayed constructors, and        
               // we have to check whether decayed elements can do it   
               return IntentConstructible<Langulus::Clone, T>;
            }
         }
         else return UnfoldConstructible<T, A...>;
      };
   }

   /// Concept for recognizing arguments, with which a statically typed       
   /// container can be constructed                                           
   template<class C, class...A>
   concept DeepConstructible = Inner::DeepConstructible<C, A...>();
}

namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Implements insertion for containers.                                   
   /// Insertion (unlike emplacement) extents the memory space and may move   
   /// things around. It guarantees that nothing gets overwritten.            
   ///   @tparam ID heap we're inserting to                                   
   ///   @tparam AS type to serialize as before inserting. Useful for byte    
   ///      and text containers. Use void to insert without serialization.    
   template<unsigned ID, class AS>
   struct Insertion {
      using CTTI_Component = Yes<>;
      static constexpr int ComponentPrecedence = 3000;

   private:
      template<CT::Container C>
      using Count = typename Deref<C>::CountType;
      template<CT::Container C>
      using Deep  = typename Deref<C>::DeepType;
      template<CT::Container C>
      using State = typename Deref<C>::StateType;
      template<CT::Container C>
      using PickRangeMut = typename Deref<C>::PickRangeMut;

   public:
      /// Insert one or more elements at specific index                       
      template<bool FORCE = true, class A1, class...AN, CT::Contiguous C>
      auto InsertAt(this C&, CT::Index auto, A1&&, AN&&...)
         -> Count<C> requires CT::RangeInsertable<C, A1, AN...>;

      /// Insert a number of elements at a specific place, nullifying them if 
      /// able to                                                             
      template<CT::Contiguous C>
      auto InsertNulledAt(this C&, CT::Index auto, Count<C>)
         -> Count<C>;

      /// Insert a number of elements at a specific place, default-           
      /// constructing them if able to                                        
      template<CT::Contiguous C>
      auto InsertDefaultAt(this C&, CT::Index auto, Count<C>)
         -> Count<C>;

      template<bool CONCAT = true, bool FORCE = true, CT::Contiguous C>
      auto SmartPushAt(this C&, CT::Index auto, auto&&, State<C> = {})
         -> Count<C>;

      /// Insert one or more elements at the performance-optimal position.    
      /// This usually means at the back of a contiguous container. Supports  
      /// intents and arrays.                                                 
      ///   @tparam FORCE if true, the container is allowed to deepen in      
      ///      order to incorporate elements of different types. Otherwise    
      ///      a compile-time or runtime exception will be thrown, if an      
      ///      incompatible type is encountered.                              
      ///   @param a1, an elements (and their intents) to insert              
      ///   @return the number of inserted elements                           
      template<bool FORCE = true, class A1, class...AN, CT::Container C>
      auto Insert(this C& self, A1&& a1, AN&&...an) -> Count<C> {
         static_assert(CT::ContainsMany<C>,
            "Container should support multiple elements");

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
            const C backup {Abandon{self}};
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
         auto to = it.begin() + lhs_count;
         auto insert = [&to](auto&& a) {
            if constexpr (CT::Copied<IntentOf(a)>)
               to->EmplaceWithIntent(Refer(LglsFwd(a)));
            else
               to->EmplaceWithIntent(FWDIntent(a));
            ++to;
         };

         try {
             insert(LglsFwd(a1));
            (insert(LglsFwd(an)), ...);
         }
         catch (...) {
            // Account for throws inside constructors                   
            const Count<C> inserted = to - it.begin();
            self.SetCountInner(inserted);
            throw;
         }

         return rhs_count;
      }

      /// Insert a number of elements at the performance-optimal position.    
      /// This usually means at the back of a contiguous container. The       
      /// inserted elements will be nullified.                                
      ///   @param count the number of elements to insert                     
      template<CT::Container C>
      auto InsertNulled(this C&, Count<C> count) -> Count<C>;

      /// Insert a number of elements at the performance-optimal position.    
      /// This usually means at the back of a contiguous container. The       
      /// inserted elements will be default-constructed.                      
      ///   @param count the number of elements to insert                     
      template<CT::Container C>
      auto InsertDefault(this C& self, Count<C> count) -> Count<C> {
         const auto previousCount = self.GetCount();
         self.AllocateMore(previousCount + count);

         if constexpr (not C::TypeErased) {
            using T = TypeOf<C>;
            if constexpr (CT::Nullable<T>) {
               // Zero the dense memory (optimization)                  
               memset(self.GetRaw() + previousCount, 0, count * sizeof(T));
            }
            else if constexpr (CT::Defaultable<T>) {
               // Construct requested elements one by one               
               auto to = self.GetRaw() + previousCount;
               const auto toEnd = to + count;
               try {
                  while (to != toEnd) {
                     new (to) T {};
                     ++to;
                  }
               } catch (...) {
                  // Partial success                                    
                  const auto constructed = to - self.GetRaw();
                  self.SetCountInner(previousCount + constructed);
                  throw;
               }
            }
            else static_assert(false,
               "Trying to default-construct elements that are "
               "incapable of default-construction/nullification"
            );
         }
         else {
            const auto T = self.GetType();
            if (T.IsNullable()) {
               // Zero the dense memory (optimization)                  
               const auto stride = T.GetSize();
               memset(
                  self.template GetRawAs<uint8_t>() + previousCount * stride,
                  0,
                  count * stride
               );
            }
            else {
               const auto defaultConstructor = T.GetDefaultConstructor();
               LglsAssert(defaultConstructor,
                  "Can't default-construct elements"
                  " - no default constructor/nullification reflected"
               );

               // Construct requested elements one by one               
               const auto stride = T.GetSize();
               auto to = self.template GetRawAs<uint8_t>() + previousCount * stride;
               const auto toEnd = to + count * stride;
               try {
                  while (to != toEnd) {
                     defaultConstructor(to);
                     to += stride;
                  }
               } catch (...) {
                  // Partial success                                    
                  const auto constructed = (to - self.template GetRawAs<uint8_t>()) / stride;
                  self.SetCountInner(previousCount + constructed);
                  throw;
               }
            }
         }

         // Success                                                     
         self.SetCountInner(previousCount + count);
         return count;
      }

      template<bool CONCAT = true, bool FORCE = true, CT::Container C>
      auto SmartPush(this C&, auto&&, State<C> = {})
         -> Count<C>;

      template<bool TRANSFER_OR = true, CT::Container C>
      auto Deepen(this C&) -> Deep<C>&;

      /// Extend the container's memory and return the newly allocated range  
      ///   @attention if extending memory without ownership, the container   
      ///      will diverge into a new allocation and copy the data           
      ///   @param count the number of elements to extend by                  
      ///   @param arguments the arguments to use for each constructor call   
      ///      - no arguments will result in default construction             
      ///   @return the newly allocated mutable range                         
      /*template<CT::Container C, class...A>
      auto Extend(this C& self, Count<C> count = 1, A&&...arguments)
      -> Decay<C> { // PickRangeMut<C>
         const auto previousCount = self.GetCount();
         if constexpr (sizeof...(A) == 0)
            self.InsertDefault(count);
         else if (count == 1)
            self.InsertConstruct(LglsFwd(arguments)...);
         else {
            LglsAssert(
               ((not IntentOfT<decltype(arguments)>::IsMoved()) and ...),
               "Can't use move semantics here - "
               "the arguments need to be reused multiple times"
            );
            
            self.AllocateMore(previousCount + count);
            for (Count<C> i = 0; i < count; i++)
               self.InsertConstruct(LglsFwd(arguments)...); //TODO this is a pretty slow way to batch-insert, lots of overhead
         }
         return self.SelectInner(previousCount, count);
      }*/

      
      /// Concatenation at specific index                                     
      template<CT::Contiguous C>
      auto ConcatAt(this C& self, CT::Index auto index, CT::Container auto&& data) -> Count<C> {
         const auto rhs_count = DeintCast(data).GetCount();
         if (not rhs_count)
            return 0;

         using S = IntentOf(data);
         //using T = Tif<CT::TypeErased<C>, TypeOf<Deint<S>>, TypeOf<C>>;
         if constexpr (CT::TypeErased<C>)
            self.SetType(DeintCast(data).GetType());
         else
            self.template SetType<TypeOf<Deint<S>>>();

         self.BranchOut();
         const auto lhs_count = self.GetCount();
         const auto idx = self.template SimplifyIndex<false>(index);
         self.AllocateMore(lhs_count + rhs_count);

         if (idx < lhs_count) {
            // We're moving to the right, so make sure we do it in      
            // reverse to avoid any potential overlap                   
            const auto moved = lhs_count - idx;
            self.SelectInner(idx + rhs_count, moved).template CreateWithIntent<true>(
               Abandon(self.SelectInner(idx, moved)));
         }

         // Construct data in place                                     
         self.SelectInner(idx, rhs_count).CreateWithIntent(LglsFwd(data));
         self.SetCount(lhs_count + rhs_count);
         return rhs_count;
      }

      /// Concatenation at the back. Unlike insertion, concatenation always   
      /// inserts the contents of the argument containers one by one.         
      ///   @param a1_intent, an_intent the containers to concatenate to the  
      ///      right of 'this'                                                
      ///   @return the number of concatenated elements                       
      template<CT::Container C, CT::Container A1, CT::Container...AN>
      auto Concat(this C& self, A1&& a1_intent, AN&&...an_intent) -> Count<C> {
         static_assert(CT::ContainsMany<C>,
            "Container should support multiple elements");

         // Gather the number of all elements and types.                
         // Empty containers can't change type. If one of the type      
         // changes raises a conflict, this function will throw.        
         Count<C> rhs_count = 0;
          self.PrepareForAbsorption(LglsFwd(a1_intent), rhs_count);
         (self.PrepareForAbsorption(LglsFwd(an_intent), rhs_count), ...);
         if (not rhs_count)
            return 0;
         
         const Count<C> lhs_count = self.GetCount();
         const Count<C> all_count = lhs_count + rhs_count;
         auto it = IterateHandles(self);

         if (self.GetUses() > 1) {
            // We have to branch out                                    
            const C backup {Abandon{self}};
            self.AllocateFresh(self.RequestHeap(all_count));
            // Set count immediately, so that iterators are valid.      
            self.SetCountInner(all_count);

            // Reinsert the old items.                                  
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
         auto to = it.begin() + lhs_count;
         auto insert = [&to](auto&& a) {
            auto item = IterateHandles(DeintCast(a)).begin();
            while (item) {
               to->EmplaceWithIntent(IntentOf(a)::Nest(*item));
               ++item; ++to;
            }
         };

         try {
             insert(LglsFwd(a1_intent));
            (insert(LglsFwd(an_intent)), ...);
         }
         catch (...) {
            // Account for throws inside constructors                   
            const Count<C> inserted = to - it.begin();
            self.SetCountInner(inserted);
            throw;
         }

         return rhs_count;
      }

   protected:
      /// Helper function that gathers the number of elements and types.      
      /// An incompatible type will result in 'deepened' being true, and      
      /// 'out_count' being rewritten to reflect the number of required sub-  
      /// containers.                                                         
      template<CT::Container C, class A>
      void PrepareForInsertion(this C& self, A&& a, Count<C>& out_count, bool& deepened) {
         using S = IntentOf(a);

         if constexpr (CT::Array<A>) {
            using E = Decvq<Deref<DeextAll<Deint<S>>>>;
            if (not deepened) {
               try {
                  self.template SetType<E>();
               } catch(...) {
                  deepened = true;
                  out_count = 2;
                  return;
               }
               out_count += GetAllExtentsOf(a);
            }
            else ++out_count;
         }
         else {
            using E = Decvq<Deref<Deint<S>>>;
            self.template SetType<E>();
            if (not deepened) {
               try {
                  self.template SetType<E>();
               } catch(...) {
                  deepened = true;
                  out_count = 2;
                  return;
               }
               ++out_count;
            }
            else ++out_count;
         }
      }

      /// Helper function that gathers the number of elements and types.      
      /// Empty containers can't change this container's type. If one of the  
      /// type changes raises a conflict the function will throw.             
      template<CT::Container C, CT::Container A>
      void PrepareForAbsorption(this C& self, A&& a, Count<C>& out_count) {
         const auto c = DeintCast(a).GetCount();
         if (not c)
            return;

         using S = IntentOf(a);
         using E = TypeOf<Deint<S>>;
         if constexpr (CT::NotVoid<E>)
            self.template SetType<E>();
         else
            self.SetType(DeintCast(a).GetType());
         out_count += c;
      }
   };
}
