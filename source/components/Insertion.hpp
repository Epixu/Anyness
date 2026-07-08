///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Component.hpp"
#include "Langulus/Assume.hpp"
#include "Langulus/IntentOf.hpp"
#include "Langulus/Utils/Types.hpp"
#include "source/Container.hpp"
#include <Langulus/CT/Unfold.hpp>
#include <Langulus/CT/Index.hpp>
#include <Langulus/CT/ReflectAs.hpp>
#include <Langulus/CT/Contiguous.hpp>
#include <Langulus/CT/Nullable.hpp>
#include <Langulus/CT/Defaultable.hpp>
#include <Langulus/CT/Serializer.hpp>
#include <Langulus/CT/Deep.hpp>


/*namespace Langulus::CT
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
}*/

namespace Langulus::Anyness::Component
{
   /// Refers back to this particular component instance through the deduced  
   /// 'this'. Just for convenience. It is #undef-ed at the end of this file. 
   #define ThisCom self.Insertion<AS, ID, SHARED...>

   ///                                                                        
   /// Implements insertion for containers.                                   
   /// Insertion (unlike emplacement) extends the memory space and may move   
   /// things around. It guarantees that nothing gets overwritten.            
   /// Insertion (unlike merging) allows for duplicated elements. That's why  
   /// this component is omitted for containers like Map and Set.             
   /// Supports five kinds of insertion:                                      
   ///   1. Insert/InsertAt - conventional insertion, where the container is  
   ///      expanded to incorporate the new elements at the desired position. 
   ///   2. Concat/ConcatAt - concatenation that inserts the contents of      
   ///      any container at the desired position, dsiregarding any state     
   ///      differences.                                                      
   ///   3. Compose/ComposeAt - a structure-preserving insertion that respects
   ///      states and disownment in order to form more complex containers.   
   ///   4. And/AndAt - an or-state-preserving insertion that deepens the     
   ///      container if `IsOr() == true`, and then inserts the new content.  
   ///   5. Or/OrAt - an or-state-preserving insertion that deepens the       
   ///      container if `IsOr() == false`, and then inserts the new content. 
   ///   @tparam AS type to serialize as before inserting. Useful for byte    
   ///      and text containers. Use void to insert without serialization.    
   ///   @tparam ID, SHARED providers that share the same insertion behavior. 
   template<class AS, Cid ID, Cid...SHARED>
   struct Insertion {
      using CTTI_Component = Yes<>;
      using CTTI_ReflectAs = void;
      using Id             = Values<ID, SHARED...>;

      static constexpr int ComponentPrecedence = 3000;

   private:
      //template<CT::Container C>
      //using Count = typename Deref<C>::CountType;
      template<CT::Container C>
      using Deep  = typename Deref<C>::DeepType;
      template<CT::Container C>
      using State = typename Deref<C>::StateType;
      template<CT::Container C>
      using PickRangeMut = typename Deref<C>::PickRangeMut;

   public:
      /// MARK: InsertAt                                                      
      /// Insert one or more elements at the specified position. Supports     
      /// intents and arrays.                                                 
      ///   @tparam FORCE if true, the container is allowed to deepen in      
      ///      order to incorporate elements of different types. Otherwise    
      ///      a compile-time or runtime exception will be thrown, if an      
      ///      incompatible type is encountered.                              
      ///   @attention If FORCE is enabled, and the index is somewhere in     
      ///      the middle of the container, it will be split in more than 2   
      ///      deep containers, so that insertion order is preserved.         
      ///   @param idx the index to insert at                                 
      ///   @param a1, an elements or arrays (and their intents) to insert    
      ///   @return the number of inserted elements (after any conversions)   
      template<bool FORCE = true, class A1, class...AN, CT::Contiguous C>
      auto InsertAt(this C& self, CT::Index auto&& idx, A1&& a1, AN&&...an) -> size_t {
         static_assert(CT::ContainsMany<C>,
            "Container should support multiple elements");

         if constexpr (CT::NotVoid<AS> and not Same<TypeOf<AS>, Deint<A1>, Deint<AN>...>) {
            // Conversion to AS required.                               
            static_assert(Exact<C, AS>, "Serializing insertion type mismatch");
            const size_t initial_count = self.GetCountInner();
            size_t offset = self.SimplifyIndex(idx);
            // ConvertInsertInner uses ConcatAt, so any exceptions will 
            // be handled there                                         
            ThisCom::ConvertInsertInner(offset, LglsFwd(a1));
           (ThisCom::ConvertInsertInner(offset, LglsFwd(an)), ...);
            return self.GetCountInner() - initial_count;
         }
         else {
            // No conversion required.                                  
            // Gather the number of all elements and types.             
            // Empty containers can't change type. If one of the type   
            // changes raises a conflict, this function will throw.     
            bool deepened = false;
            size_t rhs_count = 0;
            ThisCom::PrepareForInsertion(LglsFwd(a1), rhs_count, deepened);
           (ThisCom::PrepareForInsertion(LglsFwd(an), rhs_count, deepened), ...);
            if (not rhs_count)
               return 0;
            
            // Reallocate/branch out                                    
            const size_t lhs_count = self.GetCount();
            const size_t all_count = lhs_count + rhs_count;
            const size_t offset    = self.SimplifyIndex(idx);

            if (not self.IsDisowned() and self.GetUses() == 1 and not deepened) {
               // No need to branch-out                                 
               ThisCom::AllocateMore(all_count);

            }

            self.BranchOut(all_count); //TODO when branching out, reinsert in a new container with the gap predefined, iinstead of always moving elements. See Erase for reference

            TODO(); //TODO form a gap
            
            // Insert the new                                           
            auto to = self.GetHandle() + offset;

            try {
               ThisCom::InsertInner(to, LglsFwd(a1));
              (ThisCom::InsertInner(to, LglsFwd(an)), ...);
            }
            catch (...) {
               // Account for throws inside constructors                
               const size_t inserted = to - self.GetHandle();
               TODO(); //TODO a gap remains, move things back
               self.SetCountInner(inserted);
               throw;
            }

            self.SetCountInner(all_count);
            return rhs_count;
         }
      }

      /// Insert a number of elements at a specific place, nullifying them if 
      /// able to                                                             
      template<CT::Contiguous C>
      auto InsertNulledAt(this C&, CT::Index auto, size_t) -> size_t;

      /// Insert a number of elements at a specific place, default-           
      /// constructing them if able to                                        
      template<CT::Contiguous C>
      auto InsertDefaultAt(this C&, CT::Index auto, size_t) -> size_t;

      /// MARK: ComposeAt                                                     
      /// Structure-preserving insertion that uses the best approach in order 
      /// to keep current hierarchy and states, but also reuse memory.        
      ///   @attention any disowned data will remain disowned                 
      ///   @tparam CONCAT whether or not concatenation is allowed            
      ///   @tparam FORCE insert even if types mismatch by deepening          
      ///   @param index the index at which to insert (if needed)             
      ///   @param value the value to smart-push                              
      ///   @param state a state to apply after pushing is done               
      ///   @return the number of pushed items (zero if unsuccessful)         
      template<bool CONCAT = true, bool FORCE = true, CT::Contiguous C>
      auto ComposeAt(
         this C& self, CT::Index auto&& idx, auto&& value, State<C> state = {}
      ) -> size_t {
         using I = IntentOf(value);
         using T = Deint<I>;
   
         if constexpr (CT::Deep<T>) {
            // We're inserting a deep item, so we can do various smart  
            // things before inserting, like absorbing and concatenating
            if (not DeintCast(value).IsValid())
               return 0;
   
            const bool stateCompliant = self.CanFitState(DeintCast(value));
            if (self.IsEmpty() and stateCompliant) {
               // We can directly absorb                                
               self.Free();
               self.Absorb(LglsFwd(value));
               return 1;
            }
   
            if constexpr (CONCAT) {
               // Let's try concatenating.                              
               // If FORCE is enabled, this will deepen in order to     
               // preserve disowned data instead of copying it.         
               if (ThisCom::SmartConcatAt<FORCE>(LglsFwd(idx), stateCompliant, LglsFwd(value), state))
                  return 1;
            }
         }
   
         // If reached, then none of the above succeeded - just push.   
         // If FORCE is enabled, this will deepen in order to preserve  
         // disowned data instead of copying it.                        
         return ThisCom::ComposeAtInner<FORCE>(LglsFwd(idx), LglsFwd(value), state);
      }

      /// MARK: Insert                                                        
      /// Insert one or more elements at the performance-optimal position.    
      /// This usually means at the back of a contiguous container. Supports  
      /// intents and arrays.                                                 
      ///   @tparam FORCE if true, the container is allowed to deepen in      
      ///      order to incorporate elements of different types. Otherwise    
      ///      a compile-time or runtime exception will be thrown, if an      
      ///      incompatible type is encountered.                              
      ///   @param a1, an elements (and their intents) to insert              
      ///   @return the number of inserted elements (after any conversions)   
      template<bool FORCE = true, class A1, class...AN, CT::Contiguous C>
      auto Insert(this C& self, A1&& a1, AN&&...an) -> size_t {
         static_assert(CT::ContainsMany<C>,
            "Container should support multiple elements");

         if constexpr (CT::NotVoid<AS> and not Same<TypeOf<AS>, Deint<A1>, Deint<AN>...>) {
            // Conversion to AS required.                               
            static_assert(Exact<C, AS>, "Serializing insertion type mismatch");
            const size_t initial_count = self.GetCountInner();
            size_t offset = initial_count;
            // ConvertInsertInner uses ConcatAt, so any exceptions will 
            // be handled there                                         
            ThisCom::ConvertInsertInner(offset, LglsFwd(a1));
           (ThisCom::ConvertInsertInner(offset, LglsFwd(an)), ...);
            return offset - initial_count;
         }
         else {
            // No conversion required.                                  
            // Gather the number of all elements and types.             
            // Empty containers can't change type. If one of the type   
            // changes raises a conflict, this function will throw.     
            bool deepened = false;
            size_t rhs_count = 0;
            ThisCom::PrepareForInsertion(LglsFwd(a1), rhs_count, deepened);
           (ThisCom::PrepareForInsertion(LglsFwd(an), rhs_count, deepened), ...);
            if (not rhs_count)
               return 0;
            
            // Reallocate/branch out                                    
            const size_t lhs_count = self.GetCount();
            const size_t all_count = lhs_count + rhs_count;
            self.BranchOut(all_count);
            
            // Insert the new                                           
            auto to = self.GetHandle() + lhs_count;
            try {
               ThisCom::InsertInner(to, LglsFwd(a1));
              (ThisCom::InsertInner(to, LglsFwd(an)), ...);
            }
            catch (...) {
               // Account for throws inside constructors                
               const size_t inserted = to - self.GetHandle();
               self.SetCountInner(inserted);
               throw;
            }

            self.SetCountInner(all_count);
            return rhs_count;
         }
      }

      /// Insert a number of elements at the performance-optimal position.    
      /// This usually means at the back of a contiguous container. The       
      /// inserted elements will be nullified.                                
      ///   @param count the number of elements to insert                     
      template<CT::Contiguous C>
      auto InsertNulled(this C&, size_t count) -> size_t;

      /// MARK: InsertDefault                                                 
      /// Insert a number of elements at the performance-optimal position.    
      /// This usually means at the back of a contiguous container. The       
      /// inserted elements will be default-constructed.                      
      ///   @param count the number of elements to insert                     
      template<CT::Contiguous C>
      auto InsertDefault(this C& self, size_t count) -> size_t {
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
                  0, count * stride
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

         // Complete success                                            
         self.SetCountInner(previousCount + count);
         return count;
      }

      /// MARK: Compose                                                       
      /// Structure-preserving insertion that uses the best approach in order 
      /// to keep current hierarchy and states, but also reuse memory as much 
      /// as possible.                                                        
      ///   @attention any disowned data will remain disowned                 
      ///   @tparam CONCAT whether or not concatenation is allowed            
      ///   @tparam FORCE insert even if types mismatch by deepening          
      ///   @param value the value to insert                                  
      ///   @return the number of pushed items (zero if unsuccessful)         
      template<bool CONCAT = true, bool FORCE = true, CT::Contiguous C>
      auto Compose(this C& self, auto&& value) -> size_t {
         using I = IntentOf(value);
         using T = Deint<I>;
         auto& other = DeintCast(value);
         const bool stateCompliant = ThisCom::CheckState(other);
   
         if constexpr (CT::DeepDense<T>) {
            // We're inserting a deep item, so we can do various smart  
            // things before inserting, like absorbing and concatenating
            if (not other.IsValid())
               return 0;
   
            if (stateCompliant) {
               if (self.IsEmpty()) {
                  // We can directly absorb                             
                  self.AssignAbsorb(LglsFwd(value));
                  return 1;
               }
      
               if constexpr (CONCAT) {
                  // We are allowed to attempt concatenatenation        
                  if (ThisCom::Concat(LglsFwd(value)))
                     return 1;
               }
            }
         }
   
         // If reached, then none of the above succeeded - just push.   
         // If FORCE is enabled, this will deepen in order to preserve  
         // state, in the case there's conflict.                        
         return ThisCom::template ComposeInner<FORCE>(
            stateCompliant, Index::Back, LglsFwd(value)
         );
      }

      /// MARK: Deepen                                                        
      /// Wrap all contained elements inside a sub-block                      
      ///   @return a reference to the newly created inner container          
      template<CT::Deep C>
      auto Deepen(this C& self) -> C& {
         LglsAssert(not self.IsTypeConstrained() or self.template IsSame<C>(),
            "Can't deepen with incompatible type");
      
         // Allocate a new container and move this one inside of it     
         C temp {Piecewise, Abandon {self}};
         self.Swap(temp);
         return *self.template Get<C>();
      }

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

      
      /// MARK: ConcatAt                                                      
      /// Concatenation at specific index.                                    
      /// Possible only for contiguous containers with multiple elements.     
      template<CT::Contiguous C>
      auto ConcatAt(this C& self, CT::Index auto&& idx, CT::Container auto&& data) -> size_t {
         static_assert(CT::ContainsMany<C>,
            "Container should support multiple elements");

         const size_t rhs_count = DeintCast(data).GetCount();
         if (not rhs_count)
            return 0;

         self.AbsorbType(FWDIntent(data));

         // Reallocate/branch out                                       
         const size_t lhs_count = self.GetCount();
         const size_t all_count = lhs_count + rhs_count;
         self.BranchOut(all_count);

         const size_t offset = self.SimplifyIndex(idx);
         auto to = self.GetHandle() + offset;
         ThisCom::MakeGap(to, offset, lhs_count, all_count);   //TODO catch user data exceptions on moves, fatal failure - reset contents
         ThisCom::CopyRegion(to, rhs_count, LglsFwd(data));    //TODO catch user data exceptions on construction, partial success allowed
         self.SetCountInner(all_count);
         return rhs_count;
      }

      /// MARK: Concat                                                        
      /// Concatenation at the back. Unlike insertion, concatenation always   
      /// inserts the contents of the argument containers one by one.         
      /// Possible only for contiguous containers with multiple elements.     
      ///   @param a1_intent, an_intent the containers to concatenate to the  
      ///      right of 'this'                                                
      ///   @return the number of concatenated elements                       
      template<CT::Contiguous C, CT::Container A1, CT::Container...AN>
      auto Concat(this C& self, A1&& a1_intent, AN&&...an_intent) -> size_t {
         static_assert(CT::ContainsMany<C>,
            "Container should support multiple elements");

         // Gather the number of all elements and types.                
         // Empty containers can't change type. If one of the type      
         // changes raises a conflict, this function will throw.        
         size_t rhs_count = 0;
          self.PrepareForAbsorption(LglsFwd(a1_intent), rhs_count);
         (self.PrepareForAbsorption(LglsFwd(an_intent), rhs_count), ...);
         if (not rhs_count)
            return 0;
         
         // Reallocate/branch out                                       
         const size_t lhs_count = self.GetCount();
         const size_t all_count = lhs_count + rhs_count;
         self.BranchOut(all_count);
         
         // Concatenate all new containers                              
         auto to = self.GetHandle() + lhs_count;
         try {
            ThisCom::CopyRegion(to, DeintCast(a1_intent).GetCount(), LglsFwd(a1_intent));
           (ThisCom::CopyRegion(to, DeintCast(an_intent).GetCount(), LglsFwd(an_intent)), ...);
         }
         catch (...) {
            // Account for throws inside constructors                   
            const size_t inserted = to - self.GetHandle();
            self.SetCountInner(inserted);
            throw;
         }

         self.SetCountInner(all_count);
         return rhs_count;
      }

   protected:
      /// MARK: Protected                                                     
      /// Helper function that gathers the number of elements and types.      
      /// An incompatible type will result in 'deepened' being true, and      
      /// 'out_count' being rewritten to reflect the number of required sub-  
      /// containers.                                                         
      ///   @attention operates in all relevant dimensions simultaneously     
      template<CT::Contiguous C, class A>
      void PrepareForInsertion(this C& self, A&& a, size_t& out_count, bool& deepened) {
         if constexpr (CT::Handle<A>) {
            // Inserting handles                                        
            if (not deepened) {
               try {
                  self.AbsorbType(Copy(a));
               } catch(...) {
                  deepened = true;
                  out_count = 2;
                  return;
               }
               ++out_count;
            }
            else ++out_count;
         }
         else {
            // Inserting element or an array                            
            if (not deepened) {
               try {
                  self.DeduceType(LglsFwd(a));
               } catch(...) {
                  deepened = true;
                  out_count = 2;
                  return;
               }
               out_count += GetAllExtentsOf(a);
            }
            else ++out_count;
         }
      }

      /// Helper function that gathers the number of elements and types.      
      /// Empty containers can't change this container's type. If one of the  
      /// type changes raises a conflict the function will throw.             
      ///   @attention operates in all relevant dimensions simultaneously     
      template<CT::Contiguous C, CT::Container A>
      void PrepareForAbsorption(this C& self, A&& a, size_t& out_count) {
         const auto c = DeintCast(a).GetCount();
         if (not c)
            return;

         self.AbsorbType(Copy(a));
         out_count += c;
      }

      /// MARK: ConvertInsertInner                                            
      template<CT::Contiguous C, class T>
      void ConvertInsertInner(this C& self, size_t& at, T&& a) {
         using I  = IntentOf(a);
         using IT = DeextAll<Deint<I>>;
         static_assert(CT::NotVoid<AS> and not Same<TypeOf<AS>, IT>,
            "Use InsertInner instead");
      
         AS converted;
         if constexpr (CT::Array<T>) {
            for (size_t i = 0; i < ExtentOf<T>; ++i)
               Langulus::Serialize(DeintCast(a)[i], converted);
         }
         else Langulus::Serialize(DeintCast(a), converted);

         const size_t offset = converted.GetCount();
         ThisCom::ConcatAt(at, Abandon {converted});
         at += offset;
      }

      /// MARK: InsertInner                                                   
      /// A deeply unsafe function, that places 'a' at handle 'to'            
      /// and moves handle further. Supports T being a bounded array.         
      /// Does not perform conversion.                                        
      ///   @attention works in all dimensions at once                        
      template<CT::Handle H, class T>
      static void InsertInner(H& to, T&& a) {
         using I  = IntentOf(a);
         using IT = DeextAll<Deint<T>>;
         static_assert(CT::Void<AS> or Same<TypeOf<AS>, IT>,
            "Use ConvertInsertInner instead");

         // Non-converting insertion                                    
         if constexpr (CT::Array<T>) {
            for (size_t i = 0; i < ExtentOf<T>; ++i) {
               Id::ForEach([&]<Cid D>{
                  if constexpr (CT::Copied<I>)
                     to.template EmplaceWithIntent<D>(Refer(DeintCast(a)[i]));
                  else
                     to.template EmplaceWithIntent<D>(I::Nest(DeintCast(a)[i]));
               });
               
               ++to;
            }
         }
         else {
            Id::ForEach([&]<Cid D>{
               if constexpr (CT::Copied<I>)
                  to.template EmplaceWithIntent<D>(Refer(LglsFwd(a)));
               else
                  to.template EmplaceWithIntent<D>(FWDIntent(a));
            });

            ++to;
         }
      }
      
      /// MARK: CheckState                                                    
      /// Check state compatibility for composition. Rules are:               
      /// 1. An invalid container is compatible with any other                
      /// 2. If one or more of the containers is disowned, they are always    
      ///    incompatible.                                                    
      /// 3. Missing containers are not compatible with non-missing ones.     
      ///    If both containers are missing, they are compatible only if they 
      ///    both are either past or future at the same time.                 
      /// 4. In order to be compatible, a container has to be either capable  
      ///    of being deepened, or matching the other type exactly.           
      /// 5. Containers of different or-ness are only compatible if one of    
      ///    the containers has count <= 1. That's the one case when or-ness  
      ///    does not matter at all.                                          
      ///   @param other - the block to check                                 
      ///   @return true if state is compatible                               
      constexpr bool CheckState(this auto const& self, CT::Container auto const& other) noexcept {
         return not self.IsValid() or self.IsDisowned() or other.IsDisowned() or (
                (not self.IsTypeConstrained() or other.IsExact(self)) //TODO IsTypeConstrained is allowed as long as it is deepenable
            and (self.GetCount() <= 1 or other.GetCount() <= 1 or self.IsOr() == other.IsOr())
            and ((not self.IsMissing() and not other.IsMissing()) or self.IsFuture() == other.IsFuture())
         );
      }

      /// Inner composition function                                          
      ///   @tparam FORCE - insert even if types mismatch, by making this     
      ///      container deeper.                                              
      ///   @param stateCompliant whether states are compatible               
      ///   @param index the place to insert at                               
      ///   @param value the value to concatenate                             
      ///   @return the number of inserted elements                           
      template<bool FORCE, class C>
      auto ComposeInner(
          this C& self, bool stateCompliant, CT::Index auto&& index, auto&& value
      ) -> size_t {
         using I = IntentOf(value);
         using T = Deint<I>;
         auto& other = DeintCast(value);

         if constexpr (CT::TypeErased<C>) {
            if ((IsUntyped() and IsInvalid()) or IsSimilar<T>()) {
               // Mutate-insert inside untyped container                   
               return ThisCom::template InsertAt<false>(index, LglsFwd(value));
            }
            else if (IsEmpty() and IsTyped() and not IsTypeConstrained()) {
               // If incompatibly typed but empty and not constrained, we  
               // can still reset the container and reuse it               
               Reset();
               return ThisCom::template InsertAt<false>(index, LglsFwd(value));
            }
            else if (IsDeep()) {
               // Already deep, push value wrapped in a container          
               if (not stateCompliant) {
                  // If container is not or-compliant after insertion, we  
                  // need to add another layer                             
                  self.Deepen();
               }

               return ThisCom::template InsertAt<false>(
                  index, Deep<C> {LglsFwd(value)} //TODO are we allowed to absorb here? should we use Piecewise?
               );
            }

            if constexpr (FORCE) {
               // If this is reached, all else failed, but we are allowed  
               // to deepen, so just do it                                 
               self.Deepen();
               return ThisCom::template InsertAt<false>(
                  index, Deep<C> {LglsFwd(value)} //TODO are we allowed to absorb here? should we use Piecewise?
               );
            }
            else return 0;
         }
         else {
            if constexpr (Same<TypeOf<C>, T>) {
               // Insert to a same-typed container                         
               return ThisCom::template InsertAt<false>(
                  index, LglsFwd(value)
               );
            }
            else if constexpr (CT::Deep<TypeOf<C>>) {
               // Already deep, push value wrapped in a container          
               if (not stateCompliant) {
                  // If container is not or-compliant after insertion, we  
                  // need to add another layer                             
                  self.Deepen();
               }

               return ThisCom::template InsertAt<false>(
                  index, Deep<C> {LglsFwd(value)} //TODO are we allowed to absorb here? should we use Piecewise?
               );
            }
            else return 0;
         }
      }

      void MakeGap(this auto& self, auto& handle, size_t offset, size_t lhs_count, size_t all_count) {
         //auto handle = self.GetHandle() + offset;
         if (offset < lhs_count) {
            // We're moving to the right, so make sure we do it in      
            // reverse to avoid any potential overlap                   
            //TODO batch optimization for PODs
            const size_t moved = lhs_count - offset;
            auto from = handle + (moved - 1);
            auto to   = self.GetHandle() + (all_count - 1);
            auto const end = (from - moved).GetRaw();
            while (from.GetRaw() != end) {
               Id::ForEach([&]<Cid D>{
                  to.template EmplaceWithIntent<D>(Abandon {from});
               });
               --from; --to;
            }
         }
      }

      void CopyRegion(this auto& self, auto& handle, size_t rhs_count, CT::Container auto&& data) {
         //TODO batch optimization for PODs
         using I = IntentOf(data);
         auto src = DeintCast(data).GetHandle();
         //auto dst = self.GetHandle() + offset;
         auto const end = (handle + rhs_count).GetRaw();
         while (handle.GetRaw() != end) {
            Id::ForEach([&]<Cid D>{
               if constexpr (CT::Copied<I>)
                  handle.template EmplaceWithIntent<D>(Refer(src));
               else
                  handle.template EmplaceWithIntent<D>(I::Nest(src));
            });
            ++handle; ++src;
         }
      }
   };

   #undef ThisCom
}
