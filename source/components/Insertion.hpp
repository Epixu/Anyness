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
      ///   @tparam C - the contained type                                    
      ///   @tparam ...A - the arguments to test                              
      ///   @return true if container is constructible using {A...}           
      template<Container C, class...A>
      consteval bool DeepConstructible() noexcept {
         using FA = FirstOf<A...>;
         using SA = IntentOf<FA>;
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
   /// Implements insertion for containers                                    
   ///   @tparam ID - heap we're inserting to                                 
   ///   @tparam AS - type to serialize as before inserting. Useful for byte  
   ///      and text containers. Use void to insert without serialization     
   template<unsigned ID = 0, class AS = void>
   struct Insertion {
      using CTTI_Component = Yes<>;

      template<CT::Container C>
      static consteval bool Validate() {
         static_assert(C::VariableCount, "You can't insert stuff in a "
            "container that doesn't provide variable count");
      }

   private:
      template<CT::Container C>
      using Count = typename C::CountType;
      template<CT::Container C>
      using Deep  = typename C::DeepType;
      template<CT::Container C>
      using State = typename C::StateType;
      template<CT::Container C>
      using PickRangeMut = typename C::PickRangeMut;

   public:
      /// Insertion one or more elements at specific index                    
      template<bool FORCE = true, class A1, class...AN, CT::IndexedLinearly C>
      auto InsertAt(this C&, CT::Index auto, A1&&, AN&&...)
         -> Count<C> requires CT::RangeInsertable<C, A1, AN...>;

      /// Insert a number of elements at a specific place, nullifying them if 
      /// able to                                                             
      template<CT::IndexedLinearly C>
      auto InsertNulledAt(this C&, CT::Index auto, Count<C>)
         -> Count<C>;

      /// Insert a number of elements at a specific place, default-           
      /// constructing them if able to                                        
      template<CT::IndexedLinearly C>
      auto InsertDefaultAt(this C&, CT::Index auto, Count<C>)
         -> Count<C>;

      template<bool CONCAT = true, bool FORCE = true, CT::IndexedLinearly C>
      auto SmartPushAt(this C&, CT::Index auto, auto&&, State<C> = {})
         -> Count<C>;

      /// Insert one or more elements at the back                             
      template<bool FORCE = true, class A1, class...AN, CT::Container C>
      auto Insert(this C&, A1&&, AN&&...)
         -> Count<C> requires CT::RangeInsertable<C, A1, AN...>;

      /// Insert a number of elements at the back, nullifying them if able to 
      template<CT::Container C>
      auto InsertNulled(this C&, Count<C>) -> Count<C>;

      /// Insert a number of elements at the back, default-constructing them  
      ///   @param count - the number of elements to insert                   
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
                  self.SetCount(previousCount + constructed);
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
                  self.SetCount(previousCount + constructed);
                  throw;
               }
            }
         }

         // Success                                                     
         self.mCount = previousCount + count;
         return count;
      }

      template<bool CONCAT = true, bool FORCE = true, CT::Container C>
      auto SmartPush(this C&, auto&&, State<C> = {})
         -> Count<C>;

      template<bool TRANSFER_OR = true, CT::Container C>
      auto Deepen(this C&) -> Deep<C>&;


      /// Extend the container's memory and return the newly allocated range  
      ///   @attention if extending memory without ownership, the container   
      ///      will copy the data and diverge into a new allocation           
      ///   @param count - the number of elements to extend by                
      ///   @param arguments - the arguments to use for each constructor call 
      ///      no arguments will result in default construction               
      ///   @return the newly allocated mutable range                         
      template<CT::Container C, class...A>
      auto Extend(this C& self, Count<C> count = 1, A&&...arguments)
      -> PickRangeMut<C> {
         const auto previousCount = self.GetCount();
         if constexpr (sizeof...(A) == 0)
            self.InsertDefault(count);
         else if (count == 1)
            self.Emplace(FWD(arguments)...);
         else {
            // When creating multiple items, we can't allow arguments   
            // to be forwarded, because they might be moved away        
            self.AllocateMore(previousCount + count);
            for (Count<C> i = 0; i < count; i++)
               self.Emplace(DeintCast(arguments)...);
         }
         return self.SelectInner(previousCount, count);
      }
   };
}
