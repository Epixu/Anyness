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
      /// Insertion at specific index                                         
      template<bool FORCE = true, class A1, class...AN, CT::IndexedLinearly C>
      auto InsertAt(this C&, CT::Index auto, A1&&, AN&&...)
         -> Count<C> requires CT::RangeInsertable<C, A1, AN...>;

      template<bool CONCAT = true, bool FORCE = true, CT::IndexedLinearly C>
      auto SmartPushAt(this C&, CT::Index auto, auto&&, State<C> = {})
         -> Count<C>;

      /// Generic insertion                                                   
      template<bool FORCE = true, class A1, class...AN, CT::Container C>
      auto Insert(this C&, A1&&, AN&&...)
         -> Count<C> requires CT::RangeInsertable<C, A1, AN...>;

      template<bool CONCAT = true, bool FORCE = true, CT::Container C>
      auto SmartPush(this C&, auto&&, State<C> = {})
         -> Count<C>;

      template<bool TRANSFER_OR = true, CT::Container C>
      auto Deepen(this C&) -> Deep<C>&;

      template<CT::Container C>
      void Null(this C&, Count<C>);

      template<CT::Container C, class...A>
      auto Extend(this C&, Count<C> = 1, A&&...) -> PickRangeMut<C>;
   };
}
