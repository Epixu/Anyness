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
#include <Langulus/CT/ReflectAs.hpp>


namespace Langulus::CT
{
   /// Check if container's elements are unfold-assignable                    
   ///   @attention type-erased elements are always assignable, and will fail 
   ///      at runtime if not reflected as such                               
   template<class C, class A>
   concept RangeAssignable = Container<C> and (
      Untyped<C> or UnfoldAssignable<TypeOf<C>, A>
   );

   namespace Inner
   {
      /// Test whether a container is assignable with the given argument      
      ///   @tparam C - the container                                         
      ///   @tparam A - the argument to test                                  
      ///   @return true if you can assign A to the container                 
      template<Container C, class A>
      consteval bool DeepAssignable() noexcept {
         using SA = IntentOf<A>;
         using T  = TypeOf<C>;

         if constexpr (Untyped<C>) {
            // Type-erased containers accept almost any type - they     
            // will report errors at runtime instead, if any            
            return Reflectable<Deint<A>>;
         }
         else if constexpr (Container<A>) {
            if constexpr (SA::Shallow) {
               // Generally, shallow intents are always supported,      
               // but copying will call element assigners, so we        
               // have to check if the contained type supports it       
               if constexpr (Copied<SA>)
                  return ReferAssignable<T>;
               else
                  return true;
            }
            else {
               // Cloning always calls element assigners, and we        
               // have to check whether contained elements can do it    
               return IntentAssignable<Langulus::Clone, T>;
            }
         }
         else return UnfoldAssignable<T, A>;
      };
   }

   /// Concept for recognizing argument with which a statically typed         
   /// container can be assigned                                              
   template<class C, class A>
   concept DeepAssignable = Inner::DeepAssignable<C, A>();
}

namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Implements assignment for containers                                   
   ///   @tparam ID - heap we're removing from                                
   template<unsigned ID = 0>
   struct Assignment {
      using CTTI_Component = Yes<>;
      static constexpr int ComponentPrecedence = 3000;

      template<CT::Container C, class A>
      void Fill(this C&, A&&) requires CT::RangeAssignable<C, A>;
      
      template<CT::Container C, class A>
      C& operator = (this C& self, A&&) requires CT::RangeAssignable<C, A>;
   };
}
