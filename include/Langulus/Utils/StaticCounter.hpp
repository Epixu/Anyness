///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include <type_traits>

namespace Langulus
{
   namespace Inner
   {
      /// Original code from: stackoverflow.com/questions/6166337             
      ///   @tparam UNIQUE a type that differentiates counters                
      ///   @tparam N the current counter state                               
      template<class UNIQUE, int N>
      struct counter {
         struct generator {
            friend consteval bool is_defined(counter) {
               return true;
            }
         };

         friend consteval bool is_defined(counter);

         template<typename Tag = counter, bool = is_defined(Tag{})>
         static consteval bool exists(int) {
            return true;
         }

         static consteval bool exists(...) {
            return generator(), false;
         }
      };

      struct counter_tester;
      struct counter_tester2;
      struct global_counter;
   }

   /// Every time you call this you get a new integer at compile-time         
   ///   @attention resets between translation units                          
   ///   @attention the order is undefined                                    
   ///   @attention make sure you include all relevant uses to get the same   
   ///              count                                                     
   template<class UNIQUE, typename ODR_VIOLATION_PREVENTER = decltype([]{}), int N = 0>
   consteval int unique_id() {
      if constexpr (not Inner::counter<UNIQUE, N>::exists(N))
         return N;
      else
         return unique_id<UNIQUE, ODR_VIOLATION_PREVENTER, N + 1>();
   }

   static_assert(unique_id<Inner::counter_tester>() == 0);
   static_assert(unique_id<Inner::counter_tester>() == 1);
   static_assert(unique_id<Inner::counter_tester>() == 2);
   static_assert(unique_id<Inner::counter_tester>() == 3);

   static_assert(unique_id<Inner::counter_tester2>() == 0);
   static_assert(unique_id<Inner::counter_tester2>() == 1);
   static_assert(unique_id<Inner::counter_tester2>() == 2);
   static_assert(unique_id<Inner::counter_tester2>() == 3);

   static_assert(unique_id<Inner::counter_tester2, decltype([]{})>() == 4);
   static_assert(unique_id<Inner::counter_tester2, decltype([]{})>() == 5);
   static_assert(unique_id<Inner::counter_tester2, decltype([]{})>() == 6);
   static_assert(unique_id<Inner::counter_tester2, decltype([]{})>() == 7);
}

/// Global counter, not associated with a type                                
#define LglsGlobalCounter() \
   std::integral_constant<int, ::Langulus::unique_id<::Langulus::global_counter>()>

/// Counter that increases only for a specific type                           
#define LglsCounter(T) \
   std::integral_constant<int, ::Langulus::unique_id<T>()>

/// When a template involves concepts, there is too much aggressive caching,  
/// which screws default template arguments involving counters. These can     
/// remedy the situation.                                                     
#define LglsGlobalCounterForConcept() \
   std::integral_constant<int, ::Langulus::unique_id<::Langulus::global_counter, decltype([]{})>()>

#define LglsCounterForConcept(T) \
   std::integral_constant<int, ::Langulus::unique_id<T, decltype([]{})>()>