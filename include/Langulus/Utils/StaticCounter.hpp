///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once

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
   }

   /// Every time you call this you get a new integer at compile-time         
   ///   @attention resets between translation units                          
   ///   @attention the order is undefined                                    
   ///   @attention make sure you include all relevant uses to get the same   
   ///              count                                                     
   template<class UNIQUE, int N = 0, typename ODR_VIOLATION_PREVENTER = decltype([]{})>
   consteval int unique_id() {
      if constexpr (not Inner::counter<UNIQUE, N>::exists(N))
         return N;
      else
         return unique_id<UNIQUE, N + 1>();
   }

   static_assert(unique_id<Inner::counter_tester>() == 0);
   static_assert(unique_id<Inner::counter_tester>() == 1);
   static_assert(unique_id<Inner::counter_tester>() == 2);
   static_assert(unique_id<Inner::counter_tester>() == 3);

   static_assert(unique_id<Inner::counter_tester2>() == 0);
   static_assert(unique_id<Inner::counter_tester2>() == 1);
   static_assert(unique_id<Inner::counter_tester2>() == 2);
   static_assert(unique_id<Inner::counter_tester2>() == 3);
}