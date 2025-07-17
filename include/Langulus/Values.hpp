///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Core.hpp"


namespace Langulus
{
   ///                                                                        
   /// Can be used to handle value sequences at compile-time                  
   ///                                                                        
   template<auto...EN>
   struct Values;

   /// Empty values list                                                      
   template<>
   struct Values<> {
      static constexpr bool Empty = true;
      static constexpr size_t Count = 0;
   };

   /// Filled values list                                                     
   template<auto E1, auto...EN>
   struct Values<E1, EN...> {
      using FirstType = decltype(E1);
      static constexpr auto First = E1;
      static constexpr bool Empty = false;
      static constexpr size_t Count = sizeof...(EN) + 1;

   protected:
      template<unsigned I>
      static consteval auto AtInner() {
         if constexpr (I == 0)
            return E1;
         else if constexpr (I < Count)
            return Values<EN...>::template AtInner<I - 1>();
         else
            static_assert(false, "Index is out of value list bounds");
      }

   public:
      template<unsigned I>
      static constexpr auto At = AtInner<I>();
   };
}
