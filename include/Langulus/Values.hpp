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
   template<auto E1, auto...EN>
   struct Values {
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

      static constexpr void ForEach(auto&& lambda) {
         static_assert(requires{ lambda.template operator()<E1>(); },
            "Provided argument is not a lambda of the form []<auto>");
          lambda.template operator()<E1>();
         (lambda.template operator()<EN>(), ...);
      }

      static constexpr bool ForEachAnd(auto&& lambda) {
         static_assert(requires{ {lambda.template operator()<E1>()} -> ::std::convertible_to<bool>; },
            "Provided argument is not a lambda of the form []<auto> -> convertible to bool");
         return lambda.template operator()<E1>()
            and (... and lambda.template operator()<EN>());
      }

      static constexpr bool ForEachOr(auto&& lambda) {
         static_assert(requires{ {lambda.template operator()<E1>()} -> ::std::convertible_to<bool>; },
            "Provided argument is not a lambda of the form []<auto> -> convertible to bool");
         return lambda.template operator()<E1>()
             or (... or lambda.template operator()<EN>());
      }
   };
}
