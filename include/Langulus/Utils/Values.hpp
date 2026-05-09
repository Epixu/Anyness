///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../Core.hpp"


namespace Langulus
{
   template<auto...> struct Values;

   ///                                                                        
   /// Can be used to handle value sequences at compile-time                  
   ///                                                                        
   template<>
   struct Values<> {
      using FirstType = void;
      static constexpr auto First = 0;
      static constexpr bool Empty = true;
      static constexpr size_t Count = 0;

   protected:
      template<uint>
      static consteval auto AtInner() {
         static_assert(false, "Empty values");
      }

   public:
      template<uint I>
      static constexpr auto At = AtInner<I>();
      template<auto>
      static constexpr bool Contains = false;

      static constexpr void ForEach(auto&&)    {}
      static constexpr bool ForEachAnd(auto&&) { return false; }
      static constexpr bool ForEachOr(auto&&)  { return false; }
   };

   ///                                                                        
   /// Can be used to handle value sequences at compile-time                  
   ///                                                                        
   template<auto E1>
   struct Values<E1> {
      using FirstType = decltype(E1);
      static constexpr auto First = E1;
      static constexpr bool Empty = false;
      static constexpr size_t Count = 1;

   protected:
      template<uint I>
      static consteval auto AtInner() {
         static_assert(I == 0, "Index is out of value list bounds");
         return E1;
      }

   public:
      template<uint I>
      static constexpr auto At = AtInner<I>();

      template<auto E>
      static constexpr bool Contains = E == E1;

      static constexpr void ForEach(auto&& lambda) {
         static_assert(requires{ lambda.template operator()<E1>(); },
            "Provided argument is not a lambda of the form []<auto>");
          lambda.template operator()<E1>();
      }

      static constexpr bool ForEachAnd(auto&& lambda) {
         static_assert(requires{ {lambda.template operator()<E1>()} -> ::std::convertible_to<bool>; },
            "Provided argument is not a lambda of the form []<auto> -> convertible to bool");
         return lambda.template operator()<E1>();
      }

      static constexpr bool ForEachOr(auto&& lambda) {
         static_assert(requires{ {lambda.template operator()<E1>()} -> ::std::convertible_to<bool>; },
            "Provided argument is not a lambda of the form []<auto> -> convertible to bool");
         return lambda.template operator()<E1>();
      }
   };

   ///                                                                        
   /// Can be used to handle value sequences at compile-time                  
   ///                                                                        
   template<auto E1, auto E2, auto...EN>
   struct Values<E1, E2, EN...> {
      using FirstType = decltype(E1);
      static constexpr auto First = E1;
      static constexpr auto Second = E2;
      static constexpr bool Empty = false;
      static constexpr size_t Count = sizeof...(EN) + 2;

   protected:
      template<uint I>
      static consteval auto AtInner() {
         static_assert(I < Count, "Index is out of value list bounds");

              if constexpr (I == 0)    return E1;
         else if constexpr (I == 1)    return E2;
         else return Values<EN...>::template AtInner<I - 2>();
      }

   public:
      template<uint I>
      static constexpr auto At = AtInner<I>();

      template<auto E>
      static constexpr bool Contains = E == E1 or E == E2 or ((E == EN) or ...);

      static constexpr void ForEach(auto&& lambda) {
         static_assert(requires{ lambda.template operator()<E1>(); },
            "Provided argument is not a lambda of the form []<auto>");
          lambda.template operator()<E1>();
          lambda.template operator()<E2>();
         (lambda.template operator()<EN>(), ...);
      }

      static constexpr bool ForEachAnd(auto&& lambda) {
         static_assert(requires{ {lambda.template operator()<E1>()} -> ::std::convertible_to<bool>; },
            "Provided argument is not a lambda of the form []<auto> -> convertible to bool");
         return lambda.template operator()<E1>()
            and lambda.template operator()<E2>()
            and (... and lambda.template operator()<EN>());
      }

      static constexpr bool ForEachOr(auto&& lambda) {
         static_assert(requires{ {lambda.template operator()<E1>()} -> ::std::convertible_to<bool>; },
            "Provided argument is not a lambda of the form []<auto> -> convertible to bool");
         return lambda.template operator()<E1>()
             or lambda.template operator()<E2>()
             or (... or lambda.template operator()<EN>());
      }
   };
}
