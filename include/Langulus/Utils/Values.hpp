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
   /// Empty compile-time value list                                          
   ///                                                                        
   template<>
   struct Values<> {
      using FirstType = void;
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

      LANGULUS(ALWAYS_INLINED)
      static constexpr void ForEach(auto&&)    {}

      LANGULUS(ALWAYS_INLINED)
      static constexpr bool ForEachAnd(auto&&) { return false; }

      LANGULUS(ALWAYS_INLINED)
      static constexpr bool ForEachOr(auto&&)  { return false; }

      template<auto...N>
      consteval auto operator + (Values<N...>&&) const -> Values<N...> { return {}; }

      LANGULUS(ALWAYS_INLINED)
      static constexpr void Expand(auto&&) {}

      template<class>
      using Intersect = Values<>;
   };


   ///                                                                        
   /// Compile-time value list with a single value.                           
   ///                                                                        
   template<auto E1>
   struct Values<E1> {
      using FirstType = decltype(E1);
      static constexpr auto First   = E1;
      static constexpr auto Last    = E1;
      static constexpr bool Empty   = false;
      static constexpr size_t Count = 1;

   protected:
      template<uint I>
      static consteval auto AtInner() {
         static_assert(I == 0, "Index is out of value list bounds");
         return E1;
      }

      template<auto...EN>
      static consteval bool IntersectInner(Values<EN...>&&) {
         return ((E1 == EN) or ...);
      }

   public:
      template<uint I>
      static constexpr auto At = AtInner<I>();

      template<auto E>
      static constexpr bool Contains = E == E1;

      template<auto...N>
      consteval auto operator + (Values<N...>&&) const -> Values<E1, N...> { return {}; }

      LANGULUS(ALWAYS_INLINED)
      static constexpr void ForEach(auto&& lambda) {
         static_assert(requires{ LglsLamb(lambda, E1); },
            "Provided argument is not a lambda of the form []<auto>");
          LglsLamb(lambda, E1);
      }

      LANGULUS(ALWAYS_INLINED)
      static constexpr bool ForEachAnd(auto&& lambda) {
         static_assert(requires{ {LglsLamb(lambda, E1)} -> ::std::convertible_to<bool>; },
            "Provided argument is not a lambda of the form []<auto> -> convertible to bool");
         return LglsLamb(lambda, E1);
      }

      LANGULUS(ALWAYS_INLINED)
      static constexpr bool ForEachOr(auto&& lambda) {
         static_assert(requires{ {LglsLamb(lambda, E1)} -> ::std::convertible_to<bool>; },
            "Provided argument is not a lambda of the form []<auto> -> convertible to bool");
         return LglsLamb(lambda, E1);
      }

      /// Doesn't generate code for further loops if lambda returns anything  
      /// but a No (utilizes a compile-time short-circuit)                    
      LANGULUS(ALWAYS_INLINED)
      static constexpr decltype(auto) ForEachConstOr(auto&& lambda) {
         return LglsLamb(lambda, E1);
      }

      LANGULUS(ALWAYS_INLINED)
      static constexpr auto Expand(auto&& lambda) {
         static_assert(requires{ LglsLamb(lambda, E1); },
            "Provided argument is not a lambda of the form []<auto>");
         return LglsLamb(lambda, E1);
      }

      template<class OTHER>
      using Intersect = Tif<IntersectInner(OTHER{}), Values<E1>, Values<>>;
   };


   ///                                                                        
   /// Compile-time value list with multiple values.                          
   ///                                                                        
   template<auto E1, auto E2, auto...EN>
   struct Values<E1, E2, EN...> {
      using FirstType = decltype(E1);
      static constexpr auto First   = E1;
      static constexpr auto Second  = E2;
      static constexpr auto Last    = []<class V = Values<EN...>> {
         if constexpr (sizeof...(EN)) return V::Last;
         else return E2;
      }();
      static constexpr bool Empty   = false;
      static constexpr size_t Count = sizeof...(EN) + 2;

   protected:
      template<uint I>
      static consteval auto AtInner() {
         static_assert(I < Count, "Index is out of value list bounds");

              if constexpr (I == 0)    return E1;
         else if constexpr (I == 1)    return E2;
         else return Values<EN...>::template AtInner<I - 2>();
      }

      template<auto...INTERSECT>
      static consteval auto IntersectInner(Values<INTERSECT...>&&) {
         using other = Values<INTERSECT...>;
         if constexpr (sizeof...(INTERSECT) == 0)
            return Values<>{};
         else if constexpr (sizeof...(INTERSECT) == 1)
            return typename other::template Intersect<Values> {};
         else if constexpr (sizeof...(EN) == 0) {
            return typename Values<E1>::template Intersect<other> {}
                +  typename Values<E2>::template Intersect<other> {};
         }
         else {
            return typename Values<E1>::template Intersect<other> {}
                +  typename Values<E2>::template Intersect<other> {}
                + (typename Values<EN>::template Intersect<other> {} + ...);
         }
      }

   public:
      template<uint I>
      static constexpr auto At = AtInner<I>();

      template<auto E>
      static constexpr bool Contains = E == E1 or E == E2 or ((E == EN) or ...);

      template<auto...N>
      consteval auto operator + (Values<N...>&&) const -> Values<E1, E2, EN..., N...> { return {}; }

      LANGULUS(ALWAYS_INLINED)
      static constexpr void ForEach(auto&& lambda) {
         static_assert(requires{ LglsLamb(lambda, E1); },
            "Provided argument is not a lambda of the form []<auto>");
          LglsLamb(lambda, E1);
          LglsLamb(lambda, E2);
         (LglsLamb(lambda, EN), ...);
      }

      LANGULUS(ALWAYS_INLINED)
      static constexpr bool ForEachAnd(auto&& lambda) {
         static_assert(requires{ {LglsLamb(lambda, E1)} -> ::std::convertible_to<bool>; },
            "Provided argument is not a lambda of the form []<auto> -> convertible to bool");
         return LglsLamb(lambda, E1)
            and LglsLamb(lambda, E2)
            and (... and LglsLamb(lambda, EN));
      }

      LANGULUS(ALWAYS_INLINED)
      static constexpr bool ForEachOr(auto&& lambda) {
         static_assert(requires{ {LglsLamb(lambda, E1)} -> ::std::convertible_to<bool>; },
            "Provided argument is not a lambda of the form []<auto> -> convertible to bool");
         return LglsLamb(lambda, E1)
             or LglsLamb(lambda, E2)
             or (... or LglsLamb(lambda, EN));
      }

      /// Doesn't generate code for further loops if lambda returns anything  
      /// but a No (utilizes a compile-time short-circuit)                    
      LANGULUS(ALWAYS_INLINED)
      static constexpr decltype(auto) ForEachConstOr(auto&& lambda) {
         decltype(auto) r1 = LglsLamb(lambda, E1);
         if constexpr (not ::std::same_as<No, decltype(r1)>)
            return r1;
         else {
            decltype(auto) r2 = LglsLamb(lambda, E2);
            if constexpr (not ::std::same_as<No, decltype(r2)>)
               return r2;
            else
               return Values<EN...>::ForEachConstOr(LglsFwd(lambda));
         }
      }

      LANGULUS(ALWAYS_INLINED)
      static constexpr auto Expand(auto&& lambda) {
         static_assert(requires{ LglsLamb(lambda, E1, E2, EN...); },
            "Provided argument is not a lambda of the form []<auto...>");
         return LglsLamb(lambda, E1, E2, EN...);
      }

      template<class OTHER>
      using Intersect = decltype(IntersectInner(OTHER{}));
   };

   template<class...V>
   using ConcatenateValueLists = decltype((LglsFake(V) + ...));
}
