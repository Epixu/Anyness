#pragma once
#include "../Literal.hpp"
#include "Values.hpp"

namespace Langulus
{
   template<Literal...T>
   struct StaticVector : Values<T...> {};

   namespace Inner
   {
      template<int>
      struct StaticVectorAt {
         auto friend StaticVectorGet(StaticVectorAt);
      };
      
      template<int N, class T>
      struct StaticVectorSet {
         auto friend StaticVectorGet(StaticVectorAt<N>) {
            return T{};
         }
      };

      template<Literal T, template<Literal...> class TList, Literal...Ts>
      auto StaticVectorAppend(TList<Ts...>) -> TList<Ts..., T>;
   }

   template<Literal T, int N = 0, auto unique = []{}>
   consteval bool StaticVectorAppend() {
      if constexpr (requires { StaticVectorGet(Inner::StaticVectorAt<N>{}); })
         StaticVectorAppend<T, N+1, unique>();
      else if constexpr (N == 0)
         void(Inner::StaticVectorSet<N, StaticVector<T>>{});
      else
         void(Inner::StaticVectorSet<N, decltype(
            Inner::StaticVectorAppend<T>(StaticVectorGet(
               Inner::StaticVectorAt<N-1>{}
            ))
         )>{});
      return true;
   }

   template<auto unique = []{}, int N = 0>
   consteval auto GetStaticVector() {
      if constexpr (requires { StaticVectorGet(Inner::StaticVectorAt<N>{}); })
         return GetStaticVector<unique, N+1>();
      else if constexpr (N == 0)
         return StaticVector{};
      else
         return StaticVectorGet(Inner::StaticVectorAt<N-1>{});
   }

   template<auto unique = []{}, int N = 0>
   consteval int GetStaticVectorSize() {
      if constexpr (requires { StaticVectorGet(Inner::StaticVectorAt<N>{}); })
         return GetStaticVectorSize<unique, N+1>();
      else
         return N;
   }

   static_assert(::std::same_as<decltype(GetStaticVector()), StaticVector<>>);
   static_assert(GetStaticVectorSize() == 0);
   static_assert(StaticVectorAppend<"int">());
   static_assert(::std::same_as<decltype(GetStaticVector()), StaticVector<"int">>);
   static_assert(GetStaticVectorSize() == 1);
   static_assert(StaticVectorAppend<"float">());
   static_assert(::std::same_as<decltype(GetStaticVector()), StaticVector<"int", "float">>);
   static_assert(GetStaticVectorSize() == 2);
}