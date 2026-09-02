#pragma once
#include "../Literal.hpp"
#include "Values.hpp"


namespace Langulus
{
   template<class ID, Literal...T>
   struct StaticSet : Values<T...> {};

   namespace Inner
   {
      template<class ID, int>
      struct StaticSetAt {
         auto friend StaticSetGet(StaticSetAt);
      };
      
      template<class ID, int N, class T>
      struct StaticSetSet {
         auto friend StaticSetGet(StaticSetAt<ID, N>) {
            return T{};
         }
      };

      template<class ID, Literal T, template<class, Literal...> class TList, Literal...Ts>
      consteval auto StaticSetInsert(TList<ID, Ts...>) {
         if constexpr (((Ts == T) or ...))
            return TList<ID, Ts...> {};
         else
            return TList<ID, Ts..., T> {};
      }

      struct TestStaticSet;
      struct TestStaticSet2;
   }

   template<class ID, Literal T, int N = 0, auto unique = []{}>
   consteval bool StaticSetInsert() {
      if constexpr (requires { StaticSetGet(Inner::StaticSetAt<ID, N>{}); }) {
         if constexpr (decltype(StaticSetGet(Inner::StaticSetAt<ID, N>{}))::template At<N> == T)
            return false;
         else 
            return StaticSetInsert<ID, T, N+1, unique>();
      }
      else if constexpr (N == 0) {
         (void) Inner::StaticSetSet<ID, N, StaticSet<ID, T>>{};
         return true;
      }
      else {
         (void) Inner::StaticSetSet<ID, N, decltype(
            Inner::StaticSetInsert<ID, T>(StaticSetGet(
               Inner::StaticSetAt<ID, N-1>{}
            ))
         )>{};
         return true;
      }
   }

   template<class ID, auto unique = []{}, int N = 0>
   consteval auto GetStaticSet() {
      if constexpr (requires { StaticSetGet(Inner::StaticSetAt<ID, N>{}); })
         return GetStaticSet<ID, unique, N+1>();
      else if constexpr (N == 0)
         return StaticSet<ID>{};
      else
         return StaticSetGet(Inner::StaticSetAt<ID, N-1>{});
   }

   template<class ID, auto unique = []{}, int N = 0>
   consteval int GetStaticSetSize() {
      if constexpr (requires { StaticSetGet(Inner::StaticSetAt<ID, N>{}); })
         return GetStaticSetSize<ID, unique, N+1>();
      else
         return N;
   }

   template<class ID, Literal T, int N = 0, auto unique = []{}>
   consteval int GetStaticSetIndex() {
      if constexpr (requires { StaticSetGet(Inner::StaticSetAt<ID, N>{}); }) {
         if constexpr (decltype(StaticSetGet(Inner::StaticSetAt<ID, N>{}))::template At<N> == T)
            return N;
         else 
            return GetStaticSetIndex<ID, T, N+1, unique>();
      }
      else if constexpr (N == 0) {
         (void) Inner::StaticSetSet<ID, N, StaticSet<ID, T>>{};
         return 0;
      }
      else {
         (void) Inner::StaticSetSet<ID, N, decltype(
            Inner::StaticSetInsert<ID, T>(StaticSetGet(
               Inner::StaticSetAt<ID, N-1>{}
            ))
         )>{};
         return N;
      }
   }

   static_assert(::std::same_as<decltype(GetStaticSet<Inner::TestStaticSet>()), StaticSet<Inner::TestStaticSet>>);
   static_assert(GetStaticSetSize<Inner::TestStaticSet>() == 0);
   static_assert(StaticSetInsert<Inner::TestStaticSet, "int">());
   static_assert(::std::same_as<decltype(GetStaticSet<Inner::TestStaticSet>()), StaticSet<Inner::TestStaticSet, "int">>);
   static_assert(GetStaticSetSize<Inner::TestStaticSet>() == 1);
   static_assert(StaticSetInsert<Inner::TestStaticSet, "float">());
   static_assert(::std::same_as<decltype(GetStaticSet<Inner::TestStaticSet>()), StaticSet<Inner::TestStaticSet, "int", "float">>);
   static_assert(GetStaticSetSize<Inner::TestStaticSet>() == 2);
   static_assert(not StaticSetInsert<Inner::TestStaticSet, "int">());
   static_assert(::std::same_as<decltype(GetStaticSet<Inner::TestStaticSet>()), StaticSet<Inner::TestStaticSet, "int", "float">>);
   static_assert(GetStaticSetSize<Inner::TestStaticSet>() == 2);
   static_assert(not StaticSetInsert<Inner::TestStaticSet, "float">());
   static_assert(::std::same_as<decltype(GetStaticSet<Inner::TestStaticSet>()), StaticSet<Inner::TestStaticSet, "int", "float">>);
   static_assert(GetStaticSetSize<Inner::TestStaticSet>() == 2);
   static_assert(StaticSetInsert<Inner::TestStaticSet, "bool">());
   static_assert(::std::same_as<decltype(GetStaticSet<Inner::TestStaticSet>()), StaticSet<Inner::TestStaticSet, "int", "float", "bool">>);
   static_assert(GetStaticSetSize<Inner::TestStaticSet>() == 3);

   static_assert(::std::same_as<decltype(GetStaticSet<Inner::TestStaticSet2>()), StaticSet<Inner::TestStaticSet2>>);
   static_assert(GetStaticSetSize<Inner::TestStaticSet2>() == 0);
   static_assert(GetStaticSetIndex<Inner::TestStaticSet2, "int">() == 0);
   static_assert(::std::same_as<decltype(GetStaticSet<Inner::TestStaticSet2>()), StaticSet<Inner::TestStaticSet2, "int">>);
   static_assert(GetStaticSetSize<Inner::TestStaticSet2>() == 1);
   static_assert(GetStaticSetIndex<Inner::TestStaticSet2, "float">() == 1);
   static_assert(::std::same_as<decltype(GetStaticSet<Inner::TestStaticSet2>()), StaticSet<Inner::TestStaticSet2, "int", "float">>);
   static_assert(GetStaticSetSize<Inner::TestStaticSet2>() == 2);
   static_assert(GetStaticSetIndex<Inner::TestStaticSet2, "int">() == 0);
   static_assert(::std::same_as<decltype(GetStaticSet<Inner::TestStaticSet2>()), StaticSet<Inner::TestStaticSet2, "int", "float">>);
   static_assert(GetStaticSetSize<Inner::TestStaticSet2>() == 2);
   static_assert(GetStaticSetIndex<Inner::TestStaticSet2, "float">() == 1);
   static_assert(::std::same_as<decltype(GetStaticSet<Inner::TestStaticSet2>()), StaticSet<Inner::TestStaticSet2, "int", "float">>);
   static_assert(GetStaticSetSize<Inner::TestStaticSet2>() == 2);
   static_assert(GetStaticSetIndex<Inner::TestStaticSet2, "bool">() == 2);
   static_assert(::std::same_as<decltype(GetStaticSet<Inner::TestStaticSet2>()), StaticSet<Inner::TestStaticSet2, "int", "float", "bool">>);
   static_assert(GetStaticSetSize<Inner::TestStaticSet>() == 3);
}