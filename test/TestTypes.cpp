///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "Main.hpp"
#include <Langulus/Utils/Tuple.hpp>
#include <Langulus/Utils/Types.hpp>

using namespace Langulus;


///                                                                           
/// CT::Void                                                                  
///                                                                           
namespace
{
   struct VoidType { using CTTI_Void = Yes<>; };
   struct VoidTypeDerived : VoidType {};
   struct VoidTypeExternal {};
   struct NonVoidTypeDerived : VoidType { using CTTI_Void = No; };
   struct IncompleteType;
}

namespace Langulus::CTTI
{
   template<>
   struct Void<VoidTypeExternal> {};
}

TEST_CASE_TEMPLATE("Testing void types", TestType
   , void
   , VoidType
   , VoidType const
   , VoidType&
   , VoidTypeDerived
   , VoidTypeDerived const
   , VoidTypeDerived&
   , VoidTypeExternal
   , VoidTypeExternal const
   , VoidTypeExternal&
   , Types<>
) {
   static_assert(    CT::Void<TestType>);
   static_assert(not CT::NotVoid<TestType>);
}

TEST_CASE_TEMPLATE("Testing non-void types", TestType
   , void*
   , VoidType*
   , NonVoidTypeDerived
   , NonVoidTypeDerived const
   , NonVoidTypeDerived*
   //, IncompleteType         // shouldn't compile
   //, IncompleteType const   // shouldn't compile
   , IncompleteType*
   , int
   , int const
   , int const&
   , int&
   , Types<void>
   , Types<void*>
   , Types<void, void>
) {
   static_assert(not CT::Void<TestType>);
   static_assert(    CT::NotVoid<TestType>);
}

//static_assert(CT::Void<>); // shouldn't compile at all
static_assert(    CT::Void<VoidType, VoidTypeDerived, VoidTypeExternal>);
static_assert(not CT::Void<VoidType, VoidTypeDerived, NonVoidTypeDerived>);

//static_assert(CT::NotVoid<>); // shouldn't compile at all
static_assert(    CT::NotVoid<VoidType*, NonVoidTypeDerived, int>);
static_assert(not CT::NotVoid<VoidType*, NonVoidTypeDerived, VoidType>);


///                                                                           
/// CT::Typelist                                                              
///                                                                           
namespace
{
   struct CustomTypelist { using CTTI_Typelist = Yes<>; };
   struct CustomTypelistDerived : CustomTypelist {};
   struct CustomTypelistExternal {};
   struct CustomNonTypelistDerived : CustomTypelist { using CTTI_Typelist = No; };
}

namespace Langulus::CTTI
{
   template<>
   struct Typelist<CustomTypelistExternal> {};
}

TEST_CASE_TEMPLATE("Testing typelists", TestType
   , Types<>
   , Types<void>
   , Types<void, void>
   , Types<int>
   , Types<int, float>
   , CustomTypelist
   , CustomTypelist const
   , CustomTypelist&
   , CustomTypelistDerived
   , CustomTypelistDerived const
   , CustomTypelistDerived&
   , CustomTypelistExternal
   , CustomTypelistExternal const
   , CustomTypelistExternal&
) {
   static_assert(    CT::Typelist<TestType>);
   static_assert(not CT::NotTypelist<TestType>);
}

TEST_CASE_TEMPLATE("Testing non-typelists", TestType
   , void
   , void*
   , CustomTypelist*
   , CustomNonTypelistDerived
   , CustomNonTypelistDerived const
   , CustomNonTypelistDerived*
   //, IncompleteType         // shouldn't compile
   //, IncompleteType const   // shouldn't compile
   , IncompleteType*
   , int
   , int const
   , int const&
   , int&
) {
   static_assert(not CT::Typelist<TestType>);
   static_assert(    CT::NotTypelist<TestType>);
}

//static_assert(CT::Typelist<>); // shouldn't compile at all
static_assert(    CT::Typelist<Types<>, CustomTypelist, CustomTypelistExternal>);
static_assert(not CT::Typelist<Types<>, CustomTypelist, CustomNonTypelistDerived>);

//static_assert(CT::NotTypelist<>); // shouldn't compile at all
static_assert(    CT::NotTypelist<void, CustomNonTypelistDerived, int>);
static_assert(not CT::NotTypelist<void, CustomNonTypelistDerived, Types<>>);

using TestingList7 = Types<void, int, float, bool, char, CustomTypelist, CustomTypelistExternal>;
static_assert(TestingList7::Count == 7);
static_assert(::std::same_as<typename TestingList7::First, void>);
static_assert(::std::same_as<typename TestingList7::Second, int>);
static_assert(::std::same_as<typename TestingList7::template At<0>, void>);
static_assert(::std::same_as<typename TestingList7::template At<1>, int>);
static_assert(::std::same_as<typename TestingList7::template At<2>, float>);
static_assert(::std::same_as<typename TestingList7::template At<3>, bool>);
static_assert(::std::same_as<typename TestingList7::template At<4>, char>);
static_assert(::std::same_as<typename TestingList7::template At<5>, CustomTypelist>);
static_assert(::std::same_as<typename TestingList7::template At<6>, CustomTypelistExternal>);

SCENARIO("Testing Types::Empty") {
   {
      using T = Types<>;
      static_assert(T::Empty);
   }
   {
      using T = Types<void>;
      static_assert(not T::Empty);
   }
   {
      using T = Types<Types<void>>;
      static_assert(not T::Empty);
   }
   {
      using T = Types<int, float>;
      static_assert(not T::Empty);
   }
   {
      using T = Types<int, float, Types<void>>;
      static_assert(not T::Empty);
   }
}

SCENARIO("Testing Types::Count") {
   {
      using T = Types<>;
      static_assert(T::Count == 0);
   }
   {
      using T = Types<void>;
      static_assert(T::Count == 1);
   }
   {
      using T = Types<Types<void>>;
      static_assert(T::Count == 1);
   }
   {
      using T = Types<int, float>;
      static_assert(T::Count == 2);
   }
   {
      using T = Types<int, float, bool>;
      static_assert(T::Count == 3);
   }
   {
      using T = Types<int, float, bool, Types<void>>;
      static_assert(T::Count == 4);
   }
}

SCENARIO("Testing Types::First") {
   {
      using T = Types<>;
      static_assert(::std::same_as<typename T::First, void>);
   }
   {
      using T = Types<int>;
      static_assert(::std::same_as<typename T::First, int>);
   }
   {
      using T = Types<Types<void>>;
      static_assert(::std::same_as<typename T::First, Types<void>>);
   }
   {
      using T = Types<int&, float>;
      static_assert(::std::same_as<typename T::First, int&>);
   }
   {
      using T = Types<int const, float, bool>;
      static_assert(::std::same_as<typename T::First, const int>);
   }
}

SCENARIO("Testing Types::Second") {
   {
      using T = Types<>;
      static_assert(::std::same_as<typename T::Second, void>);
   }
   {
      using T = Types<int>;
      static_assert(::std::same_as<typename T::Second, void>);
   }
   {
      using T = Types<int&, float>;
      static_assert(::std::same_as<typename T::Second, float>);
   }
   {
      using T = Types<int const, float&, bool>;
      static_assert(::std::same_as<typename T::Second, float&>);
   }
   {
      using T = Types<int const, Types<int>, float&, bool>;
      static_assert(::std::same_as<typename T::Second, Types<int>>);
   }
}

SCENARIO("Testing Types::Reverse") {
   {
      using T = Types<>;
      static_assert(::std::same_as<typename T::Reverse, T>);
   }
   {
      using T = Types<int>;
      static_assert(::std::same_as<typename T::Reverse, T>);
   }
   {
      using T = Types<Types<int>>;
      static_assert(::std::same_as<typename T::Reverse, T>);
   }
   {
      using T = Types<int&, float>;
      static_assert(::std::same_as<typename T::Reverse, Types<float, int&>>);
   }
   {
      using T = Types<int const, float&, bool>;
      static_assert(::std::same_as<typename T::Reverse, Types<bool, float&, const int>>);
   }
   {
      using T = Types<int const, float&, bool, Types<void> const>;
      static_assert(::std::same_as<typename T::Reverse, Types<Types<void> const, bool, float&, const int>>);
   }
}

SCENARIO("Testing Types::ForEach") {
   {
      using T = Types<>;
      volatile int cumulative_size = 0;
      T::ForEach([&]<class E> {
         cumulative_size += sizeof(E);
      });
      REQUIRE(cumulative_size == 0);
   }
   {
      using T = Types<int>;
      volatile int cumulative_size = 0;
      T::ForEach([&]<class E> {
         cumulative_size += sizeof(E);
      });
      REQUIRE(cumulative_size == sizeof(int));
   }
   {
      using T = Types<Types<int>>;
      volatile int cumulative_size = 0;
      T::ForEach([&]<class E> {
         cumulative_size += sizeof(E);
      });
      REQUIRE(cumulative_size == sizeof(Types<int>));
   }
   {
      using T = Types<int&, float>;
      volatile int cumulative_size = 0;
      T::ForEach([&]<class E> {
         cumulative_size += sizeof(E);
      });
      REQUIRE(cumulative_size == sizeof(int) + sizeof(float));
   }
   {
      using T = Types<int const, float&, bool>;
      volatile int cumulative_size = 0;
      T::ForEach([&]<class E> {
         cumulative_size += sizeof(E);
      });
      REQUIRE(cumulative_size == sizeof(int) + sizeof(float) + sizeof(bool));
   }
}

SCENARIO("Testing Types::ForEachAnd") {
   {
      using T = Types<>;
      volatile int cumulative_size = 0;
      const auto result = T::ForEachAnd([&]<class E> {
         cumulative_size += sizeof(E);
         return true;
      });
      REQUIRE(not result);
      REQUIRE(cumulative_size == 0);
   }
   {
      using T = Types<int>;
      volatile int cumulative_size = 0;
      const auto result = T::ForEachAnd([&]<class E> {
         cumulative_size += sizeof(E);
         return true;
      });
      REQUIRE(result);
      REQUIRE(cumulative_size == sizeof(int));
   }
   {
      using T = Types<Types<int>>;
      volatile int cumulative_size = 0;
      const auto result = T::ForEachAnd([&]<class E> {
         cumulative_size += sizeof(E);
         return true;
      });
      REQUIRE(result);
      REQUIRE(cumulative_size == sizeof(Types<int>));
   }
   {
      using T = Types<int&, float>;
      volatile int cumulative_size = 0;
      const auto result = T::ForEachAnd([&]<class E> {
         cumulative_size += sizeof(E);
         return true;
      });
      REQUIRE(result);
      REQUIRE(cumulative_size == sizeof(int) + sizeof(float));
   }
   {
      using T = Types<int const, float&, bool>;
      volatile int cumulative_size = 0;
      const auto result = T::ForEachAnd([&]<class E> {
         cumulative_size += sizeof(E);
         return true;
      });
      REQUIRE(result);
      REQUIRE(cumulative_size == sizeof(int) + sizeof(float) + sizeof(bool));
   }
}

SCENARIO("Testing Types::ForEachOr") {
   {
      using T = Types<>;
      volatile int cumulative_size = 0;
      const auto result = T::ForEachOr([&]<class E> {
         cumulative_size += sizeof(E);
         return ::std::same_as<E, bool>;
      });
      REQUIRE(not result);
      REQUIRE(cumulative_size == 0);
   }
   {
      using T = Types<int>;
      volatile int cumulative_size = 0;
      const auto result = T::ForEachOr([&]<class E> {
         cumulative_size += sizeof(E);
         return ::std::same_as<E, bool>;
      });
      REQUIRE(not result);
      REQUIRE(cumulative_size == sizeof(int));
   }
   {
      using T = Types<int&, float>;
      volatile int cumulative_size = 0;
      const auto result = T::ForEachOr([&]<class E> {
         cumulative_size += sizeof(E);
         return ::std::same_as<E, bool>;
      });
      REQUIRE(not result);
      REQUIRE(cumulative_size == sizeof(int) + sizeof(float));
   }
   {
      using T = Types<int const, float&, bool, double>;
      volatile int cumulative_size = 0;
      const auto result = T::ForEachOr([&]<class E> {
         cumulative_size += sizeof(E);
         return ::std::same_as<E, bool>;
      });
      REQUIRE(result);
      REQUIRE(cumulative_size == sizeof(int) + sizeof(float) + sizeof(bool));
   }
}

SCENARIO("Testing Types::ForEachConstOr") {
   {
      using T = Types<>;
      volatile int cumulative_size = 0;
      auto result = T::ForEachConstOr([&]<class E> {
         cumulative_size += sizeof(E);
         if constexpr (::std::same_as<E, bool>)
            return true;
         else
            return No {};
      });
      static_assert(::std::same_as<decltype(result), No>);
      REQUIRE(cumulative_size == 0);
   }
   {
      using T = Types<int>;
      volatile int cumulative_size = 0;
      auto result = T::ForEachConstOr([&]<class E> {
         cumulative_size += sizeof(E);
         if constexpr (::std::same_as<E, bool>)
            return true;
         else
            return No{};
      });
      static_assert(::std::same_as<decltype(result), No>);
      REQUIRE(cumulative_size == sizeof(int));
   }
   {
      using T = Types<int&, float>;
      volatile int cumulative_size = 0;
      auto result = T::ForEachConstOr([&]<class E> {
         cumulative_size += sizeof(E);
         if constexpr (::std::same_as<E, bool>)
            return true;
         else
            return No{};
      });
      static_assert(::std::same_as<decltype(result), No>);
      REQUIRE(cumulative_size == sizeof(int) + sizeof(float));
   }
   {
      using T = Types<int const, float&, bool, double>;
      volatile int cumulative_size = 0;
      auto result = T::ForEachConstOr([&]<class E> {
         cumulative_size += sizeof(E);
         if constexpr (::std::same_as<E, bool>)
            return true;
         else
            return No{};
      });
      static_assert(::std::same_as<decltype(result), bool>);
      REQUIRE(result);
      REQUIRE(cumulative_size == sizeof(int) + sizeof(float) + sizeof(bool));
   }
}

SCENARIO("Testing Types::ForEachIndexed") {
   {
      using T = Types<>;
      volatile int cumulative_size = 0;
      T::ForEachIndexed([&]<class E, int I> {
         cumulative_size += sizeof(E);
         static_assert(I == 0);
      });
      REQUIRE(cumulative_size == 0);
   }
   {
      using T = Types<int>;
      volatile int counter = 0;
      volatile int cumulative_size = 0;
      T::ForEachIndexed([&]<class E, int I> {
         cumulative_size += sizeof(E);
         REQUIRE(I == counter);
         counter += 1;
      });
      REQUIRE(cumulative_size == sizeof(int));
   }
   {
      using T = Types<int&, float>;
      volatile int counter = 0;
      volatile int cumulative_size = 0;
      T::ForEachIndexed([&]<class E, int I> {
         cumulative_size += sizeof(E);
         REQUIRE(I == counter);
         counter += 1;
      });
      REQUIRE(cumulative_size == sizeof(int) + sizeof(float));
   }
   {
      using T = Types<int const, float&, bool>;
      volatile int counter = 0;
      volatile int cumulative_size = 0;
      T::ForEachIndexed([&]<class E, int I> {
         cumulative_size += sizeof(E);
         REQUIRE(I == counter);
         counter += 1;
      });
      REQUIRE(cumulative_size == sizeof(int) + sizeof(float) + sizeof(bool));
   }
   {
      using T = Types<int const, float&, bool, Types<double>>;
      volatile int counter = 0;
      volatile int cumulative_size = 0;
      T::ForEachIndexed([&]<class E, int I> {
         cumulative_size += sizeof(E);
         REQUIRE(I == counter);
         counter += 1;
      });
      REQUIRE(cumulative_size == sizeof(int) + sizeof(float) + sizeof(bool) + sizeof(Types<double>));
   }
}

SCENARIO("Testing Types::ForEachIndexedAnd") {
   {
      using T = Types<>;
      volatile int counter = 0;
      volatile int cumulative_size = 0;
      const auto result = T::ForEachIndexedAnd([&]<class E, int I> {
         cumulative_size += sizeof(E);
         REQUIRE(I == counter);
         counter += 1;
         return true;
      });
      REQUIRE(not result);
      REQUIRE(cumulative_size == 0);
   }
   {
      using T = Types<int>;
      volatile int counter = 0;
      volatile int cumulative_size = 0;
      const auto result = T::ForEachIndexedAnd([&]<class E, int I> {
         cumulative_size += sizeof(E);
         REQUIRE(I == counter);
         counter += 1;
         return true;
      });
      REQUIRE(result);
      REQUIRE(cumulative_size == sizeof(int));
   }
   {
      using T = Types<int&, float>;
      volatile int counter = 0;
      volatile int cumulative_size = 0;
      const auto result = T::ForEachIndexedAnd([&]<class E, int I> {
         cumulative_size += sizeof(E);
         REQUIRE(I == counter);
         counter += 1;
         return true;
      });
      REQUIRE(result);
      REQUIRE(cumulative_size == sizeof(int) + sizeof(float));
   }
   {
      using T = Types<int const, float&, bool>;
      volatile int counter = 0;
      volatile int cumulative_size = 0;
      const auto result = T::ForEachIndexedAnd([&]<class E, int I> {
         cumulative_size += sizeof(E);
         REQUIRE(I == counter);
         counter += 1;
         return true;
      });
      REQUIRE(result);
      REQUIRE(cumulative_size == sizeof(int) + sizeof(float) + sizeof(bool));
   }
}

SCENARIO("Testing Types::ForEachIndexedOr") {
   {
      using T = Types<>;
      volatile int counter = 0;
      volatile int cumulative_size = 0;
      const auto result = T::ForEachIndexedOr([&]<class E, int I> {
         cumulative_size += sizeof(E);
         REQUIRE(I == counter);
         counter += 1;
         return ::std::same_as<E, bool>;
      });
      REQUIRE(not result);
      REQUIRE(cumulative_size == 0);
   }
   {
      using T = Types<int>;
      volatile int counter = 0;
      volatile int cumulative_size = 0;
      const auto result = T::ForEachIndexedOr([&]<class E, int I> {
         cumulative_size += sizeof(E);
         REQUIRE(I == counter);
         counter += 1;
         return ::std::same_as<E, bool>;
      });
      REQUIRE(not result);
      REQUIRE(cumulative_size == sizeof(int));
   }
   {
      using T = Types<int&, float>;
      volatile int counter = 0;
      volatile int cumulative_size = 0;
      const auto result = T::ForEachIndexedOr([&]<class E, int I> {
         cumulative_size += sizeof(E);
         REQUIRE(I == counter);
         counter += 1;
         return ::std::same_as<E, bool>;
      });
      REQUIRE(not result);
      REQUIRE(cumulative_size == sizeof(int) + sizeof(float));
   }
   {
      using T = Types<int const, float&, bool, double>;
      volatile int counter = 0;
      volatile int cumulative_size = 0;
      const auto result = T::ForEachIndexedOr([&]<class E, int I> {
         cumulative_size += sizeof(E);
         REQUIRE(I == counter);
         counter += 1;
         return ::std::same_as<E, bool>;
      });
      REQUIRE(result);
      REQUIRE(cumulative_size == sizeof(int) + sizeof(float) + sizeof(bool));
   }
}

SCENARIO("Testing Types::Expand") {
   {
      using T = Types<>;
      T::Expand([]<class...E> {
         static_assert(sizeof...(E) == 0);
         static_assert((sizeof(E) + ...) == 0);
      });
   }
   {
      using T = Types<int>;
      T::Expand([]<class...E> {
         static_assert(sizeof...(E) == 1);
         static_assert((sizeof(E) + ...) == sizeof(int));
      });
   }
   {
      using T = Types<int&, float>;
      T::Expand([]<class...E> {
         static_assert(sizeof...(E) == 2);
         static_assert((sizeof(E) + ...) == sizeof(int) + sizeof(float));
      });
   }
   {
      using T = Types<int const, float&, bool>;
      T::Expand([]<class...E> {
         static_assert(sizeof...(E) == 3);
         static_assert((sizeof(E) + ...) == sizeof(int) + sizeof(float) + sizeof(bool));
      });
   }
   {
      using T = Types<int const, float&, bool, Types<double>>;
      T::Expand([]<class...E> {
         static_assert(sizeof...(E) == 4);
         static_assert((sizeof(E) + ...) == sizeof(int) + sizeof(float) + sizeof(bool) + sizeof(Types<double>));
      });
   }
}

SCENARIO("Testing Types::At") {
   {
      using T = Types<>;
      static_assert(::std::same_as<typename T::template At<0>, void>);
      static_assert(::std::same_as<typename T::template At<1>, void>);
      static_assert(::std::same_as<typename T::template At<2>, void>);
   }
   {
      using T = Types<int>;
      static_assert(::std::same_as<typename T::template At<0>, int>);
      static_assert(::std::same_as<typename T::template At<1>, void>);
      static_assert(::std::same_as<typename T::template At<2>, void>);
   }
   {
      using T = Types<int&, float>;
      static_assert(::std::same_as<typename T::template At<0>, int&>);
      static_assert(::std::same_as<typename T::template At<1>, float>);
      static_assert(::std::same_as<typename T::template At<2>, void>);
   }
   {
      using T = Types<int const, float&, bool, Types<double>>;
      static_assert(::std::same_as<typename T::template At<0>, int const>);
      static_assert(::std::same_as<typename T::template At<1>, float&>);
      static_assert(::std::same_as<typename T::template At<2>, bool>);
      static_assert(::std::same_as<typename T::template At<3>, Types<double>>);
      static_assert(::std::same_as<typename T::template At<4>, void>);
   }
}

SCENARIO("Testing Types::GenerateTypes") {
   {
      using T = Types<>;
      auto result = T::GenerateTypes([]<class E> {
         return Types<E>{};
      });
      static_assert(::std::same_as<decltype(result), T>);
   }
   {
      using T = Types<int>;
      auto result = T::GenerateTypes([]<class E> {
         return Types<E>{};
      });
      static_assert(::std::same_as<decltype(result), Types<T>>);
   }
   {
      using T = Types<int&, float>;
      auto result = T::GenerateTypes([]<class E> {
         return Types<E>{};
      });
      static_assert(::std::same_as<decltype(result), Types<Types<int&>, Types<float>>>);
   }
   {
      using T = Types<int const, float&, bool>;
      auto result = T::GenerateTypes([]<class E> {
         return Types<E>{};
      });
      static_assert(::std::same_as<decltype(result), Types<Types<int const>, Types<float&>, Types<bool>>>);
   }
   {
      using T = Types<int const, float&, bool, Types<double>>;
      auto result = T::GenerateTypes([]<class E> {
         return Types<E>{};
      });
      static_assert(::std::same_as<decltype(result), Types<Types<int const>, Types<float&>, Types<bool>, Types<Types<double>>>>);
   }
}

SCENARIO("Testing Types::GenerateData") {
   {
      using T = Types<>;
      constexpr auto result = T::GenerateData([]<class E> {
         return static_cast<E>(25);
      });
      static_assert(result == ::std::tuple<>{});
   }
   {
      using T = Types<int>;
      constexpr auto result = T::GenerateData([]<class E> {
         return static_cast<E>(25);
      });
      static_assert(result == ::std::tuple<int>{25});
   }
   {
      using T = Types<int, float>;
      constexpr auto result = T::GenerateData([]<class E> {
         return static_cast<E>(25);
      });
      static_assert(result == ::std::tuple<int, float>{25, 25.0f});
   }
   {
      using T = Types<int const, float, bool>;
      constexpr auto result = T::GenerateData([]<class E> {
         return static_cast<E>(25);
      });
      static_assert(result == ::std::tuple<int const, float, bool>{25, 25.0f, true});
   }
   {
      using T = Types<int const, float, bool, Types<double>>;
      constexpr auto result = T::GenerateData([]<class E> {
         if constexpr (CT::Typelist<E>)
            return E{};
         else 
            return static_cast<E>(25);
      });
      static_assert(result == ::std::tuple<int const, float, bool, Types<double>>{25, 25.0f, true, {}});
   }
}

SCENARIO("Testing Types::GenerateDataOptimized") {
   {
      using T = Types<>;
      constexpr auto result = T::GenerateDataOptimized([]<class E> {
         return static_cast<E>(25);
      });
      static_assert(result == compact_tuple<>{});
   }
   {
      using T = Types<int>;
      constexpr auto result = T::GenerateDataOptimized([]<class E> {
         return static_cast<E>(25);
      });
      static_assert(result == compact_tuple<int>{25});
   }
   {
      using T = Types<int, float>;
      constexpr auto result = T::GenerateDataOptimized([]<class E> {
         return static_cast<E>(25);
      });
      static_assert(result == compact_tuple<int, float>{25, 25.0f});
   }
   {
      using T = Types<int const, float, bool>;
      constexpr auto result = T::GenerateDataOptimized([]<class E> {
         return static_cast<E>(25);
      });
      static_assert(result == compact_tuple<int const, float, bool>{25, 25.0f, true});
   }
   {
      using T = Types<int const, float, bool, Types<double>>;
      constexpr auto result = T::GenerateDataOptimized([]<class E> {
         if constexpr (CT::Typelist<E>)
            return E{};
         else
            return static_cast<E>(25);
      });
      static_assert(result == compact_tuple<int const, float, bool, Types<double>>{25, 25.0f, true, {}});
   }
}

SCENARIO("Testing Types::Contains") {
   {
      using T = Types<>;
      static_assert(not T::template Contains<void>);
      static_assert(not T::template Contains<int>);
   }
   {
      using T = Types<int>;
      static_assert(not T::template Contains<void>);
      static_assert(not T::template Contains<float>);
      static_assert(    T::template Contains<int>);
   }
   {
      using T = Types<int, float>;
      static_assert(not T::template Contains<void>);
      static_assert(    T::template Contains<float>);
      static_assert(    T::template Contains<int>);
      static_assert(not T::template Contains<bool>);
   }
   {
      using T = Types<int const, float, bool>;
      static_assert(not T::template Contains<void>);
      static_assert(    T::template Contains<float>);
      static_assert(not T::template Contains<int>);
      static_assert(    T::template Contains<int const>);
      static_assert(    T::template Contains<bool>);
   }
   {
      using T = Types<int const, float, bool, Types<double>>;
      static_assert(not T::template Contains<void>);
      static_assert(    T::template Contains<float>);
      static_assert(not T::template Contains<int>);
      static_assert(    T::template Contains<int const>);
      static_assert(    T::template Contains<bool>);
      static_assert(    T::template Contains<Types<double>>);
      static_assert(not T::template Contains<double>);
   }
}

SCENARIO("Testing Types::operator +") {
   {
      using T1 = Types<>;
      using T2 = Types<>;
      constexpr auto result = T1{} + T2{};
      static_assert(::std::same_as<decltype(result), const Types<>>);
   }
   {
      using T1 = Types<>;
      using T2 = Types<int>;
      constexpr auto result = T1{} + T2{};
      static_assert(::std::same_as<decltype(result), const Types<int>>);
   }
   {
      using T1 = Types<>;
      using T2 = Types<int, const float>;
      constexpr auto result = T1{} + T2{};
      static_assert(::std::same_as<decltype(result), const Types<int, const float>>);
   }
   {
      using T1 = Types<bool>;
      using T2 = Types<int, const float>;
      constexpr auto result = T1{} + T2{};
      static_assert(::std::same_as<decltype(result), const Types<bool, int, const float>>);
   }
   {
      using T1 = Types<bool>;
      using T2 = Types<int, const float, Types<bool>>;
      constexpr auto result = T1{} + T2{};
      static_assert(::std::same_as<decltype(result), const Types<bool, int, const float, Types<bool>>>);
   }
   {
      using T1 = Types<bool, bool&, void, void>;
      using T2 = Types<int, const float, Types<bool>>;
      constexpr auto result = T1{} + T2{};
      static_assert(::std::same_as<decltype(result), const Types<bool, bool&, void, void, int, const float, Types<bool>>>);
   }
}
