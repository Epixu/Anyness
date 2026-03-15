///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "../Main.hpp"
#include <Langulus/CT/Bool.hpp>

using namespace Langulus;

namespace
{
   template<class T>
   struct SheddableType { using CTTI_Sheddable = T; };
   struct IncompleteType;
}


///                                                                           
/// Akin/AkinAsOneOf                                                          
///                                                                           
TEST_CASE_TEMPLATE("Testing Akin/AkinAsOneOf", TestPair
   , Types<IncompleteType, IncompleteType>
   , Types<IncompleteType, IncompleteType const>
   , Types<IncompleteType, IncompleteType const&>
   , Types<IncompleteType, IncompleteType const*>
   , Types<IncompleteType, IncompleteType const*&>
   , Types<IncompleteType, IncompleteType const* const&>
   , Types<IncompleteType, IncompleteType const* const*>
   , Types<IncompleteType, IncompleteType const* const*&>
   , Types<IncompleteType, IncompleteType const* const* const&>
   , Types<IncompleteType, SheddableType<IncompleteType>>
   , Types<IncompleteType, SheddableType<IncompleteType>*>
   , Types<IncompleteType, SheddableType<IncompleteType> const*&>
   , Types<IncompleteType, SheddableType<IncompleteType>>
   , Types<IncompleteType, SheddableType<IncompleteType*>>
   , Types<IncompleteType, SheddableType<IncompleteType const**&> const*&>
   , Types<SheddableType<IncompleteType>, SheddableType<IncompleteType>>
   , Types<SheddableType<IncompleteType>, SheddableType<IncompleteType>*>
   , Types<SheddableType<IncompleteType>, SheddableType<IncompleteType> const*&>
   , Types<bool, bool>
   , Types<bool, bool const>
   , Types<bool, bool const****&>
   , Types<void, void>
   , Types<void, void const*>
   , Types<void, void const**const*const*&>
   , Types<int*, int const*>
   , Types<int*, int const* const>
   , Types<int*, int const* const*>
) {
   using T1 = typename TestPair::First;
   using T2 = typename TestPair::Second;
   static_assert(Akin       <T1, T2>);
   static_assert(AkinAsOneOf<T1, T2>);

   if constexpr (CT::Complete<Shed<T1>, Shed<T2>>) {
      if constexpr (CT::NotVoid<Shed<T1>, Shed<T2>>)
         REQUIRE(MetaDataOf<T1>().Is(MetaDataOf<T2>()));
   }
}

TEST_CASE_TEMPLATE("Testing not Akin/AkinAsOneOf", TestPair
   , Types<IncompleteType, void>
   , Types<IncompleteType, bool>
   , Types<bool, int>
   , Types<bool, void>
) {
   using T1 = typename TestPair::First;
   using T2 = typename TestPair::Second;
   static_assert(not Akin       <T1, T2>);
   static_assert(not AkinAsOneOf<T1, T2>);

   if constexpr (CT::Complete<Shed<T1>, Shed<T2>>) {
      if constexpr (CT::NotVoid<Shed<T1>, Shed<T2>>)
         REQUIRE(not MetaDataOf<T1>().Is(MetaDataOf<T2>()));
   }
}

//static_assert(    Akin<>);    // shouldn't compile at all
//static_assert(    Akin<int>); // shouldn't compile at all
static_assert(    Akin<int, int const, int const*, int const**>);
static_assert(not Akin<int, bool, void, void*>);

//static_assert(    AkinAsOneOf<>);    // shouldn't compile at all
//static_assert(    AkinAsOneOf<int>); // shouldn't compile at all
static_assert(    AkinAsOneOf<int, bool, void, void*, int***>);
static_assert(not AkinAsOneOf<int, bool, void, void*>);


///                                                                           
/// Same/SameAsOneOf                                                          
///                                                                           
TEST_CASE_TEMPLATE("Testing Same/SameAsOneOf", TestPair
   , Types<int*, int const*>
   , Types<int*, int const* const>
   , Types<int*, int const*&>
   , Types<int*, int const* const&>
   , Types<int*, int const*&&>
   , Types<int*, int const* const&&>
   , Types<IncompleteType, IncompleteType>
   , Types<IncompleteType, IncompleteType const>
   , Types<IncompleteType, IncompleteType const&>
   , Types<SheddableType<IncompleteType>, SheddableType<IncompleteType>>
   , Types<bool, bool>
   , Types<bool, bool const>
   , Types<void, void>
) {
   using T1 = typename TestPair::First;
   using T2 = typename TestPair::Second;
   static_assert(Same       <T1, T2>);
   static_assert(SameAsOneOf<T1, T2>);

   if constexpr (CT::Complete<Shed<T1>, Shed<T2>>) {
      if constexpr (CT::NotVoid<Shed<T1>, Shed<T2>>)
         REQUIRE(MetaDataOf<T1>().IsSame(MetaDataOf<T2>()));
   }
}

TEST_CASE_TEMPLATE("Testing not Same/SameAsOneOf", TestPair
   , Types<IncompleteType, void>
   , Types<IncompleteType, bool>
   , Types<IncompleteType, IncompleteType const*>
   , Types<IncompleteType, IncompleteType const*&>
   , Types<IncompleteType, IncompleteType const* const&>
   , Types<IncompleteType, IncompleteType const* const*>
   , Types<IncompleteType, IncompleteType const* const*&>
   , Types<IncompleteType, IncompleteType const* const* const&>
   , Types<IncompleteType, SheddableType<IncompleteType>>
   , Types<IncompleteType, SheddableType<IncompleteType>*>
   , Types<IncompleteType, SheddableType<IncompleteType> const*&>
   , Types<IncompleteType, SheddableType<IncompleteType>>
   , Types<IncompleteType, SheddableType<IncompleteType*>>
   , Types<IncompleteType, SheddableType<IncompleteType const**&> const*&>
   , Types<SheddableType<IncompleteType>, SheddableType<IncompleteType>*>
   , Types<SheddableType<IncompleteType>, SheddableType<IncompleteType> const*&>
   , Types<bool, int>
   , Types<bool, void>
   , Types<bool, bool const****&>
   , Types<void, void const*>
   , Types<void, void const** const* const*&>
   , Types<int*, int**>
) {
   using T1 = typename TestPair::First;
   using T2 = typename TestPair::Second;
   static_assert(not Same       <T1, T2>);
   static_assert(not SameAsOneOf<T1, T2>);

   if constexpr (CT::Complete<Shed<T1>, Shed<T2>>) {
      if constexpr (CT::NotVoid<Shed<T1>, Shed<T2>>)
         REQUIRE(not MetaDataOf<T1>().IsSame(MetaDataOf<T2>()));
   }
}

//static_assert(    Same<>);    // shouldn't compile at all
//static_assert(    Same<int>); // shouldn't compile at all
static_assert(    Same<int, int const, int const&, volatile int const&>);
static_assert(not Same<int, int const, int const&, volatile int const&, int*>);

//static_assert(    SameAsOneOf<>);    // shouldn't compile at all
//static_assert(    SameAsOneOf<int>); // shouldn't compile at all
static_assert(    SameAsOneOf<int, bool, void, void*, int***, int const&>);
static_assert(not SameAsOneOf<int, bool, void, void*, int***, int*>);


///                                                                           
/// Exact/ExactAsOneOf                                                        
///                                                                           
TEST_CASE_TEMPLATE("Testing Exact/ExactAsOneOf", TestPair
   , Types<IncompleteType, IncompleteType>
   , Types<IncompleteType, IncompleteType&>
   , Types<SheddableType<IncompleteType>, SheddableType<IncompleteType>>
   , Types<bool, bool>
   , Types<bool, bool&>
   , Types<void, void>
   , Types<int*, int*&>
) {
   using T1 = typename TestPair::First;
   using T2 = typename TestPair::Second;
   static_assert(Exact       <T1, T2>);
   static_assert(ExactAsOneOf<T1, T2>);

   if constexpr (CT::Complete<Shed<T1>, Shed<T2>>) {
      if constexpr (CT::NotVoid<Shed<T1>, Shed<T2>>)
         REQUIRE(MetaDataOf<T1>().IsExact(MetaDataOf<T2>()));
   }
}

TEST_CASE_TEMPLATE("Testing not Exact/ExactAsOneOf", TestPair
   , Types<bool, bool const>
   , Types<int*, int const*>
   , Types<int*, int const* const>
   , Types<IncompleteType, IncompleteType const>
   , Types<IncompleteType, IncompleteType const&>
   , Types<IncompleteType, void>
   , Types<IncompleteType, bool>
   , Types<IncompleteType, IncompleteType const*>
   , Types<IncompleteType, IncompleteType const*&>
   , Types<IncompleteType, IncompleteType const* const&>
   , Types<IncompleteType, IncompleteType const* const*>
   , Types<IncompleteType, IncompleteType const* const*&>
   , Types<IncompleteType, IncompleteType const* const* const&>
   , Types<IncompleteType, SheddableType<IncompleteType>>
   , Types<IncompleteType, SheddableType<IncompleteType>*>
   , Types<IncompleteType, SheddableType<IncompleteType> const*&>
   , Types<IncompleteType, SheddableType<IncompleteType>>
   , Types<IncompleteType, SheddableType<IncompleteType*>>
   , Types<IncompleteType, SheddableType<IncompleteType const**&> const*&>
   , Types<SheddableType<IncompleteType>, SheddableType<IncompleteType>*>
   , Types<SheddableType<IncompleteType>, SheddableType<IncompleteType> const*&>
   , Types<bool, int>
   , Types<bool, void>
   , Types<bool, bool const****&>
   , Types<void, void const*>
   , Types<void, void const** const* const*&>
   , Types<int*, int**>
) {
   using T1 = typename TestPair::First;
   using T2 = typename TestPair::Second;
   static_assert(not Exact       <T1, T2>);
   static_assert(not ExactAsOneOf<T1, T2>);

   if constexpr (CT::Complete<Shed<T1>, Shed<T2>>) {
      if constexpr (CT::NotVoid<Shed<T1>, Shed<T2>>)
         REQUIRE(not MetaDataOf<T1>().IsExact(MetaDataOf<T2>()));
   }
}

//static_assert(    Exact<>);    // shouldn't compile at all
//static_assert(    Exact<int>); // shouldn't compile at all
static_assert(    Exact<int, int, int&, int&&>);
static_assert(not Exact<int, int, int&, int&&, int const>);

//static_assert(    ExactAsOneOf<>);    // shouldn't compile at all
//static_assert(    ExactAsOneOf<int>); // shouldn't compile at all
static_assert(    ExactAsOneOf<int, bool, void, void*, int***, int&>);
static_assert(not ExactAsOneOf<int, bool, void, void*, int***, int const>);
