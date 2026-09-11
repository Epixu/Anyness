///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "../Main.hpp"
#include "../TestTypes/CommonTypes.hpp"
#include <Langulus/CT/POD.hpp>

using namespace Langulus;


///                                                                           
/// CT::POD                                                                   
///                                                                           
TEST_CASE_TEMPLATE("Testing CT::POD types", TestType
   //, IncompleteType                // shouldn't compile
   //, IncompleteType const          // shouldn't compile
   //, SheddableType<IncompleteType> // shouldn't compile
   , SheddableType<Pi>
   , SheddableType<Pi const>
   , SheddableType<Pi const&>
   , SheddableType<ForcefullyPod>
   , SheddableType<ForcefullyPod const>
   , SheddableType<ForcefullyPod const&>
   , SheddableType<PureAbstract*>
   , SheddableType<PureAbstract* const>
   , SheddableType<PureAbstract* const&>
   , EmptyType
   , ActualAggregateType
   , Pi, Pi const, Pi&, int, int const, int&, void*, void
   , Types<>, Types<int>
   , ArrayType, ArrayType2, ArrayTypeRef, ArrayTypeRef2
   , PointerType, PointerType2, PureAbstract*, NonDestructible*, NonAggregateTypeDerived*
   , PrivatelyConstructible
   , ForcefullyPod
   , ForcefullyPod[5]
   , ForcefullyPod[5][5]
) {
   static_assert(    CT::POD<TestType>);
   static_assert(not CT::NotPOD<TestType>);
}

TEST_CASE_TEMPLATE("Testing CT::NotPOD types", TestType
   //, IncompleteType                     // shouldn't compile
   //, IncompleteType const               // shouldn't compile
   //, SheddableType<IncompleteType>      // shouldn't compile
   //, SheddableType<PureAbstract>        // shouldn't compile
   //, SheddableType<PureAbstract const>  // shouldn't compile
   , SheddableType<PureAbstract const&>
   , SheddableType<Complex[5]>
   , SheddableType<Complex[5][5]>
   , PureAbstract
   , PureAbstract const
   , PureAbstract const&
   , ForcedAbstractInternally
   , ForcedAbstractExternally
   , ForcedAbstractExternally[5]
   , ForcedAbstractExternally[5][5]
   , NonAggregateTypeDerived
   , NonDestructible, DestructibleType
   , NonIntentConstructible
   , PartiallyIntentConstructible
   , PartiallyIntentConstructibleButImplicitly
   , Complex, ContainsComplex, AggregateTypeComplex
   , Complex[5]
   , Complex[5][5]
   , CustomAggregateType, AggregateTypeDerived, NonAggregateTypeDerived
) {
   static_assert(not CT::POD<TestType>);
   static_assert(    CT::NotPOD<TestType>);
}

//static_assert(    CT::POD<>); // shouldn't compile at all
static_assert(    CT::POD<ForcefullyPod, ForcefullyPod[5], int, int*>);
static_assert(not CT::POD<ForcefullyPod, ForcefullyPod[5], int, int*, PureAbstract>);

//static_assert(    CT::NotPOD<>); // shouldn't compile at all
static_assert(    CT::NotPOD<Complex, SheddableType<Complex>, Complex[5][5]>);
static_assert(not CT::NotPOD<Complex, SheddableType<Complex>, Complex[5][5], int>);
