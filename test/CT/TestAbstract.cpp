///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "../Main.hpp"
#include "../TestTypes/CommonTypes.hpp"
#include <Langulus/CT/Abstract.hpp>

using namespace Langulus;

//TODO test extents

///                                                                           
/// CT::Abstract                                                              
///                                                                           
TEST_CASE_TEMPLATE("Testing CT::Abstract types", TestType
   //, IncompleteType                     // shouldn't compile
   //, IncompleteType const               // shouldn't compile
   //, SheddableType<IncompleteType>      // shouldn't compile
   //, SheddableType<PureAbstract>        // shouldn't compile
   //, SheddableType<PureAbstract const>  // shouldn't compile
   , SheddableType<PureAbstract const&>
   , PureAbstract
   , PureAbstract const
   , PureAbstract&
   , ForcedAbstractExternally
   , ForcedAbstractExternally const
   , ForcedAbstractExternally&
   , ForcedAbstractInternally
   , ForcedAbstractInternally const
   , ForcedAbstractInternally&
   , InheritedAbstract1
   , InheritedAbstract1 const
   , InheritedAbstract1&
   , InheritedAbstract2ButPrivate
   , InheritedAbstract2
   , InheritedAbstract2 const
   , InheritedAbstract2&
) {
   static_assert(    CT::Abstract<TestType>);
   static_assert(not CT::NotAbstract<TestType>);
}

TEST_CASE_TEMPLATE("Testing CT::NotAbstract types", TestType
   //, IncompleteType                // shouldn't compile
   //, IncompleteType const          // shouldn't compile
   //, SheddableType<IncompleteType> // shouldn't compile
   , void
   , void*
   , SheddableType<PureAbstract*>
   , SheddableType<PureAbstract* const>
   , SheddableType<PureAbstract* const&>
   , PureAbstract*
   , ImpureVirtual
   , InheritedAbstract1ButPrivate
   , InheritedAbstractExternally
   , IncompleteType*
   , int
   , int const
   , int const&
   , int&
   , Types<void*>
   , InheritedAbstract1Disabled
) {
   static_assert(not CT::Abstract<TestType>);
   static_assert(    CT::NotAbstract<TestType>);
}

//static_assert(    CT::Abstract<>); // shouldn't compile at all
static_assert(    CT::Abstract<ForcedAbstractExternally, PureAbstract, ForcedAbstractInternally>);
static_assert(not CT::Abstract<ForcedAbstractExternally, PureAbstract, int>);

//static_assert(    CT::NotAbstract<>); // shouldn't compile at all
static_assert(    CT::NotAbstract<void*, ImpureVirtual, int>);
static_assert(not CT::NotAbstract<void*, ImpureVirtual, ForcedAbstractInternally>);
