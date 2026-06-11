///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "../Main.hpp"
#include "../TestTypes/CommonTypes.hpp"
#include <Langulus/CT/Deep.hpp>

using namespace Langulus;


///                                                                           
/// CT::Deep                                                                  
///                                                                           
TEST_CASE_TEMPLATE("Testing CT::Deep types", TestType
   //, IncompleteType                 // shouldn't compile
   //, IncompleteType const           // shouldn't compile
   //, SheddableType<IncompleteType>  // shouldn't compile
   //, IncompleteType*                // shouldn't compile
   //, IncompleteType const*          // shouldn't compile
   //, SheddableType<IncompleteType*> // shouldn't compile
   
   , ForcedDeepExternally
   , ForcedDeepExternally const
   , ForcedDeepExternally&
   , ForcedDeepInternally
   , ForcedDeepInternally const
   , ForcedDeepInternally&
   , InheritedDeep1
   , InheritedDeep1 const
   , InheritedDeep1&

   , ForcedDeepExternally*
   , ForcedDeepExternally const*
   , ForcedDeepExternally*&
   , ForcedDeepInternally*
   , ForcedDeepInternally const*
   , ForcedDeepInternally*&
   , InheritedDeep1*
   , InheritedDeep1 const*
   , InheritedDeep1*&
) {
   static_assert(    CT::Deep<TestType>);
   static_assert(not CT::NotDeep<TestType>);
}

TEST_CASE_TEMPLATE("Testing CT::NotDeep types", TestType
   //, IncompleteType                // shouldn't compile
   //, IncompleteType const          // shouldn't compile
   //, SheddableType<IncompleteType> // shouldn't compile
   //, IncompleteType*
   , void
   , void*
   , InheritedDeep1ButPrivate
   , InheritedDeepExternally
   , int
   , int const
   , int const&
   , int&
   , Types<void*>
   , InheritedDeep1Disabled
) {
   static_assert(not CT::Deep<TestType>);
   static_assert(    CT::NotDeep<TestType>);
}

//static_assert(    CT::Deep<>); // shouldn't compile at all
static_assert(    CT::Deep<ForcedDeepExternally, ForcedDeepInternally>);
static_assert(not CT::Deep<ForcedDeepExternally, int>);

//static_assert(    CT::NotDeep<>); // shouldn't compile at all
static_assert(    CT::NotDeep<void*, int>);
static_assert(not CT::NotDeep<void*, ForcedDeepInternally>);


///                                                                           
/// CT::DeepDense                                                             
///                                                                           
TEST_CASE_TEMPLATE("Testing CT::DeepDense types", TestType
   //, IncompleteType                 // shouldn't compile
   //, IncompleteType const           // shouldn't compile
   //, SheddableType<IncompleteType>  // shouldn't compile
   //, IncompleteType*                // shouldn't compile
   //, IncompleteType const*          // shouldn't compile
   //, SheddableType<IncompleteType*> // shouldn't compile
   
   , ForcedDeepExternally
   , ForcedDeepExternally const
   , ForcedDeepExternally&
   , ForcedDeepInternally
   , ForcedDeepInternally const
   , ForcedDeepInternally&
   , InheritedDeep1
   , InheritedDeep1 const
   , InheritedDeep1&
) {
   static_assert(    CT::DeepDense<TestType>);
}

TEST_CASE_TEMPLATE("Testing not CT::DeepDense types", TestType
   //, IncompleteType                // shouldn't compile
   //, IncompleteType const          // shouldn't compile
   //, SheddableType<IncompleteType> // shouldn't compile
   //, IncompleteType*
   , void
   , void*
   , InheritedDeep1ButPrivate
   , InheritedDeepExternally
   , int
   , int const
   , int const&
   , int&
   , Types<void*>
   , InheritedDeep1Disabled

   , ForcedDeepExternally*
   , ForcedDeepExternally const*
   , ForcedDeepExternally*&
   , ForcedDeepInternally*
   , ForcedDeepInternally const*
   , ForcedDeepInternally*&
   , InheritedDeep1*
   , InheritedDeep1 const*
   , InheritedDeep1*&
) {
   static_assert(not CT::DeepDense<TestType>);
}

//static_assert(    CT::DeepDense<>); // shouldn't compile at all
static_assert(    CT::DeepDense<ForcedDeepExternally, ForcedDeepInternally>);
static_assert(not CT::DeepDense<ForcedDeepExternally, int>);