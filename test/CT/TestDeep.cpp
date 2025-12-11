///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "../Main.hpp"
#include <Langulus/CT/Deep.hpp>

using namespace Langulus;

namespace
{
   template<class T>
   struct SheddableType { using CTTI_Sheddable = T; };

   struct IncompleteType;

   /// Proper type, reflected as abstract                                     
   struct ForcedDeepExternally {};
   struct ForcedDeepInternally {
      // ReSharper disable once CppTypeAliasNeverUsed
      using CTTI_Deep = Yes<>;
   };

   /// Types that can inherit deepness                                        
   struct InheritedDeep1 : ForcedDeepInternally {};
   struct InheritedDeep1Disabled : ForcedDeepInternally { using CTTI_Deep = No; };
   struct InheritedDeep1ButPrivate : private ForcedDeepInternally {};
   struct InheritedDeepExternally : ForcedDeepExternally {};
}

namespace Langulus::CTTI
{
   template<>
   struct Deep<ForcedDeepExternally> {};
}


///                                                                           
/// CT::Deep                                                                  
///                                                                           
TEMPLATE_TEST_CASE("Testing CT::Deep types", "[ct]"
   //IncompleteType,                 // shouldn't compile
   //IncompleteType const,           // shouldn't compile
   //SheddableType<IncompleteType>,  // shouldn't compile
   //IncompleteType*,                // shouldn't compile
   //IncompleteType const*,          // shouldn't compile
   //SheddableType<IncompleteType*>, // shouldn't compile
   
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

TEMPLATE_TEST_CASE("Testing CT::NotDeep types", "[ct]"
   //IncompleteType,                // shouldn't compile
   //IncompleteType const,          // shouldn't compile
   //SheddableType<IncompleteType>, // shouldn't compile
   //IncompleteType*,
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
