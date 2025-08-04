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

   struct BoolExternally {};
   struct NotBoolExternally {};
   struct BoolInternally { using CTTI_Bool = Yes<>; };
   struct InheritedBool : BoolInternally {};
   struct InheritedBoolDisabled : BoolInternally { using CTTI_Bool = No; };
   struct InheritedBoolButPrivate : private BoolInternally {};
   struct InheritedBoolExternally : BoolExternally {};
}

namespace Langulus::CTTI
{
   template<>
   struct Bool<BoolExternally> {};
   template<>
   struct Bool<NotBoolExternally> {
      static constexpr bool Enabled = false;
   };
}


///                                                                           
/// CT::Bool                                                                  
///                                                                           
TEMPLATE_TEST_CASE("Testing CT::Bool types", "[ct]",
   //IncompleteType,                // shouldn't compile
   //IncompleteType const,          // shouldn't compile
   //IncompleteType const&,         // shouldn't compile
   //SheddableType<IncompleteType>, // shouldn't compile
   //SheddableType<IncompleteType>&,// shouldn't compile
   BoolExternally,
   BoolExternally const,
   BoolExternally&,
   BoolInternally,
   BoolInternally const,
   BoolInternally&,
   InheritedBool,
   InheritedBool const,
   InheritedBool&,
   bool
) {
   static_assert(    CT::Bool<TestType>);
   static_assert(not CT::NotBool<TestType>);
}

TEMPLATE_TEST_CASE("Testing CT::NotBool types", "[ct]",
   //IncompleteType,                // shouldn't compile
   //IncompleteType const,          // shouldn't compile
   //IncompleteType const&,         // shouldn't compile
   //SheddableType<IncompleteType>, // shouldn't compile
   //SheddableType<IncompleteType>&,// shouldn't compile
   IncompleteType*,
   void, void*,
   int, int const, int const&, int&,
   Types<void*>,
   SheddableType<BoolInternally*>,
   SheddableType<BoolInternally* const>,
   SheddableType<BoolInternally* const&>,
   SheddableType<InheritedBoolDisabled>,
   SheddableType<InheritedBoolDisabled const>,
   SheddableType<InheritedBoolDisabled const&>,
   InheritedBoolDisabled,
   InheritedBoolExternally,
   InheritedBoolButPrivate,
   NotBoolExternally
) {
   static_assert(not CT::Bool<TestType>);
   static_assert(    CT::NotBool<TestType>);
}

//static_assert(    CT::Bool<>); // shouldn't compile at all
static_assert(    CT::Bool<BoolExternally, BoolInternally, bool>);
static_assert(not CT::Bool<BoolExternally, BoolInternally, int>);

//static_assert(    CT::NotBool<>); // shouldn't compile at all
static_assert(    CT::NotBool<InheritedBoolDisabled, InheritedBoolExternally, int>);
static_assert(not CT::NotBool<InheritedBoolDisabled, InheritedBoolExternally, bool>);
