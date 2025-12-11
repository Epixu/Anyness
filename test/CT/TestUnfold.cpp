///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "../Main.hpp"
#include <Langulus/CT/Unfold.hpp>

using namespace Langulus;

namespace
{
   template<class T>
   struct SheddableType { using CTTI_Sheddable = T; };
   template<class T>
   struct UnfoldableType { using CTTI_Unfoldable = Yes<>; };
   struct IncompleteType;

   struct UnfoldableExternally {};
   struct NotUnfoldableExternally {};
   struct UnfoldableInternally { using CTTI_Unfoldable = Yes<>; };
   struct InheritedUnfoldable : UnfoldableInternally {};
   struct InheritedUnfoldableDisabled : UnfoldableInternally { using CTTI_Unfoldable = No; };
   struct InheritedUnfoldableButPrivate : private UnfoldableInternally {};
   struct InheritedUnfoldableExternally : UnfoldableExternally {};
}

namespace Langulus::CTTI
{
   template<>
   struct Unfoldable<UnfoldableExternally> {};
   template<>
   struct Unfoldable<NotUnfoldableExternally> {
      static constexpr bool Enabled = false;
   };
}


///                                                                           
/// CT::Unfoldable                                                            
///                                                                           
TEMPLATE_TEST_CASE("Testing CT::Unfoldable types", "[ct]"
   //IncompleteType,                // shouldn't compile
   //IncompleteType const,          // shouldn't compile
   //IncompleteType const&,         // shouldn't compile
   , SheddableType<IncompleteType>  // shouldn't compile
   , SheddableType<IncompleteType>& // shouldn't compile
   , SheddableType<UnfoldableInternally*>
   , SheddableType<UnfoldableInternally* const>
   , SheddableType<UnfoldableInternally* const&>
   , SheddableType<InheritedUnfoldableDisabled>
   , SheddableType<InheritedUnfoldableDisabled const>
   , SheddableType<InheritedUnfoldableDisabled const&>
   , UnfoldableExternally
   , UnfoldableExternally const
   , UnfoldableExternally&
   , UnfoldableInternally
   , UnfoldableInternally const
   , UnfoldableInternally&
   , InheritedUnfoldable
   , InheritedUnfoldable const
   , InheritedUnfoldable&
) {
   static_assert(    CT::Unfoldable<TestType>);
   static_assert(not CT::NotUnfoldable<TestType>);
}

TEMPLATE_TEST_CASE("Testing CT::NotUnfoldable types", "[ct]"
   //IncompleteType,                // shouldn't compile
   //IncompleteType const,          // shouldn't compile
   //IncompleteType const&,         // shouldn't compile
   , IncompleteType*
   , bool, bool*
   , void, void*
   , int, int const, int const&, int&
   , Types<void*>
   , InheritedUnfoldableDisabled
   , InheritedUnfoldableExternally
   , InheritedUnfoldableButPrivate
   , NotUnfoldableExternally
) {
   static_assert(not CT::Unfoldable<TestType>);
   static_assert(    CT::NotUnfoldable<TestType>);
}

//static_assert(    CT::Unfoldable<>); // shouldn't compile at all
static_assert(    CT::Unfoldable<UnfoldableExternally, UnfoldableInternally, InheritedUnfoldable>);
static_assert(not CT::Unfoldable<UnfoldableExternally, UnfoldableInternally, int>);

//static_assert(    CT::NotUnfoldable<>); // shouldn't compile at all
static_assert(    CT::NotUnfoldable<InheritedUnfoldableDisabled, InheritedUnfoldableExternally, int>);
static_assert(not CT::NotUnfoldable<InheritedUnfoldableDisabled, InheritedUnfoldableExternally, InheritedUnfoldable>);
