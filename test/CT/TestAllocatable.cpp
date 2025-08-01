///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "../Main.hpp"
#include <Langulus/CT/Allocatable.hpp>

using namespace Langulus;

namespace
{
   template<class T>
   struct SheddableType { using CTTI_Sheddable = T; };
   struct IncompleteType;

   struct AllocatableExternally {};
   struct AllocatableExternallyDisabled {};
   struct AllocatableInternally { using CTTI_Allocatable = Yes<>; };
   struct InheritedAllocatable : AllocatableInternally {};
   struct InheritedAllocatableDisabled : AllocatableInternally { using CTTI_Allocatable = No; };
   struct InheritedAllocatableButPrivate : private AllocatableInternally {};
   struct InheritedAllocatableExternally : AllocatableExternally {};
}

namespace Langulus::CTTI
{
   template<>
   struct Allocatable<AllocatableExternally> {};
   template<>
   struct Allocatable<AllocatableExternallyDisabled> {
      static constexpr bool Enabled = false;
   };
}


///                                                                           
/// CT::Allocatable                                                           
///                                                                           
TEMPLATE_TEST_CASE("Testing CT::Allocatable types", "[ct]",
   //IncompleteType,                // shouldn't compile
   //IncompleteType const,          // shouldn't compile
   //IncompleteType const&,         // shouldn't compile
   //SheddableType<IncompleteType>, // shouldn't compile
   //SheddableType<IncompleteType>&,// shouldn't compile
   AllocatableExternally,
   AllocatableExternally const,
   AllocatableExternally&,
   AllocatableInternally,
   AllocatableInternally const,
   AllocatableInternally&,
   SheddableType<AllocatableInternally*>,
   SheddableType<AllocatableInternally* const>,
   SheddableType<AllocatableInternally* const&>,
   InheritedAllocatable,
   InheritedAllocatable const,
   InheritedAllocatable&,
   InheritedAllocatableButPrivate,
   InheritedAllocatableExternally,
   void*,
   int, int const, int const&, int&,
   Types<void*>,
   SheddableType<InheritedAllocatableDisabled*>,
   SheddableType<InheritedAllocatableDisabled* const>,
   SheddableType<InheritedAllocatableDisabled* const&>,
   IncompleteType*
) {
   static_assert(    CT::Allocatable<TestType>);
   static_assert(not CT::NotAllocatable<TestType>);
}

TEMPLATE_TEST_CASE("Testing CT::NotAllocatable types", "[ct]",
   //IncompleteType,                // shouldn't compile
   //IncompleteType const,          // shouldn't compile
   //IncompleteType const&,         // shouldn't compile
   //SheddableType<IncompleteType>, // shouldn't compile
   //SheddableType<IncompleteType>&,// shouldn't compile
   void,
   SheddableType<InheritedAllocatableDisabled>,
   SheddableType<InheritedAllocatableDisabled const>,
   SheddableType<InheritedAllocatableDisabled const&>,
   InheritedAllocatableDisabled,
   AllocatableExternallyDisabled
) {
   static_assert(not CT::Allocatable<TestType>);
   static_assert(    CT::NotAllocatable<TestType>);
}

//static_assert(    CT::Allocatable<>); // shouldn't compile at all
static_assert(    CT::Allocatable<AllocatableExternally, AllocatableInternally, int>);
static_assert(not CT::Allocatable<AllocatableExternally, AllocatableInternally, InheritedAllocatableDisabled>);

//static_assert(    CT::NotAllocatable<>); // shouldn't compile at all
static_assert(    CT::NotAllocatable<void, InheritedAllocatableDisabled, AllocatableExternallyDisabled>);
static_assert(not CT::NotAllocatable<void, InheritedAllocatableDisabled, int>);
