///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "Main.hpp"
#include <Langulus/Types.hpp>

using namespace Langulus;


///                                                                           
/// CT::Void                                                                  
///                                                                           
namespace
{
   // ReSharper disable once CppTypeAliasNeverUsed
   struct VoidType { using CTTI_Void = Yes<>; };
   struct VoidTypeDerived : VoidType {};
   struct VoidTypeExternal {};
   // ReSharper disable once CppTypeAliasNeverUsed
   struct NonVoidTypeDerived : VoidType { using CTTI_Void = No; };
   struct IncompleteType;
}

namespace Langulus::CTTI
{
   template<>
   struct Void<VoidTypeExternal> {};
}

TEMPLATE_TEST_CASE("Testing void types", "[ct]",
   void,
   VoidType,
   VoidType const,
   VoidType&,
   VoidTypeDerived,
   VoidTypeDerived const,
   VoidTypeDerived&,
   VoidTypeExternal,
   VoidTypeExternal const,
   VoidTypeExternal&,
   Types<>
) {
   static_assert(    CT::Void<TestType>);
   static_assert(not CT::NotVoid<TestType>);
}

TEMPLATE_TEST_CASE("Testing non-void types", "[ct]",
   void*,
   VoidType*,
   NonVoidTypeDerived,
   NonVoidTypeDerived const,
   NonVoidTypeDerived*,
   //IncompleteType,         // shouldn't compile
   //IncompleteType const,   // shouldn't compile
   IncompleteType*,
   int,
   int const,
   int const&,
   int&,
   Types<void>,
   Types<void*>,
   (Types<void, void>)
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
   // ReSharper disable once CppTypeAliasNeverUsed
   struct CustomTypelist { using CTTI_Typelist = Yes<>; };
   struct CustomTypelistDerived : CustomTypelist {};
   struct CustomTypelistExternal {};
   // ReSharper disable once CppTypeAliasNeverUsed
   struct CustomNonTypelistDerived : CustomTypelist { using CTTI_Typelist = No; };
}

namespace Langulus::CTTI
{
   template<>
   struct Typelist<CustomTypelistExternal> {};
}

TEMPLATE_TEST_CASE("Testing typelists", "[ct]",
   Types<>,
   Types<void>,
   (Types<void, void>),
   Types<int>,
   (Types<int, float>),
   CustomTypelist,
   CustomTypelist const,
   CustomTypelist&,
   CustomTypelistDerived,
   CustomTypelistDerived const,
   CustomTypelistDerived&,
   CustomTypelistExternal,
   CustomTypelistExternal const,
   CustomTypelistExternal&
) {
   static_assert(    CT::Typelist<TestType>);
   static_assert(not CT::NotTypelist<TestType>);
}

TEMPLATE_TEST_CASE("Testing non-typelists", "[ct]",
   void,
   void*,
   CustomTypelist*,
   CustomNonTypelistDerived,
   CustomNonTypelistDerived const,
   CustomNonTypelistDerived*,
   //IncompleteType,         // shouldn't compile
   //IncompleteType const,   // shouldn't compile
   IncompleteType*,
   int,
   int const,
   int const&,
   int&
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


SCENARIO("Types", "[types]") {

}
