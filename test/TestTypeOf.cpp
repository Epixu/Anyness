///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "Main.hpp"
#include <Langulus/TypeOf.hpp>
#include <string_view>
#include <array>
#include <vector>

using namespace Langulus;


///                                                                           
/// CT::Typed / CT::Untyped                                                   
///                                                                           
namespace
{
   template<class T>
   struct SheddableType { using CTTI_Sheddable = Yes; using CTTI_Typed = T; };
   struct CustomTypedType { using CTTI_Typed = int; };
   struct CustomTypedTypeDerived : CustomTypedType { };
   struct CustomUntypedType : CustomTypedType { using CTTI_Typed = void; };
   enum TypedEnum : int64_t {one1, two2};
   enum class TypedEnumClass : int64_t {one1, two2};
   struct IncompleteType;
}

TEMPLATE_TEST_CASE("Testing typed type", "[concepts]",
   std::vector<bool>,
   std::string_view,
   (std::array<double, 5>),
   TypedEnum,
   TypedEnumClass,
   CustomTypedType,
   CustomTypedTypeDerived,
   SheddableType<TypedEnum>,
   SheddableType<int>
) {
   static_assert(    CT::Typed<TestType>);
   static_assert(not CT::Untyped<TestType>);
}

TEMPLATE_TEST_CASE("Testing untyped type", "[concepts]",
   CustomUntypedType,
   IncompleteType,
   void, int
) {
   static_assert(not CT::Typed<TestType>);
   static_assert(    CT::Untyped<TestType>);
}

//static_assert(CT::Typed<>); // shouldn't compile at all
static_assert(    CT::Typed<std::vector<bool>, CustomTypedType, TypedEnum>);
static_assert(not CT::Typed<std::vector<bool>, CustomTypedType, int>);

//static_assert(CT::Untyped<>); // shouldn't compile at all
static_assert(    CT::Untyped<CustomUntypedType, IncompleteType, int>);
static_assert(not CT::Untyped<CustomUntypedType, IncompleteType, TypedEnum>);
