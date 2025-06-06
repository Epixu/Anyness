///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "Main.hpp"
#include <Langulus/Typenav.hpp>
#include <Langulus/CT/Same.hpp>

using namespace Langulus;


///                                                                           
/// CT::Sheddable                                                             
///                                                                           
namespace
{
   template<class T>
   struct SheddableType { using CTTI_Sheddable = Yes; using CTTI_Typed = T; };
   struct SheddableTypeDerived : SheddableType<int&> {};
   struct NonSheddableTypeDerived : SheddableType<int&> { using CTTI_Sheddable = No; };
}

TEMPLATE_TEST_CASE("Testing sheddable types", "[ct]",
   SheddableType<int&>,
   SheddableTypeDerived,
   SheddableTypeDerived&
) {
   static_assert(    CT::Sheddable<TestType>);
   static_assert(not CT::NotSheddable<TestType>);
   static_assert(    CT::Exact<Shed<TestType>, int&>);
}

TEMPLATE_TEST_CASE("Testing non-sheddable types", "[ct]",
   SheddableType<int&>*,
   NonSheddableTypeDerived,
   NonSheddableTypeDerived&,
   int,
   int&
) {
   static_assert(not CT::Sheddable<TestType>);
   static_assert(    CT::NotSheddable<TestType>);
   static_assert(    CT::Exact<Shed<TestType>, TestType>);
}

//static_assert(CT::Sheddable<>); // shouldn't compile at all
static_assert(    CT::Sheddable<SheddableType<int&>, SheddableTypeDerived, SheddableTypeDerived&>);
static_assert(not CT::Sheddable<SheddableType<int&>, SheddableTypeDerived, NonSheddableTypeDerived>);

//static_assert(CT::NotSheddable<>); // shouldn't compile at all
static_assert(    CT::NotSheddable<SheddableType<int&>*, NonSheddableTypeDerived, int&>);
static_assert(not CT::NotSheddable<SheddableType<int&>*, NonSheddableTypeDerived, SheddableTypeDerived>);


///                                                                           
/// CT::Array                                                                 
///                                                                           
namespace
{

   using ArrayType = int[50];
   using ArrayType2 = int[50][2];
   using ArrayTypeRef = int(&)[50];
   using ArrayTypeRef2 = int(&)[50][2];
   using PointerType = int*;
   using PointerType2 = int**;
   struct CustomArrayType {
      using CTTI_Array = YesValue<56>;
   };
   struct CustomNonArrayType {};
}

TEMPLATE_TEST_CASE("Testing bounded array types", "[ct]",
   SheddableType<ArrayType>,
   ArrayType,
   ArrayType2,
   ArrayTypeRef,
   ArrayTypeRef2,
   CustomArrayType
) {
   static_assert(CT::Array<TestType>);
}

TEMPLATE_TEST_CASE("Testing non-array types", "[ct]",
   SheddableType<int>,
   PointerType,
   PointerType2,
   ArrayType*,
   CustomNonArrayType,
   int,
   int&
) {
   static_assert(not CT::Array<TestType>);
}

//static_assert(CT::Array<>); // shouldn't compile at all
static_assert(    CT::Array<ArrayType, ArrayType2, ArrayTypeRef>);
static_assert(not CT::Array<ArrayType, ArrayType2, CustomNonArrayType>);


///                                                                           
/// ExtentOf                                                                  
///                                                                           
SCENARIO("Getting the extent of bounded array types", "[ct]") {
   static_assert(ExtentOf<SheddableType<ArrayType>> == 50);
   static_assert(ExtentOf<ArrayType> == 50);
   static_assert(ExtentOf<ArrayType2> == 50);
   static_assert(ExtentOf<Deext<ArrayType2>> == 2);
   static_assert(ExtentOf<ArrayTypeRef> == 50);
   static_assert(ExtentOf<ArrayTypeRef2> == 50);
   static_assert(ExtentOf<Deext<ArrayTypeRef2>> == 2);
   static_assert(ExtentOf<PointerType> == 1);
   static_assert(ExtentOf<PointerType2> == 1);
   static_assert(ExtentOf<CustomArrayType> == 56);
   static_assert(ExtentOf<CustomNonArrayType> == 1);
}


///                                                                           
/// CT::Sparse / CT::Dense                                                    
///                                                                           
namespace
{
   struct CustomPointerType { using CTTI_Sparse = Yes; };
   struct CustomNonPointerType {};
}

TEMPLATE_TEST_CASE("Testing sparse types", "[ct]",
   SheddableType<PointerType>,
   PointerType,
   CustomPointerType,
   ::std::nullptr_t,
   void**
) {
   static_assert(    CT::Sparse<TestType>);
   static_assert(not CT::Dense<TestType>);
}

TEMPLATE_TEST_CASE("Testing dense types", "[ct]",
   SheddableType<CustomNonPointerType>,
   CustomNonPointerType,
   int, int&, void
) {
   static_assert(not CT::Sparse<TestType>);
   static_assert(    CT::Dense<TestType>);
}

//static_assert(CT::Sparse<>); // shouldn't compile at all
static_assert(    CT::Sparse<SheddableType<PointerType>, PointerType, CustomPointerType>);
static_assert(not CT::Sparse<SheddableType<PointerType>, PointerType, int>);

//static_assert(CT::Dense<>); // shouldn't compile at all
static_assert(    CT::Dense<SheddableType<CustomNonPointerType>, CustomNonPointerType, int>);
static_assert(not CT::Dense<SheddableType<CustomNonPointerType>, CustomNonPointerType, int*>);


///                                                                           
/// CT::Null                                                                  
///                                                                           
namespace
{
   struct NullType { using CTTI_Null = Yes; };
   struct NullTypeDerived : NullType {};
   struct NonNullTypeDerived : NullType { using CTTI_Null = No; };
}

TEMPLATE_TEST_CASE("Testing null types", "[ct]",
   SheddableType<NullType>,
   NullType,
   NullTypeDerived,
   NullTypeDerived&,
   ::std::nullptr_t
) {
   static_assert(    CT::Null<TestType>);
   static_assert(not CT::NotNull<TestType>);
}

TEMPLATE_TEST_CASE("Testing non-null types", "[ct]",
   SheddableType<NullType*>,
   NullType*,
   ::std::nullptr_t*,
   NonNullTypeDerived,
   NonNullTypeDerived&,
   SheddableType<NonNullTypeDerived&>,
   int,
   int&
) {
   static_assert(not CT::Null<TestType>);
   static_assert(    CT::NotNull<TestType>);
}

//static_assert(CT::Null<>); // shouldn't compile at all
static_assert(    CT::Null<SheddableType<NullType>, NullType, ::std::nullptr_t>);
static_assert(not CT::Null<SheddableType<NullType>, NullType, NonNullTypeDerived>);

//static_assert(CT::NotNull<>); // shouldn't compile at all
static_assert(    CT::NotNull<SheddableType<NullType*>, ::std::nullptr_t*, int>);
static_assert(not CT::NotNull<SheddableType<NullType*>, ::std::nullptr_t*, NullType>);


///                                                                           
/// CT::Enum                                                                  
///                                                                           
namespace
{
   struct EnumType { using CTTI_Enum = Yes; };
   struct EnumTypeDerived : EnumType {};
   struct NonEnumTypeDerived : EnumType { using CTTI_Enum = No; };
   enum ActualEnum { one, two };
   enum class ActualEnumClass { one, two };
}

TEMPLATE_TEST_CASE("Testing enum types", "[ct]",
   SheddableType<EnumType>,
   EnumType,
   EnumTypeDerived,
   EnumTypeDerived&,
   ActualEnum,
   ActualEnum&,
   ActualEnumClass,
   ActualEnumClass&,
   SheddableType<ActualEnumClass&>
) {
   static_assert(    CT::Enum<TestType>);
   static_assert(not CT::NotEnum<TestType>);
}

TEMPLATE_TEST_CASE("Testing non-enum types", "[ct]",
   SheddableType<EnumType*>,
   EnumType*,
   ActualEnum*,
   ActualEnumClass*,
   NonEnumTypeDerived,
   NonEnumTypeDerived&,
   SheddableType<NonEnumTypeDerived&>,
   int,
   int&
) {
   static_assert(not CT::Enum<TestType>);
   static_assert(    CT::NotEnum<TestType>);
}

//static_assert(CT::Enum<>); // shouldn't compile at all
static_assert(    CT::Enum<SheddableType<EnumType>, EnumType, ActualEnum&>);
static_assert(not CT::Enum<SheddableType<EnumType>, EnumType, NonEnumTypeDerived>);

//static_assert(CT::NotEnum<>); // shouldn't compile at all
static_assert(    CT::NotEnum<SheddableType<EnumType*>, NonEnumTypeDerived, int>);
static_assert(not CT::NotEnum<SheddableType<EnumType*>, NonEnumTypeDerived, ActualEnum>);


///                                                                           
/// CT::Aggregate                                                             
///                                                                           
namespace
{
   struct AggregateType {
      using CTTI_Aggregate = Yes;
      int force_not_aggregate;

      AggregateType()
         : force_not_aggregate(666) {
         --force_not_aggregate;
      }
   };
   struct AggregateTypeDerived : AggregateType {};
   struct NonAggregateTypeDerived : AggregateType {
      using CTTI_Aggregate = No;

      NonAggregateTypeDerived()
         : AggregateType() {
         --force_not_aggregate;
      }
   };
   struct ActualAggregate { int one; int two; };
}

TEMPLATE_TEST_CASE("Testing aggregate types", "[ct]",
   SheddableType<AggregateType>,
   AggregateTypeDerived,
   AggregateTypeDerived&,
   ActualAggregate,
   ActualAggregate&,
   SheddableType<ActualAggregate&>
) {
   static_assert(    CT::Aggregate<TestType>);
   static_assert(not CT::NotAggregate<TestType>);
}

TEMPLATE_TEST_CASE("Testing non-aggregate types", "[ct]",
   SheddableType<AggregateType*>,
   AggregateType*,
   ActualAggregate*,
   NonAggregateTypeDerived,
   NonAggregateTypeDerived&,
   SheddableType<NonAggregateTypeDerived&>,
   int,
   int&
) {
   static_assert(not CT::Aggregate<TestType>);
   static_assert(    CT::NotAggregate<TestType>);
}

//static_assert(CT::Aggregate<>); // shouldn't compile at all
static_assert(    CT::Aggregate<SheddableType<AggregateType>, ActualAggregate, AggregateTypeDerived>);
static_assert(not CT::Aggregate<SheddableType<AggregateType>, ActualAggregate, NonAggregateTypeDerived>);

//static_assert(CT::NotAggregate<>); // shouldn't compile at all
static_assert(    CT::NotAggregate<SheddableType<AggregateType*>, NonAggregateTypeDerived, int>);
static_assert(not CT::NotAggregate<SheddableType<AggregateType*>, NonAggregateTypeDerived, ActualAggregate>);


///                                                                           
/// CT::Fundamental                                                           
///                                                                           
namespace
{
   struct FundamentalType { using CTTI_Fundamental = Yes; };
   struct FundamentalTypeDerived : FundamentalType {};
   struct NonFundamentalTypeDerived : FundamentalType { using CTTI_Fundamental = No; };
}

TEMPLATE_TEST_CASE("Testing fundamental types", "[ct]",
   SheddableType<FundamentalType>,
   FundamentalType,
   FundamentalTypeDerived,
   FundamentalTypeDerived&,
   SheddableType<FundamentalTypeDerived&>,
   int,
   int&
) {
   static_assert(    CT::Fundamental<TestType>);
   static_assert(not CT::NotFundamental<TestType>);
}

TEMPLATE_TEST_CASE("Testing non-fundamental types", "[ct]",
   SheddableType<FundamentalType*>,
   FundamentalType*,
   NonFundamentalTypeDerived,
   NonFundamentalTypeDerived&,
   SheddableType<NonFundamentalTypeDerived&>
) {
   static_assert(not CT::Fundamental<TestType>);
   static_assert(    CT::NotFundamental<TestType>);
}

//static_assert(CT::Fundamental<>); // shouldn't compile at all
static_assert(    CT::Fundamental<SheddableType<FundamentalType>, FundamentalType, int>);
static_assert(not CT::Fundamental<SheddableType<FundamentalType>, FundamentalType, NonFundamentalTypeDerived>);

//static_assert(CT::NotFundamental<>); // shouldn't compile at all
static_assert(    CT::NotFundamental<SheddableType<FundamentalType*>, NonFundamentalTypeDerived, FundamentalType*>);
static_assert(not CT::NotFundamental<SheddableType<FundamentalType*>, NonFundamentalTypeDerived, FundamentalType>);