///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "Main.hpp"
#include <Langulus/Typenav.hpp>
#include <Langulus/CT/Akin.hpp>
#include "TestTypes/CommonTypes.hpp"

using namespace Langulus;

//TODO test custom pointers with all these!!

///                                                                           
/// MARK: CT::Sheddable                                                       
///                                                                           
TEST_CASE_TEMPLATE("Testing sheddable types", TestType
   , SheddableType<int&>
   , SheddableTypeDerived
   , SheddableTypeDerived&
) {
   static_assert(    CT::Sheddable<TestType>);
   static_assert(not CT::NotSheddable<TestType>);
   static_assert(    Exact<Shed<TestType>, int&>);
}

TEST_CASE_TEMPLATE("Testing non-sheddable types", TestType
   , SheddableType<int&>*
   , NonSheddableTypeDerived1
   , NonSheddableTypeDerived1&
   , NonSheddableTypeDerived2
   , NonSheddableTypeDerived2&
   //, NonSheddableTypeDerived3   // shouldn't compile
   //, NonSheddableTypeDerived3&  // shouldn't compile
   , IncompleteType               // incomplete types are always assumed unsheddable
   , int
   , int&
) {
   static_assert(not CT::Sheddable<TestType>);
   static_assert(    CT::NotSheddable<TestType>);
   static_assert(    Exact<Shed<TestType>, TestType>);
}

//static_assert(CT::Sheddable<>); // shouldn't compile at all
static_assert(    CT::Sheddable<SheddableType<int&>, SheddableTypeDerived, SheddableTypeDerived&>);
static_assert(not CT::Sheddable<SheddableType<int&>, SheddableTypeDerived, NonSheddableTypeDerived1>);

//static_assert(CT::NotSheddable<>); // shouldn't compile at all
static_assert(    CT::NotSheddable<SheddableType<int&>*, NonSheddableTypeDerived1, int&>);
static_assert(not CT::NotSheddable<SheddableType<int&>*, NonSheddableTypeDerived1, SheddableTypeDerived>);


///                                                                           
/// MARK: CT::Array                                                           
///                                                                           
TEST_CASE_TEMPLATE("Testing bounded array types", TestType
   , SheddableType<ArrayType>
   , Refer<ArrayType>
   , Clone<ArrayType>
   , Copy<ArrayType>
   , Move<ArrayType>
   , Abandon<ArrayType>
   , Disown<ArrayType>
   , ArrayType
   , ArrayType2
   , ArrayTypeRef
   , ArrayTypeRef2
   , CustomArrayType
) {
   static_assert(CT::Array<TestType>);
}

TEST_CASE_TEMPLATE("Testing non-array types", TestType
   , SheddableType<int>
   , Refer<ArrayType*>
   , Clone<ArrayType*>
   , Copy<ArrayType*>
   , Move<ArrayType*>
   , Abandon<ArrayType*>
   , Disown<ArrayType*>
   , PointerType
   , PointerType2
   , ArrayType*
   , CustomNonArrayType
   , CustomNonArrayTypeDerived
   //, IncompleteType    // shouldn't compile
   , int
   , int&
) {
   static_assert(not CT::Array<TestType>);
}

//static_assert(CT::Array<>); // shouldn't compile at all
static_assert(    CT::Array<ArrayType, ArrayType2, ArrayTypeRef>);
static_assert(not CT::Array<ArrayType, ArrayType2, CustomNonArrayType>);


///                                                                           
/// MARK: ExtentOf                                                            
///                                                                           
SCENARIO("Getting the extent of bounded array types") {
   static_assert(ExtentOf<SheddableType<ArrayType>> == 50);
   static_assert(ExtentOf<Refer<ArrayType>> == 50);
   static_assert(ExtentOf<Move<ArrayType>> == 50);
   static_assert(ExtentOf<Abandon<ArrayType>> == 50);
   static_assert(ExtentOf<Copy<ArrayType>> == 50);
   static_assert(ExtentOf<Clone<ArrayType>> == 50);
   static_assert(ExtentOf<Disown<ArrayType>> == 50);
   static_assert(ExtentOf<ArrayType> == 50);
   static_assert(ExtentOf<ArrayType*> == 1);
   static_assert(ExtentOf<ArrayType2> == 50);
   static_assert(ExtentOf<Deext<ArrayType2>> == 2);
   static_assert(ExtentOf<ArrayTypeRef> == 50);
   static_assert(ExtentOf<ArrayTypeRef2> == 50);
   static_assert(ExtentOf<Deext<ArrayTypeRef2>> == 2);
   static_assert(ExtentOf<PointerType> == 1);
   static_assert(ExtentOf<PointerType2> == 1);
   static_assert(ExtentOf<CustomArrayType> == 56);
   static_assert(ExtentOf<CustomNonArrayType> == 1);
   static_assert(ExtentOf<CustomNonArrayTypeDerived> == 1);
   //static_assert(ExtentOf<IncompleteType> == 1); // shouldn't compile
}


///                                                                           
/// MARK: AllExtentsOf                                                        
///                                                                           
SCENARIO("Getting the extent of bounded array types") {
   static_assert(AllExtentsOf<SheddableType<ArrayType>> == 50);
   static_assert(AllExtentsOf<Refer<ArrayType>> == 50);
   static_assert(AllExtentsOf<Move<ArrayType>> == 50);
   static_assert(AllExtentsOf<Abandon<ArrayType>> == 50);
   static_assert(AllExtentsOf<Copy<ArrayType>> == 50);
   static_assert(AllExtentsOf<Clone<ArrayType>> == 50);
   static_assert(AllExtentsOf<Disown<ArrayType>> == 50);
   static_assert(AllExtentsOf<Disown<ArrayType2>> == 100);
   static_assert(AllExtentsOf<ArrayType> == 50);
   static_assert(AllExtentsOf<ArrayType*> == 1);
   static_assert(AllExtentsOf<ArrayType2> == 100);
   static_assert(AllExtentsOf<Deext<ArrayType2>> == 2);
   static_assert(AllExtentsOf<ArrayTypeRef> == 50);
   static_assert(AllExtentsOf<ArrayTypeRef2> == 100);
   static_assert(AllExtentsOf<Deext<ArrayTypeRef2>> == 2);
   static_assert(AllExtentsOf<PointerType> == 1);
   static_assert(AllExtentsOf<PointerType2> == 1);
   static_assert(AllExtentsOf<CustomArrayType> == 56);
   static_assert(AllExtentsOf<CustomNonArrayType> == 1);
   static_assert(AllExtentsOf<CustomNonArrayTypeDerived> == 1);
   //static_assert(AllExtentsOf<IncompleteType> == 1); // shouldn't compile
}


///                                                                           
/// MARK: CT::Sparse / CT::Dense                                              
///                                                                           
TEST_CASE_TEMPLATE("Testing sparse types", TestType
   , SheddableType<PointerType>
   , PointerType
   , CustomPointerType
   , IncompleteType*
   , void**
) {
   static_assert(    CT::Sparse<TestType>);
   static_assert(not CT::Dense<TestType>);
}

TEST_CASE_TEMPLATE("Testing dense types", TestType
   //, IncompleteType    // shouldn't compile, we must check whether it's a custom pointer type
   , SheddableType<CustomNonPointerType>
   , CustomNonPointerType
   , int, int&, void, nullptr_t
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
/// MARK: CT::Constant / CT::Mutable                                          
///                                                                           
TEST_CASE_TEMPLATE("Testing constant types", TestType
   , SheddableType<const PointerType>
   , SheddableType<const PointerType&>
   , PointerType* const
   , IncompleteType const
) {
   static_assert(    CT::Constant<TestType>);
   static_assert(not CT::Mutable<TestType>);
}

TEST_CASE_TEMPLATE("Testing mutable types", TestType
   , IncompleteType
   , SheddableType<PointerType>
   , SheddableType<PointerType&>
   , SheddableType<PointerType const*>
   , PointerType const* const*
   , nullptr_t
   , void, int
) {
   static_assert(not CT::Constant<TestType>);
   static_assert(    CT::Mutable<TestType>);
}

//static_assert(CT::Constant<>); // shouldn't compile at all
static_assert(    CT::Constant<SheddableType<const PointerType>, PointerType* const, IncompleteType const>);
static_assert(not CT::Constant<SheddableType<const PointerType>, PointerType* const, int>);

//static_assert(CT::Mutable<>); // shouldn't compile at all
static_assert(    CT::Mutable<SheddableType<PointerType>, SheddableType<PointerType&>, int>);
static_assert(not CT::Mutable<SheddableType<PointerType>, SheddableType<PointerType&>, IncompleteType const>);


///                                                                           
/// MARK: CT::Volatile                                                        
///                                                                           
TEST_CASE_TEMPLATE("Testing volatile types", TestType
   , SheddableType<volatile int>
   , SheddableType<volatile int&>
   , volatile int&
   , volatile int
   , volatile IncompleteType
) {
   static_assert(    CT::Volatile<TestType>);
}

TEST_CASE_TEMPLATE("Testing non-volatile types", TestType
   , SheddableType<volatile int*>
   , int
   , IncompleteType
) {
   static_assert(not CT::Volatile<TestType>);
}

//static_assert(CT::Volatile<>); // shouldn't compile at all
static_assert(    CT::Volatile<SheddableType<volatile int>, SheddableType<volatile int&>, volatile int>);
static_assert(not CT::Volatile<SheddableType<volatile int>, SheddableType<volatile int&>, int>);


///                                                                           
/// MARK: CT::Convoluted                                                      
///                                                                           
TEST_CASE_TEMPLATE("Testing convoluted types", TestType
   , SheddableType<const PointerType>
   , SheddableType<const PointerType&>
   , SheddableType<volatile PointerType>
   , SheddableType<volatile PointerType&>
   , PointerType* const
   , PointerType* volatile
   , IncompleteType const
   , volatile IncompleteType
) {
   static_assert(    CT::Convoluted<TestType>);
   static_assert(not CT::NotConvoluted<TestType>);
}

TEST_CASE_TEMPLATE("Testing non-convoluted types", TestType
   , SheddableType<PointerType>
   , SheddableType<PointerType&>
   , SheddableType<PointerType const*>
   , volatile PointerType const* const*
   , nullptr_t
   , void, int
   , IncompleteType
) {
   static_assert(    CT::NotConvoluted<TestType>);
   static_assert(not CT::Convoluted<TestType>);
}

//static_assert(CT::Convoluted<>); // shouldn't compile at all
static_assert(    CT::Convoluted<SheddableType<const PointerType>, const int, const int>);
static_assert(not CT::Convoluted<SheddableType<const PointerType>, const int, int>);

//static_assert(CT::NotConvoluted<>); // shouldn't compile at all
static_assert(    CT::NotConvoluted<SheddableType<PointerType>, SheddableType<PointerType&>, int>);
static_assert(not CT::NotConvoluted<SheddableType<PointerType>, SheddableType<PointerType&>, const int>);


///                                                                           
/// MARK:: CT::Null                                                           
///                                                                           
namespace
{
   struct NullType { using CTTI_Null = Yes<>; };
   struct NullTypeDerived : NullType {};
   struct NonNullTypeDerived : NullType { using CTTI_Null = No; };
}

TEST_CASE_TEMPLATE("Testing null types", TestType
   , SheddableType<NullType>
   , NullType
   , NullTypeDerived
   , NullTypeDerived&
   , nullptr_t
) {
   static_assert(    CT::Null<TestType>);
   static_assert(not CT::NotNull<TestType>);
}

TEST_CASE_TEMPLATE("Testing non-null types", TestType
   , SheddableType<NullType*>
   , NullType*
   , nullptr_t*
   , NonNullTypeDerived
   , NonNullTypeDerived&
   , SheddableType<NonNullTypeDerived&>
   //, IncompleteType // shouldn't compile
   , int
   , int&
) {
   static_assert(not CT::Null<TestType>);
   static_assert(    CT::NotNull<TestType>);
}

//static_assert(CT::Null<>); // shouldn't compile at all
static_assert(    CT::Null<SheddableType<NullType>, NullType, nullptr_t>);
static_assert(not CT::Null<SheddableType<NullType>, NullType, NonNullTypeDerived>);

//static_assert(CT::NotNull<>); // shouldn't compile at all
static_assert(    CT::NotNull<SheddableType<NullType*>, nullptr_t*, int>);
static_assert(not CT::NotNull<SheddableType<NullType*>, nullptr_t*, NullType>);


///                                                                           
/// MARK: CT::Enum                                                            
///                                                                           
namespace
{
   struct EnumType { using CTTI_Enum = Yes<>; };
   struct EnumTypeDerived : EnumType {};
   struct NonEnumTypeDerived : EnumType { using CTTI_Enum = No; };
   enum ActualEnum { one, two };
   enum class ActualEnumClass { one, two };
}

TEST_CASE_TEMPLATE("Testing enum types", TestType
   , SheddableType<EnumType>
   , EnumType
   , EnumTypeDerived
   , EnumTypeDerived&
   , ActualEnum
   , ActualEnum&
   , ActualEnumClass
   , ActualEnumClass&
   , SheddableType<ActualEnumClass&>
) {
   static_assert(    CT::Enum<TestType>);
   static_assert(not CT::NotEnum<TestType>);
}

TEST_CASE_TEMPLATE("Testing non-enum types", TestType
   , SheddableType<EnumType*>
   , EnumType*
   , ActualEnum*
   , ActualEnumClass*
   , NonEnumTypeDerived
   , NonEnumTypeDerived&
   , SheddableType<NonEnumTypeDerived&>
   //, IncompleteType    // shouldn't compile
   , int
   , int&
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
/// MARK: CT::Aggregate                                                       
///                                                                           
TEST_CASE_TEMPLATE("Testing aggregate types", TestType
   , SheddableType<ActualAggregateType>
   , SheddableType<CustomAggregateType>
   , CustomAggregateType
   , AggregateTypeDerived
   , AggregateTypeDerived&
   , ActualAggregateType
   , ActualAggregateType&
   , SheddableType<ActualAggregateType&>
   //, IncompleteType    // shouldn't compile
) {
   static_assert(    CT::Aggregate<TestType>);
   static_assert(not CT::NotAggregate<TestType>);
}

TEST_CASE_TEMPLATE("Testing non-aggregate types", TestType
   , SheddableType<ActualAggregateType*>
   , SheddableType<CustomAggregateType*>
   , CustomAggregateType*
   , ActualAggregateType*
   , NonAggregateTypeDerived
   , NonAggregateTypeDerived&
   , SheddableType<NonAggregateTypeDerived&>
   , int, int&
   //, IncompleteType    // shouldn't compile
) {
   static_assert(not CT::Aggregate<TestType>);
   static_assert(    CT::NotAggregate<TestType>);
}

//static_assert(CT::Aggregate<>); // shouldn't compile at all
static_assert(    CT::Aggregate<SheddableType<ActualAggregateType>, ActualAggregateType, AggregateTypeDerived>);
static_assert(not CT::Aggregate<SheddableType<ActualAggregateType>, ActualAggregateType, NonAggregateTypeDerived>);

//static_assert(CT::NotAggregate<>); // shouldn't compile at all
static_assert(    CT::NotAggregate<SheddableType<ActualAggregateType*>, NonAggregateTypeDerived, int>);
static_assert(not CT::NotAggregate<SheddableType<ActualAggregateType*>, NonAggregateTypeDerived, ActualAggregateType>);


///                                                                           
/// MARK: CT::Fundamental                                                     
///                                                                           
namespace
{
   struct FundamentalType { using CTTI_Fundamental = Yes<>; };
   struct FundamentalTypeDerived : FundamentalType {};
   struct NonFundamentalTypeDerived : FundamentalType { using CTTI_Fundamental = No; };
}

TEST_CASE_TEMPLATE("Testing fundamental types", TestType
   , SheddableType<FundamentalType>
   , FundamentalType
   , FundamentalTypeDerived
   , FundamentalTypeDerived&
   , SheddableType<FundamentalTypeDerived&>
   , int
   , int&
) {
   static_assert(    CT::Fundamental<TestType>);
   static_assert(not CT::NotFundamental<TestType>);
}

TEST_CASE_TEMPLATE("Testing non-fundamental types", TestType
   //, IncompleteType    // shouldn't compile
   , SheddableType<FundamentalType*>
   , FundamentalType*
   , NonFundamentalTypeDerived
   , NonFundamentalTypeDerived&
   , SheddableType<NonFundamentalTypeDerived&>
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


///                                                                           
/// MARK: CT::Reference                                                       
///                                                                           
TEST_CASE_TEMPLATE("Testing reference types", TestType
   , IncompleteType&
   , SheddableType<int&>
   , int*&
   , int&
   , int&&
   , int(&)[15]
) {
   static_assert(    CT::Reference<TestType>);
   static_assert(not CT::NotReference<TestType>);
}

TEST_CASE_TEMPLATE("Testing non-reference types", TestType
   , IncompleteType
   , SheddableType<int>&
   , int*
   , int[15]
) {
   static_assert(not CT::Reference<TestType>);
   static_assert(    CT::NotReference<TestType>);
}

//static_assert(CT::Reference<>); // shouldn't compile at all
static_assert(    CT::Reference<SheddableType<int&>, int&, int(&)[15]>);
static_assert(not CT::Reference<SheddableType<int&>, int&, int   [15]>);

//static_assert(CT::NotReference<>); // shouldn't compile at all
static_assert(    CT::NotReference<SheddableType<int>&, int*, IncompleteType>);
static_assert(not CT::NotReference<SheddableType<int>&, int*, IncompleteType&>);


///                                                                           
/// MARK: CT::Decayed                                                         
///                                                                           
TEST_CASE_TEMPLATE("Testing decayed types", TestType
   , SheddableType<int&>
   , int, void
   //, IncompleteType // shouldn't compile at all
) {
   static_assert(    CT::Decayed<TestType>);
   static_assert(not CT::NotDecayed<TestType>);
}

TEST_CASE_TEMPLATE("Testing non-decayed types", TestType
   , SheddableType<int>&
   , int*
   , int[15]
   , int*&
   , int&
   , int&&
   , int(&)[15]
   , IncompleteType&
) {
   static_assert(not CT::Decayed<TestType>);
   static_assert(    CT::NotDecayed<TestType>);
}

//static_assert(CT::Decayed<>); // shouldn't compile at all
static_assert(    CT::Decayed<SheddableType<int&>, int, void /*IncompleteType*/>);
static_assert(not CT::Decayed<SheddableType<int&>, int, void*/*IncompleteType&*/>);

//static_assert(CT::NotDecayed<>); // shouldn't compile at all
static_assert(    CT::NotDecayed<SheddableType<int>&, int*, void*/*IncompleteType&*/>);
static_assert(not CT::NotDecayed<SheddableType<int>&, int*, void /*IncompleteType*/>);


///                                                                           
/// MARK: CT::Slab                                                            
///                                                                           
TEST_CASE_TEMPLATE("Testing slab types", TestType
   , SheddableType<int>
   , SheddableType<int&>
   , int
   //, IncompleteType // shouldn't compile at all
) {
   static_assert(CT::Slab<TestType>);
}

TEST_CASE_TEMPLATE("Testing non-volatile types", TestType
   , SheddableType<int>&
   , int*
   , int*&
   , int[15]
   , int(&)[15]
   , IncompleteType*
) {
   static_assert(not CT::Slab<TestType>);
}

//static_assert(CT::Slab<>); // shouldn't compile at all
static_assert(    CT::Slab<SheddableType<int>, SheddableType<int&>, int>);
static_assert(not CT::Slab<SheddableType<int>, SheddableType<int&>, int*>);


///                                                                           
/// MARK: IsConstexpr                                                         
///                                                                           
namespace
{
   constexpr int const_function(int x, int y) { return x + y; }
   int nonconst_function(int x, int y) { return x + y; }
}

static_assert(    IsConstexpr([] {    const_function(1,2);}));
static_assert(not IsConstexpr([] { nonconst_function(1,2);}));


///                                                                           
/// MARK: Deref                                                               
///                                                                           
static_assert(::std::same_as<Deref<SheddableType<int&>>, SheddableType<int&>>);
static_assert(::std::same_as<Deref<SheddableType<int>&>, SheddableType<int>>);

static_assert(::std::same_as<Deref<int>,              int>);
static_assert(::std::same_as<Deref<int*>,             int*>);
static_assert(::std::same_as<Deref<int const*>,       int const*>);
static_assert(::std::same_as<Deref<int&>,             int>);
static_assert(::std::same_as<Deref<int&&>,            int>);
static_assert(::std::same_as<Deref<int const>,        int const>);
static_assert(::std::same_as<Deref<int const&>,       int const>);
static_assert(::std::same_as<Deref<int const&&>,      int const>);
static_assert(::std::same_as<Deref<const int(&)[15]>, const int[15]>);
static_assert(::std::same_as<Deref<int(&)[15]>,       int[15]>);


///                                                                           
/// MARK: Deptr                                                               
///                                                                           
static_assert(::std::same_as<Deptr<SheddableType<int**>>, int*>);
static_assert(::std::same_as<Deptr<SheddableType<int*>>, int>);
static_assert(::std::same_as<Deptr<SheddableType<int>*>, SheddableType<int>>);

static_assert(::std::same_as<Deptr<int>,         int>);
static_assert(::std::same_as<Deptr<int&>,        int>);
static_assert(::std::same_as<Deptr<int&&>,       int>);
static_assert(::std::same_as<Deptr<int const&>,  int const>);
static_assert(::std::same_as<Deptr<int const&&>, int const>);
static_assert(::std::same_as<Deptr<int(&)[15]>,  int>);

static_assert(::std::same_as<Deptr<int*>,        int>);
static_assert(::std::same_as<Deptr<int const*>,  int const>);
static_assert(::std::same_as<Deptr<int*&>,       int>);
static_assert(::std::same_as<Deptr<int*&&>,      int>);
static_assert(::std::same_as<Deptr<int const*&>, int const>);
static_assert(::std::same_as<Deptr<int const*&&>,int const>);
static_assert(::std::same_as<Deptr<int(*)[15]>,  int[15]>);
static_assert(::std::same_as<Deptr<int*(*)[15]>, int*[15]>);
static_assert(::std::same_as<Deptr<int*[15]>,    int*>);

static_assert(::std::same_as<Deptr<int**>,   int*>);
static_assert(::std::same_as<Deptr<int**&>,  int*>);
static_assert(::std::same_as<Deptr<int**&&>, int*>);
static_assert(::std::same_as<Deptr<int const**&>,  int const*>);
static_assert(::std::same_as<Deptr<int const**&&>, int const*>);
static_assert(::std::same_as<Deptr<int const* const*&>,  int const* const>);
static_assert(::std::same_as<Deptr<int const* const*&&>, int const* const>);

static_assert(::std::same_as<Deptr<SheddableType<int**>, 2>, int>);
static_assert(::std::same_as<Deptr<SheddableType<int*>, 2>, int>);
static_assert(::std::same_as<Deptr<SheddableType<int>*, 2>, SheddableType<int>>);

static_assert(::std::same_as<Deptr<int, 2>,   int>);
static_assert(::std::same_as<Deptr<int&, 2>,  int>);
static_assert(::std::same_as<Deptr<int&&, 2>, int>);
static_assert(::std::same_as<Deptr<int const&, 2>,  int const>);
static_assert(::std::same_as<Deptr<int const&&, 2>, int const>);
static_assert(::std::same_as<Deptr<int(&)[15], 2>,  int>);

static_assert(::std::same_as<Deptr<int*, 2>,         int>);
static_assert(::std::same_as<Deptr<int*&, 2>,        int>);
static_assert(::std::same_as<Deptr<int*&&, 2>,       int>);
static_assert(::std::same_as<Deptr<int const*&, 2>,  int const>);
static_assert(::std::same_as<Deptr<int const*&&, 2>, int const>);
static_assert(::std::same_as<Deptr<int(*)[15], 2>,   int>);
static_assert(::std::same_as<Deptr<int*(*)[15], 2>,  int*>);
static_assert(::std::same_as<Deptr<int*[15], 2>,     int>);

static_assert(::std::same_as<Deptr<int**, 2>,   int>);
static_assert(::std::same_as<Deptr<int**&, 2>,  int>);
static_assert(::std::same_as<Deptr<int**&&, 2>, int>);
static_assert(::std::same_as<Deptr<int const**&, 2>,  int const>);
static_assert(::std::same_as<Deptr<int const**&&, 2>, int const>);
static_assert(::std::same_as<Deptr<int const* const*&, 2>,  int const>);
static_assert(::std::same_as<Deptr<int const* const*&&, 2>, int const>);


///                                                                           
/// MARK: Decvq                                                               
///                                                                           
static_assert(::std::same_as<Decvq<SheddableType<int* const>>, SheddableType<int* const>>);
static_assert(::std::same_as<Decvq<SheddableType<int>* const volatile>, SheddableType<int>*>);

static_assert(::std::same_as<Decvq<int>,   int>);
static_assert(::std::same_as<Decvq<int&>,  int&>);
static_assert(::std::same_as<Decvq<int&&>, int&&>);
static_assert(::std::same_as<Decvq<volatile int>, int>);
static_assert(::std::same_as<Decvq<int const&>,  int const&>);
static_assert(::std::same_as<Decvq<int const&&>, int const&&>);
static_assert(::std::same_as<Decvq<int const>,   int>);
static_assert(::std::same_as<Decvq<volatile int const>, int>);
static_assert(::std::same_as<Decvq<volatile int(&)[15]>, volatile int(&)[15]>);
static_assert(::std::same_as<Decvq<volatile const int[15]>, int[15]>);

static_assert(::std::same_as<Decvq<int*>,   int*>);
static_assert(::std::same_as<Decvq<int*&>,  int*&>);
static_assert(::std::same_as<Decvq<int*&&>, int*&&>);
static_assert(::std::same_as<Decvq<volatile int*>, volatile int*>);
static_assert(::std::same_as<Decvq<int const*&>,  int const*&>);
static_assert(::std::same_as<Decvq<int const*&&>, int const*&&>);
static_assert(::std::same_as<Decvq<int const* const&>,  int const* const&>);
static_assert(::std::same_as<Decvq<int const* const&&>, int const* const&&>);
static_assert(::std::same_as<Decvq<int const* const>,   int const*>);
static_assert(::std::same_as<Decvq<int const* const volatile>, int const*>);

static_assert(::std::same_as<Decvq<int**>,   int**>);
static_assert(::std::same_as<Decvq<int**&>,  int**&>);
static_assert(::std::same_as<Decvq<int**&&>, int**&&>);
static_assert(::std::same_as<Decvq<volatile int**>, volatile int**>);
static_assert(::std::same_as<Decvq<int const**&>,  int const**&>);
static_assert(::std::same_as<Decvq<int const**&&>, int const**&&>);
static_assert(::std::same_as<Decvq<int const* const*&>,  int const* const*&>);
static_assert(::std::same_as<Decvq<int const* const*&&>, int const* const*&&>);
static_assert(::std::same_as<Decvq<int const* const* const&>,  int const* const* const&>);
static_assert(::std::same_as<Decvq<int const* const* const&&>, int const* const* const&&>);
static_assert(::std::same_as<Decvq<int const* const* const>,   int const* const*>);
static_assert(::std::same_as<Decvq<int const* const* const volatile>, int const* const*>);


///                                                                           
/// MARK: Decq                                                                
///                                                                           
static_assert(::std::same_as<Decq<SheddableType<int* const>>, SheddableType<int* const>>);
static_assert(::std::same_as<Decq<SheddableType<int>* const>, SheddableType<int>*>);

static_assert(::std::same_as<Decq<int>,   int>);
static_assert(::std::same_as<Decq<int&>,  int&>);
static_assert(::std::same_as<Decq<int&&>, int&&>);
static_assert(::std::same_as<Decq<volatile int>, volatile int>);
static_assert(::std::same_as<Decq<int const&>,  int const&>);
static_assert(::std::same_as<Decq<int const&&>, int const&&>);
static_assert(::std::same_as<Decq<int const>,   int>);
static_assert(::std::same_as<Decq<int const volatile>, int volatile>);
static_assert(::std::same_as<Decq<volatile int(&)[15]>, volatile int(&)[15]>);
static_assert(::std::same_as<Decq<volatile const int[15]>, volatile int[15]>);

static_assert(::std::same_as<Decq<int*>,   int*>);
static_assert(::std::same_as<Decq<int*&>,  int*&>);
static_assert(::std::same_as<Decq<int*&&>, int*&&>);
static_assert(::std::same_as<Decq<volatile int*>, volatile int*>);
static_assert(::std::same_as<Decq<int const*&>,  int const*&>);
static_assert(::std::same_as<Decq<int const*&&>, int const*&&>);
static_assert(::std::same_as<Decq<int const* const&>,  int const* const&>);
static_assert(::std::same_as<Decq<int const* const&&>, int const* const&&>);
static_assert(::std::same_as<Decq<int const* const>,   int const*>);
static_assert(::std::same_as<Decq<int const* const volatile>, int const* volatile>);

static_assert(::std::same_as<Decq<int**>,   int**>);
static_assert(::std::same_as<Decq<int**&>,  int**&>);
static_assert(::std::same_as<Decq<int**&&>, int**&&>);
static_assert(::std::same_as<Decq<volatile int**>, volatile int**>);
static_assert(::std::same_as<Decq<int const**&>,  int const**&>);
static_assert(::std::same_as<Decq<int const**&&>, int const**&&>);
static_assert(::std::same_as<Decq<int const* const*&>,  int const* const*&>);
static_assert(::std::same_as<Decq<int const* const*&&>, int const* const*&&>);
static_assert(::std::same_as<Decq<int const* const* const&>,  int const* const* const&>);
static_assert(::std::same_as<Decq<int const* const* const&&>, int const* const* const&&>);
static_assert(::std::same_as<Decq<int const* const* const>,   int const* const*>);
static_assert(::std::same_as<Decq<int const* const* const volatile>, int const* const* volatile>);


///                                                                           
/// MARK: Devq                                                                
///                                                                           
static_assert(::std::same_as<Decq<SheddableType<int* const>>, SheddableType<int* const>>);
static_assert(::std::same_as<Decq<SheddableType<int>* const volatile>, SheddableType<int>* volatile>);

static_assert(::std::same_as<Devq<int>, int>);
static_assert(::std::same_as<Devq<int&>, int&>);
static_assert(::std::same_as<Devq<int&&>, int&&>);
static_assert(::std::same_as<Devq<volatile int>, int>);
static_assert(::std::same_as<Devq<int const&>, int const&>);
static_assert(::std::same_as<Devq<int const&&>, int const&&>);
static_assert(::std::same_as<Devq<int const>, int const>);
static_assert(::std::same_as<Devq<volatile int const>, int const>);
static_assert(::std::same_as<Devq<volatile int(&)[15]>, volatile int(&)[15]>);
static_assert(::std::same_as<Devq<volatile const int[15]>, const int[15]>);

static_assert(::std::same_as<Devq<int*>, int*>);
static_assert(::std::same_as<Devq<int*&>, int*&>);
static_assert(::std::same_as<Devq<int*&&>, int*&&>);
static_assert(::std::same_as<Devq<volatile int*>, volatile int*>);
static_assert(::std::same_as<Devq<int const*&>, int const*&>);
static_assert(::std::same_as<Devq<int const*&&>, int const*&&>);
static_assert(::std::same_as<Devq<int const* const&>, int const* const&>);
static_assert(::std::same_as<Devq<int const* const&&>, int const* const&&>);
static_assert(::std::same_as<Devq<int const* const>, int const* const>);
static_assert(::std::same_as<Devq<int const* const volatile>, int const* const>);

static_assert(::std::same_as<Devq<int**>, int**>);
static_assert(::std::same_as<Devq<int**&>, int**&>);
static_assert(::std::same_as<Devq<int**&&>, int**&&>);
static_assert(::std::same_as<Devq<volatile int**>, volatile int**>);
static_assert(::std::same_as<Devq<int const**&>, int const**&>);
static_assert(::std::same_as<Devq<int const**&&>, int const**&&>);
static_assert(::std::same_as<Devq<int const* const*&>, int const* const*&>);
static_assert(::std::same_as<Devq<int const* const*&&>, int const* const*&&>);
static_assert(::std::same_as<Devq<int const* const* const&>, int const* const* const&>);
static_assert(::std::same_as<Devq<int const* const* const&&>, int const* const* const&&>);
static_assert(::std::same_as<Devq<int const* const* const>, int const* const* const>);
static_assert(::std::same_as<Devq<int const* const* const volatile>, int const* const* const>);


///                                                                           
/// MARK: Deext                                                               
///                                                                           
static_assert(::std::same_as<Deext<SheddableType<int(&)[15]>>, SheddableType<int(&)[15]>>);
static_assert(::std::same_as<Deext<SheddableType<int>(&)[15]>, SheddableType<int>>);

static_assert(::std::same_as<Deext<int>, int>);
static_assert(::std::same_as<Deext<int&>, int>);
static_assert(::std::same_as<Deext<int&&>, int>);
static_assert(::std::same_as<Deext<volatile int>, volatile int>);
static_assert(::std::same_as<Deext<int const&>, int const>);
static_assert(::std::same_as<Deext<int const&&>, int const>);
static_assert(::std::same_as<Deext<int const>, int const>);
static_assert(::std::same_as<Deext<volatile int(&)[15]>, volatile int>);
static_assert(::std::same_as<Deext<volatile const int[15]>, volatile const int>);

static_assert(::std::same_as<Deext<int*>, int*>);
static_assert(::std::same_as<Deext<int*&>, int*>);
static_assert(::std::same_as<Deext<int*&&>, int*>);
static_assert(::std::same_as<Deext<volatile int*>, volatile int*>);
static_assert(::std::same_as<Deext<int const*&>, int const*>);
static_assert(::std::same_as<Deext<int const*&&>, int const*>);
static_assert(::std::same_as<Deext<int const* const&>, int const* const>);
static_assert(::std::same_as<Deext<int const* const&&>, int const* const>);
static_assert(::std::same_as<Deext<int const* const>, int const* const>);
static_assert(::std::same_as<Deext<int const* const volatile>, int const* const volatile>);


///                                                                           
/// MARK: Decay                                                               
///                                                                           
static_assert(::std::same_as<Decay<SheddableType<int* const>>, int>);
static_assert(::std::same_as<Decay<SheddableType<int>* const volatile>, int>);

static_assert(::std::same_as<Decay<int>, int>);
static_assert(::std::same_as<Decay<int&>, int>);
static_assert(::std::same_as<Decay<int&&>, int>);
static_assert(::std::same_as<Decay<volatile int>, int>);
static_assert(::std::same_as<Decay<int const&>, int>);
static_assert(::std::same_as<Decay<int const&&>, int>);
static_assert(::std::same_as<Decay<int const>, int>);
static_assert(::std::same_as<Decay<volatile int(&)[15]>, int>);
static_assert(::std::same_as<Decay<volatile const int[15]>, int>);

static_assert(::std::same_as<Decay<int*>, int>);
static_assert(::std::same_as<Decay<int*&>, int>);
static_assert(::std::same_as<Decay<int*&&>, int>);
static_assert(::std::same_as<Decay<volatile int*>, int>);
static_assert(::std::same_as<Decay<int const*&>, int>);
static_assert(::std::same_as<Decay<int const*&&>, int>);
static_assert(::std::same_as<Decay<int const* const&>, int>);
static_assert(::std::same_as<Decay<int const* const&&>, int>);
static_assert(::std::same_as<Decay<int const* const>, int>);
static_assert(::std::same_as<Decay<int const* const volatile>, int>);

static_assert(::std::same_as<Decay<int**>, int>);
static_assert(::std::same_as<Decay<int**&>, int>);
static_assert(::std::same_as<Decay<int**&&>, int>);
static_assert(::std::same_as<Decay<volatile int**>, int>);
static_assert(::std::same_as<Decay<int const**&>, int>);
static_assert(::std::same_as<Decay<int const**&&>, int>);
static_assert(::std::same_as<Decay<int const* const*&>, int>);
static_assert(::std::same_as<Decay<int const* const*&&>, int>);
static_assert(::std::same_as<Decay<int const* const* const&>, int>);
static_assert(::std::same_as<Decay<int const* const* const&&>, int>);
static_assert(::std::same_as<Decay<int const* const* const>, int>);
static_assert(::std::same_as<Decay<int const* const* const volatile>, int>);


///                                                                           
/// MARK: DecvqAll                                                            
///                                                                           
static_assert(::std::same_as<DecvqAll<SheddableType<int* const>>, SheddableType<int* const>>);
static_assert(::std::same_as<DecvqAll<SheddableType<int>* const volatile>, SheddableType<int>*>);

static_assert(::std::same_as<DecvqAll<int>,   int>);
static_assert(::std::same_as<DecvqAll<int&>,  int&>);
static_assert(::std::same_as<DecvqAll<int&&>, int&&>);
static_assert(::std::same_as<DecvqAll<volatile int>, int>);
static_assert(::std::same_as<DecvqAll<int const&>,  int&>);
static_assert(::std::same_as<DecvqAll<int const&&>, int&&>);
static_assert(::std::same_as<DecvqAll<int const>,   int>);
static_assert(::std::same_as<DecvqAll<volatile int const>, int>);
static_assert(::std::same_as<DecvqAll<volatile int(&)[15]>, int(&)[15]>);
static_assert(::std::same_as<DecvqAll<volatile const int[15]>, int[15]>);

static_assert(::std::same_as<DecvqAll<int*>,   int*>);
static_assert(::std::same_as<DecvqAll<int*&>,  int*&>);
static_assert(::std::same_as<DecvqAll<int*&&>, int*&&>);
static_assert(::std::same_as<DecvqAll<volatile int*>, int*>);
static_assert(::std::same_as<DecvqAll<int const*&>,  int*&>);
static_assert(::std::same_as<DecvqAll<int const*&&>, int*&&>);
static_assert(::std::same_as<DecvqAll<int const* const&>,  int*&>);
static_assert(::std::same_as<DecvqAll<int const* const&&>, int*&&>);
static_assert(::std::same_as<DecvqAll<int const* const>,   int*>);
static_assert(::std::same_as<DecvqAll<int const* const volatile>, int*>);

static_assert(::std::same_as<DecvqAll<int**>,   int**>);
static_assert(::std::same_as<DecvqAll<int**&>,  int**&>);
static_assert(::std::same_as<DecvqAll<int**&&>, int**&&>);
static_assert(::std::same_as<DecvqAll<volatile int**>, int**>);
static_assert(::std::same_as<DecvqAll<int const**&>,  int**&>);
static_assert(::std::same_as<DecvqAll<int const**&&>, int**&&>);
static_assert(::std::same_as<DecvqAll<int const* const*&>,  int**&>);
static_assert(::std::same_as<DecvqAll<int const* const*&&>, int**&&>);
static_assert(::std::same_as<DecvqAll<int const* const* const&>,  int**&>);
static_assert(::std::same_as<DecvqAll<int const* const* const&&>, int**&&>);
static_assert(::std::same_as<DecvqAll<int const* const* const>,   int**>);
static_assert(::std::same_as<DecvqAll<int const* const* const volatile>, int**>);

TEST_CASE_TEMPLATE("Testing DecvqAllCast", TestType
   , SheddableType<int>
   , SheddableType<int> const
   , SheddableType<int>&
   , SheddableType<int> const&
   , int
   , int const
   , int*
   , int const*
   , int const* const
   , int**
   , int const* const*
   , int const* const* const
   , int*&
   , int const* const&
   //int const* const&& //TODO explain why this is commented???
   , int[15]
   , const int[15]
   , int(&)[15]
   , const int(&)[15]
   , IncompleteType*
   , IncompleteType const*
   , IncompleteType const* const
) {
   if constexpr (::std::is_bounded_array_v<TestType>)
      static_assert(::std::same_as<decltype(DecvqAllCast(LglsFake(TestType))), Deext<DecvqAll<TestType>>*>);
   else
      static_assert(::std::same_as<decltype(DecvqAllCast(LglsFake(TestType))), DecvqAll<TestType>>);
}

///                                                                           
/// MARK: IndirectsOf                                                         
///                                                                           
static_assert(IndirectsOf<SheddableType<int>> == 0);
static_assert(IndirectsOf<SheddableType<int* const>> == 1);
static_assert(IndirectsOf<SheddableType<int>* const volatile> == 1);
static_assert(IndirectsOf<SheddableType<int*>* const volatile> == 2);

static_assert(IndirectsOf<int> == 0);
static_assert(IndirectsOf<int&> == 0);
static_assert(IndirectsOf<int&&> == 0);
static_assert(IndirectsOf<int(&)[15]> == 0);

static_assert(IndirectsOf<int*> == 1);
static_assert(IndirectsOf<int**> == 2);
static_assert(IndirectsOf<int***> == 3);
static_assert(IndirectsOf<int***&> == 3);
static_assert(IndirectsOf<int***&&> == 3);
static_assert(IndirectsOf<int const* const* const* const> == 3);
