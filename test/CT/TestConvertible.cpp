///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "../Main.hpp"
#include "../TestTypes/CommonTypes.hpp"
#include <Langulus/CT/Convertible.hpp>


///                                                                           
/// CT::ConvertibleImplicit                                                   
///                                                                           
TEST_CASE_TEMPLATE("Testing CT::ConvertibleImplicit from int", TestType
   //, IncompleteType                // shouldn't compile
   //, IncompleteType const          // shouldn't compile
   //, SheddableType<IncompleteType> // shouldn't compile
   , SheddableType<BuiltinConvertibleFromIntViaConstructor>
   , SheddableType<BuiltinConvertibleFromIntViaConstructor const>
   , SheddableType<BuiltinConvertibleFromIntViaConstructor const&>
   , BuiltinConvertibleFromIntViaConstructor
   , BuiltinConvertibleFromIntViaConstructor const
   , BuiltinConvertibleFromIntViaConstructor&
   , int, const int, const int&, int&
   , float, float const, float const&, float&
   , bool, bool const, bool const&, bool&
) {
   static_assert(CT::ConvertibleImplicit<int, TestType>);
}

TEST_CASE_TEMPLATE("Testing not CT::ConvertibleImplicit from int", TestType
   //, IncompleteType                // shouldn't compile
   //, IncompleteType const          // shouldn't compile
   //, SheddableType<IncompleteType> // shouldn't compile
   , void
   , void*
   , SheddableType<BuiltinConvertibleFromIntViaExplicitConstructor>
   , SheddableType<BuiltinConvertibleFromIntViaExplicitConstructor const>
   , SheddableType<BuiltinConvertibleFromIntViaExplicitConstructor const&>
   , BuiltinConvertibleFromIntViaExplicitConstructor
   , BuiltinConvertibleFromIntViaExplicitConstructor const
   , BuiltinConvertibleFromIntViaExplicitConstructor&
   , SheddableType<ConvertibleFromIntExternallyMissingConverter*>
   , SheddableType<ConvertibleFromIntExternallyMissingConverter* const>
   , SheddableType<ConvertibleFromIntExternallyMissingConverter* const&>
   , ConvertibleFromIntExternallyMissingConverter*
   , SheddableType<ConvertibleFromIntInternallyMissingConverter*>
   , SheddableType<ConvertibleFromIntInternallyMissingConverter* const>
   , SheddableType<ConvertibleFromIntInternallyMissingConverter* const&>
   , ConvertibleFromIntInternallyMissingConverter*   
   , ConvertibleFromIntExternally
   , ConvertibleFromIntExternally const
   , ConvertibleFromIntExternally&
   , ConvertibleFromIntInternally
   , ConvertibleFromIntInternally const
   , ConvertibleFromIntInternally&
   , SheddableType<InheritedConvertibleFromInt1Disabled*>
   , SheddableType<InheritedConvertibleFromInt1Disabled* const>
   , SheddableType<InheritedConvertibleFromInt1Disabled* const&>
   , InheritedConvertibleFromInt1Disabled*
   , SheddableType<InheritedConvertibleFromInt2*>
   , SheddableType<InheritedConvertibleFromInt2* const>
   , SheddableType<InheritedConvertibleFromInt2* const&>
   , InheritedConvertibleFromInt1
   , InheritedConvertibleFromInt1 const
   , InheritedConvertibleFromInt1&
   , InheritedConvertibleFromInt2*
   , InheritedConvertibleFromInt3
   , InheritedConvertibleFromInt3 const
   , InheritedConvertibleFromInt3&
   , SheddableType<InheritedConvertibleFromInt4*>
   , SheddableType<InheritedConvertibleFromInt4* const>
   , SheddableType<InheritedConvertibleFromInt4* const&>
   , InheritedConvertibleFromInt4*
   , IncompleteType*
   , Types<void*>
   , int*, float*, bool*

   , BuiltinConvertibleToIntViaOperator
   , BuiltinConvertibleToIntViaOperatorMutable
   , BuiltinConvertibleToIntViaExplicitOperator
   , BuiltinConvertibleToIntViaExplicitOperatorMutable
   , BuiltinConvertibleToIntBecauseAggregate
   , ConvertibleToIntExternallyMissingConverter
   , ConvertibleToIntExternally
   , ConvertibleToIntInternallyMissingConverter
   , ConvertibleToIntInternally
   , InheritedConvertibleToInt1
   , InheritedConvertibleToInt1Disabled
   , InheritedConvertibleToInt2
   , InheritedConvertibleToInt3
   , InheritedConvertibleToInt4
   , InheritedConvertibleToInt5
   , InheritedConvertibleToInt6
   , InheritedConvertibleToInt1ButPrivate
   , InheritedConvertibleToInt2ButPrivate
   , InheritedConvertibleToInt3ButPrivate
   , InheritedConvertibleToIntExternally
) {
   static_assert(not CT::ConvertibleImplicit<int, TestType>);
}

//static_assert(    CT::ConvertibleImplicit<>);     // shouldn't compile at all
//static_assert(    CT::ConvertibleImplicit<int>);  // shouldn't compile at all
static_assert(    CT::ConvertibleImplicit<int, int, float, bool>);
static_assert(not CT::ConvertibleImplicit<int, int, float, bool, void>);


///                                                                           
/// CT::ConvertibleExplicit                                                   
///                                                                           
TEST_CASE_TEMPLATE("Testing CT::ConvertibleExplicit from int", TestType
   //, IncompleteType                // shouldn't compile
   //, IncompleteType const          // shouldn't compile
   //, SheddableType<IncompleteType> // shouldn't compile
   , SheddableType<BuiltinConvertibleFromIntViaExplicitConstructor>
   , SheddableType<BuiltinConvertibleFromIntViaExplicitConstructor const>
   , SheddableType<BuiltinConvertibleFromIntViaExplicitConstructor const&>
   , BuiltinConvertibleFromIntViaExplicitConstructor
   , BuiltinConvertibleFromIntViaExplicitConstructor const
   , BuiltinConvertibleFromIntViaExplicitConstructor&
   , InheritedConvertibleFromInt3         //TODO why?
   , InheritedConvertibleFromInt3 const   //TODO why?
   , InheritedConvertibleFromInt3&        //TODO why?
   , SheddableType<BuiltinConvertibleFromIntViaConstructor>
   , SheddableType<BuiltinConvertibleFromIntViaConstructor const>
   , SheddableType<BuiltinConvertibleFromIntViaConstructor const&>
   , BuiltinConvertibleFromIntViaConstructor
   , BuiltinConvertibleFromIntViaConstructor const
   , BuiltinConvertibleFromIntViaConstructor&
   , int, const int, const int&, int&
   , float, float const, float const&, float&
   , bool, bool const, bool const&, bool&
   , BuiltinConvertibleToIntBecauseAggregate
) {
   static_assert(CT::ConvertibleExplicit<int, TestType>);
}

TEST_CASE_TEMPLATE("Testing not CT::ConvertibleExplicit from int", TestType
   //, IncompleteType                // shouldn't compile
   //, IncompleteType const          // shouldn't compile
   //, SheddableType<IncompleteType> // shouldn't compile
   , void
   , void*
   , SheddableType<ConvertibleFromIntExternallyMissingConverter*>
   , SheddableType<ConvertibleFromIntExternallyMissingConverter* const>
   , SheddableType<ConvertibleFromIntExternallyMissingConverter* const&>
   , ConvertibleFromIntExternallyMissingConverter*
   , SheddableType<ConvertibleFromIntInternallyMissingConverter*>
   , SheddableType<ConvertibleFromIntInternallyMissingConverter* const>
   , SheddableType<ConvertibleFromIntInternallyMissingConverter* const&>
   , ConvertibleFromIntInternallyMissingConverter*   
   , ConvertibleFromIntExternally
   , ConvertibleFromIntExternally const
   , ConvertibleFromIntExternally&
   , ConvertibleFromIntInternally
   , ConvertibleFromIntInternally const
   , ConvertibleFromIntInternally&
   , SheddableType<InheritedConvertibleFromInt1Disabled*>
   , SheddableType<InheritedConvertibleFromInt1Disabled* const>
   , SheddableType<InheritedConvertibleFromInt1Disabled* const&>
   , InheritedConvertibleFromInt1Disabled*
   , SheddableType<InheritedConvertibleFromInt2*>
   , SheddableType<InheritedConvertibleFromInt2* const>
   , SheddableType<InheritedConvertibleFromInt2* const&>
   , InheritedConvertibleFromInt1
   , InheritedConvertibleFromInt1 const
   , InheritedConvertibleFromInt1&
   , InheritedConvertibleFromInt2*
   , SheddableType<InheritedConvertibleFromInt4*>
   , SheddableType<InheritedConvertibleFromInt4* const>
   , SheddableType<InheritedConvertibleFromInt4* const&>
   , InheritedConvertibleFromInt4*
   , IncompleteType*
   , Types<void*>
   , int*, float*, bool*

   , BuiltinConvertibleToIntViaOperator
   , BuiltinConvertibleToIntViaOperatorMutable
   , BuiltinConvertibleToIntViaExplicitOperator
   , BuiltinConvertibleToIntViaExplicitOperatorMutable
   , ConvertibleToIntExternallyMissingConverter
   , ConvertibleToIntExternally
   , ConvertibleToIntInternallyMissingConverter
   , ConvertibleToIntInternally
   , InheritedConvertibleToInt1
   , InheritedConvertibleToInt1Disabled
   , InheritedConvertibleToInt2
   , InheritedConvertibleToInt3
   , InheritedConvertibleToInt4
   , InheritedConvertibleToInt5
   , InheritedConvertibleToInt6
   , InheritedConvertibleToInt1ButPrivate
   , InheritedConvertibleToInt2ButPrivate
   , InheritedConvertibleToInt3ButPrivate
   , InheritedConvertibleToIntExternally
) {
   static_assert(not CT::ConvertibleExplicit<int, TestType>);
}

//static_assert(    CT::ConvertibleExplicit<>);     // shouldn't compile at all
//static_assert(    CT::ConvertibleExplicit<int>);  // shouldn't compile at all
static_assert(    CT::ConvertibleExplicit<int, BuiltinConvertibleFromIntViaExplicitConstructor, BuiltinConvertibleFromIntViaExplicitConstructor, BuiltinConvertibleFromIntViaExplicitConstructor>);
static_assert(not CT::ConvertibleExplicit<int, BuiltinConvertibleFromIntViaExplicitConstructor, BuiltinConvertibleFromIntViaExplicitConstructor, IncompleteType*>);


///                                                                           
/// CT::ConvertibleCustom                                                     
///                                                                           
TEST_CASE_TEMPLATE("Testing CT::ConvertibleCustom from int", TestType
   //, IncompleteType                // shouldn't compile
   //, IncompleteType const          // shouldn't compile
   //, SheddableType<IncompleteType> // shouldn't compile
   , ConvertibleFromIntExternally
   , ConvertibleFromIntExternally const
   , ConvertibleFromIntExternally&
   , ConvertibleFromIntInternally
   , ConvertibleFromIntInternally const
   , ConvertibleFromIntInternally&
) {
   static_assert(CT::ConvertibleCustom<int, TestType>);
}

TEST_CASE_TEMPLATE("Testing not CT::ConvertibleCustom from int", TestType
   //, IncompleteType                // shouldn't compile
   //, IncompleteType const          // shouldn't compile
   //, SheddableType<IncompleteType> // shouldn't compile
   , SheddableType<BuiltinConvertibleFromIntViaExplicitConstructor>
   , SheddableType<BuiltinConvertibleFromIntViaExplicitConstructor const>
   , SheddableType<BuiltinConvertibleFromIntViaExplicitConstructor const&>
   , BuiltinConvertibleFromIntViaExplicitConstructor
   , BuiltinConvertibleFromIntViaExplicitConstructor const
   , BuiltinConvertibleFromIntViaExplicitConstructor&
   , InheritedConvertibleFromInt3         //TODO why?
   , InheritedConvertibleFromInt3 const   //TODO why?
   , InheritedConvertibleFromInt3&        //TODO why?
   , SheddableType<BuiltinConvertibleFromIntViaConstructor>
   , SheddableType<BuiltinConvertibleFromIntViaConstructor const>
   , SheddableType<BuiltinConvertibleFromIntViaConstructor const&>
   , BuiltinConvertibleFromIntViaConstructor
   , BuiltinConvertibleFromIntViaConstructor const
   , BuiltinConvertibleFromIntViaConstructor&
   , int, const int, const int&, int&
   , float, float const, float const&, float&
   , bool, bool const, bool const&, bool&
   , void
   , void*
   , SheddableType<ConvertibleFromIntExternallyMissingConverter*>
   , SheddableType<ConvertibleFromIntExternallyMissingConverter* const>
   , SheddableType<ConvertibleFromIntExternallyMissingConverter* const&>
   , ConvertibleFromIntExternallyMissingConverter*
   , SheddableType<ConvertibleFromIntInternallyMissingConverter*>
   , SheddableType<ConvertibleFromIntInternallyMissingConverter* const>
   , SheddableType<ConvertibleFromIntInternallyMissingConverter* const&>
   , ConvertibleFromIntInternallyMissingConverter*
   , SheddableType<InheritedConvertibleFromInt1Disabled*>
   , SheddableType<InheritedConvertibleFromInt1Disabled* const>
   , SheddableType<InheritedConvertibleFromInt1Disabled* const&>
   , InheritedConvertibleFromInt1Disabled*
   , SheddableType<InheritedConvertibleFromInt2*>
   , SheddableType<InheritedConvertibleFromInt2* const>
   , SheddableType<InheritedConvertibleFromInt2* const&>
   , InheritedConvertibleFromInt1
   , InheritedConvertibleFromInt1 const
   , InheritedConvertibleFromInt1&
   , InheritedConvertibleFromInt2*
   , SheddableType<InheritedConvertibleFromInt4*>
   , SheddableType<InheritedConvertibleFromInt4* const>
   , SheddableType<InheritedConvertibleFromInt4* const&>
   , InheritedConvertibleFromInt4*
   , IncompleteType*
   , Types<void*>
   , int*, float*, bool*

   , BuiltinConvertibleToIntViaOperator
   , BuiltinConvertibleToIntViaOperatorMutable
   , BuiltinConvertibleToIntViaExplicitOperator
   , BuiltinConvertibleToIntViaExplicitOperatorMutable
   , BuiltinConvertibleToIntBecauseAggregate
   , ConvertibleToIntExternallyMissingConverter
   , ConvertibleToIntExternally
   , ConvertibleToIntInternallyMissingConverter
   , ConvertibleToIntInternally
   , InheritedConvertibleToInt1
   , InheritedConvertibleToInt1Disabled
   , InheritedConvertibleToInt2
   , InheritedConvertibleToInt3
   , InheritedConvertibleToInt4
   , InheritedConvertibleToInt5
   , InheritedConvertibleToInt6
   , InheritedConvertibleToInt1ButPrivate
   , InheritedConvertibleToInt2ButPrivate
   , InheritedConvertibleToInt3ButPrivate
   , InheritedConvertibleToIntExternally
) {
   static_assert(not CT::ConvertibleCustom<int, TestType>);
}

//static_assert(    CT::ConvertibleCustom<>);     // shouldn't compile at all
//static_assert(    CT::ConvertibleCustom<int>);  // shouldn't compile at all
static_assert(    CT::ConvertibleCustom<int, ConvertibleFromIntExternally, ConvertibleFromIntInternally>);
static_assert(not CT::ConvertibleCustom<int, ConvertibleFromIntExternally, int>);


///                                                                           
/// CT::Convertible from int                                                  
///                                                                           
TEST_CASE_TEMPLATE("Testing CT::Convertible from int", TestType
   //, IncompleteType                // shouldn't compile
   //, IncompleteType const          // shouldn't compile
   //, SheddableType<IncompleteType> // shouldn't compile
   , SheddableType<BuiltinConvertibleFromIntViaConstructor>
   , SheddableType<BuiltinConvertibleFromIntViaConstructor const>
   , SheddableType<BuiltinConvertibleFromIntViaConstructor const&>
   , BuiltinConvertibleFromIntViaConstructor
   , BuiltinConvertibleFromIntViaConstructor const
   , BuiltinConvertibleFromIntViaConstructor&
   , SheddableType<BuiltinConvertibleFromIntViaExplicitConstructor>
   , SheddableType<BuiltinConvertibleFromIntViaExplicitConstructor const>
   , SheddableType<BuiltinConvertibleFromIntViaExplicitConstructor const&>
   , BuiltinConvertibleFromIntViaExplicitConstructor
   , BuiltinConvertibleFromIntViaExplicitConstructor const
   , BuiltinConvertibleFromIntViaExplicitConstructor&
   , ConvertibleFromIntExternally
   , ConvertibleFromIntExternally const
   , ConvertibleFromIntExternally&
   , ConvertibleFromIntInternally
   , ConvertibleFromIntInternally const
   , ConvertibleFromIntInternally&
   , InheritedConvertibleFromInt3
   , InheritedConvertibleFromInt3 const
   , InheritedConvertibleFromInt3&
   , int, const int, const int&, int&
   , float, float const, float const&, float&
   , bool, bool const, bool const&, bool&
   , BuiltinConvertibleToIntBecauseAggregate
) {
   static_assert(CT::Convertible<int, TestType>);
   static_assert(CT::ConvertibleToOneOf<int, void, void*, TestType>);
}

TEST_CASE_TEMPLATE("Testing not CT::Convertible from int", TestType
   //, IncompleteType                // shouldn't compile
   //, IncompleteType const          // shouldn't compile
   //, SheddableType<IncompleteType> // shouldn't compile
   , void
   , void*
   , SheddableType<ConvertibleFromIntExternallyMissingConverter*>
   , SheddableType<ConvertibleFromIntExternallyMissingConverter* const>
   , SheddableType<ConvertibleFromIntExternallyMissingConverter* const&>
   , ConvertibleFromIntExternallyMissingConverter*
   , SheddableType<ConvertibleFromIntInternallyMissingConverter*>
   , SheddableType<ConvertibleFromIntInternallyMissingConverter* const>
   , SheddableType<ConvertibleFromIntInternallyMissingConverter* const&>
   , ConvertibleFromIntInternallyMissingConverter*
   , SheddableType<InheritedConvertibleFromInt1Disabled*>
   , SheddableType<InheritedConvertibleFromInt1Disabled* const>
   , SheddableType<InheritedConvertibleFromInt1Disabled* const&>
   , InheritedConvertibleFromInt1Disabled*
   , SheddableType<InheritedConvertibleFromInt2*>
   , SheddableType<InheritedConvertibleFromInt2* const>
   , SheddableType<InheritedConvertibleFromInt2* const&>
   , InheritedConvertibleFromInt1
   , InheritedConvertibleFromInt1 const
   , InheritedConvertibleFromInt1&
   , InheritedConvertibleFromInt2*
   , SheddableType<InheritedConvertibleFromInt4*>
   , SheddableType<InheritedConvertibleFromInt4* const>
   , SheddableType<InheritedConvertibleFromInt4* const&>
   , InheritedConvertibleFromInt4*
   , IncompleteType*
   , Types<void*>
   , int*, float*, bool*

   , BuiltinConvertibleToIntViaOperator
   , BuiltinConvertibleToIntViaOperatorMutable
   , BuiltinConvertibleToIntViaExplicitOperator
   , BuiltinConvertibleToIntViaExplicitOperatorMutable
   , ConvertibleToIntExternallyMissingConverter
   , ConvertibleToIntExternally
   , ConvertibleToIntInternallyMissingConverter
   , ConvertibleToIntInternally
   , InheritedConvertibleToInt1
   , InheritedConvertibleToInt1Disabled
   , InheritedConvertibleToInt2
   , InheritedConvertibleToInt3
   , InheritedConvertibleToInt4
   , InheritedConvertibleToInt5
   , InheritedConvertibleToInt6
   , InheritedConvertibleToInt1ButPrivate
   , InheritedConvertibleToInt2ButPrivate
   , InheritedConvertibleToInt3ButPrivate
   , InheritedConvertibleToIntExternally
) {
   static_assert(not CT::Convertible<int, TestType>);
   static_assert(not CT::ConvertibleToOneOf<int, TestType, SheddableType<TestType>>);
}

//static_assert(    CT::Convertible<>);     // shouldn't compile at all
//static_assert(    CT::Convertible<int>);  // shouldn't compile at all
static_assert(    CT::Convertible<int, int, float, bool>);
static_assert(not CT::Convertible<int, int, float, bool, void>);


///                                                                           
/// CT::Convertible to int                                                    
///                                                                           
TEST_CASE_TEMPLATE("Testing CT::Convertible to int", TestType
   //, IncompleteType                // shouldn't compile
   //, IncompleteType const          // shouldn't compile
   //, SheddableType<IncompleteType> // shouldn't compile
   , BuiltinConvertibleToIntViaOperator
   , BuiltinConvertibleToIntViaOperatorMutable
   , BuiltinConvertibleToIntViaExplicitOperator
   , BuiltinConvertibleToIntViaExplicitOperatorMutable
   , ConvertibleToIntExternally
   , ConvertibleToIntInternally
   , InheritedConvertibleToInt3
   , InheritedConvertibleToInt4
   , InheritedConvertibleToInt5
   , InheritedConvertibleToInt6
   , int, const int, const int&, int&
   , float, float const, float const&, float&
   , bool, bool const, bool const&, bool&
) {
   static_assert(CT::Convertible<TestType, int>);
}

TEST_CASE_TEMPLATE("Testing not CT::Convertible to int", TestType
   //, IncompleteType                // shouldn't compile
   //, IncompleteType const          // shouldn't compile
   //, SheddableType<IncompleteType> // shouldn't compile
   , void
   , void*
   , SheddableType<ConvertibleFromIntExternallyMissingConverter*>
   , SheddableType<ConvertibleFromIntExternallyMissingConverter* const>
   , SheddableType<ConvertibleFromIntExternallyMissingConverter* const&>
   , ConvertibleFromIntExternallyMissingConverter*
   , SheddableType<ConvertibleFromIntInternallyMissingConverter*>
   , SheddableType<ConvertibleFromIntInternallyMissingConverter* const>
   , SheddableType<ConvertibleFromIntInternallyMissingConverter* const&>
   , ConvertibleFromIntInternallyMissingConverter*
   , SheddableType<InheritedConvertibleFromInt1Disabled*>
   , SheddableType<InheritedConvertibleFromInt1Disabled* const>
   , SheddableType<InheritedConvertibleFromInt1Disabled* const&>
   , InheritedConvertibleFromInt1Disabled*
   , SheddableType<InheritedConvertibleFromInt2*>
   , SheddableType<InheritedConvertibleFromInt2* const>
   , SheddableType<InheritedConvertibleFromInt2* const&>
   , InheritedConvertibleFromInt1
   , InheritedConvertibleFromInt1 const
   , InheritedConvertibleFromInt1&
   , InheritedConvertibleFromInt2*
   , SheddableType<InheritedConvertibleFromInt4*>
   , SheddableType<InheritedConvertibleFromInt4* const>
   , SheddableType<InheritedConvertibleFromInt4* const&>
   , InheritedConvertibleFromInt4*
   , IncompleteType*
   , Types<void*>
   , int*, float*, bool*

   , SheddableType<BuiltinConvertibleFromIntViaConstructor>
   , SheddableType<BuiltinConvertibleFromIntViaConstructor const>
   , SheddableType<BuiltinConvertibleFromIntViaConstructor const&>
   , BuiltinConvertibleFromIntViaConstructor
   , BuiltinConvertibleFromIntViaConstructor const
   , BuiltinConvertibleFromIntViaConstructor&
   , SheddableType<BuiltinConvertibleFromIntViaExplicitConstructor>
   , SheddableType<BuiltinConvertibleFromIntViaExplicitConstructor const>
   , SheddableType<BuiltinConvertibleFromIntViaExplicitConstructor const&>
   , BuiltinConvertibleFromIntViaExplicitConstructor
   , BuiltinConvertibleFromIntViaExplicitConstructor const
   , BuiltinConvertibleFromIntViaExplicitConstructor&
   , ConvertibleFromIntExternally
   , ConvertibleFromIntExternally const
   , ConvertibleFromIntExternally&
   , ConvertibleFromIntInternally
   , ConvertibleFromIntInternally const
   , ConvertibleFromIntInternally&
   , InheritedConvertibleFromInt3
   , InheritedConvertibleFromInt3 const
   , InheritedConvertibleFromInt3&
   , BuiltinConvertibleToIntBecauseAggregate

   //, ConvertibleToIntExternallyMissingConverter // shouldn't compile
   , ConvertibleToIntInternallyMissingConverter
   , InheritedConvertibleToInt1
   , InheritedConvertibleToInt1Disabled
   , InheritedConvertibleToInt2
   , InheritedConvertibleToInt1ButPrivate
   , InheritedConvertibleToInt2ButPrivate
   , InheritedConvertibleToInt3ButPrivate
   , InheritedConvertibleToIntExternally
) {
   static_assert(not CT::Convertible<TestType, int>);
}

///                                                                           
/// Langulus::MorphismsFrom                                                   
///                                                                           
TEST_CASE_TEMPLATE("Testing Langulus::MorphismsFrom to int", TestType
   //, IncompleteType                // shouldn't compile
   //, IncompleteType const          // shouldn't compile
   //, SheddableType<IncompleteType> // shouldn't compile
   , ConvertibleToIntExternally
   , ConvertibleToIntInternally
) {
   static_assert(Exact<Langulus::MorphismsFrom<TestType>, Types<int>>);
}

TEST_CASE_TEMPLATE("Testing Langulus::MorphismsFrom to nothing", TestType
   //, IncompleteType                // shouldn't compile
   //, IncompleteType const          // shouldn't compile
   //, SheddableType<IncompleteType> // shouldn't compile
   , BuiltinConvertibleToIntViaOperator
   , BuiltinConvertibleToIntViaOperatorMutable
   , BuiltinConvertibleToIntViaExplicitOperator
   , BuiltinConvertibleToIntViaExplicitOperatorMutable
   , InheritedConvertibleToInt3
   , InheritedConvertibleToInt4
   , InheritedConvertibleToInt5
   , InheritedConvertibleToInt6
   //, int, const int, const int&, int& // struct ConverterFrom<int> exists
   , float, float const, float const&, float&
   , bool, bool const, bool const&, bool&
) {
   static_assert(Exact<Langulus::MorphismsFrom<TestType>, Types<>>);
}

///                                                                           
/// Langulus::MorphismsTo                                                     
///                                                                           
/*TEST_CASE_TEMPLATE("Testing Langulus::MorphismsTo from int", TestType
   //, IncompleteType                // shouldn't compile
   //, IncompleteType const          // shouldn't compile
   //, SheddableType<IncompleteType> // shouldn't compile
   , ConvertibleFromIntExternally
   , ConvertibleFromIntInternally
) {
   static_assert(Exact<Langulus::MorphismsTo<TestType>, Types<int>>);
}

TEST_CASE_TEMPLATE("Testing Langulus::MorphismsTo from ::std::string (defined in Main.hpp)", TestType
   //, IncompleteType                // shouldn't compile
   //, IncompleteType const          // shouldn't compile
   //, SheddableType<IncompleteType> // shouldn't compile
   , int, const int, const int&, int&
) {
   static_assert(Exact<Langulus::MorphismsTo<TestType>, Types<::std::string>>);
}*/