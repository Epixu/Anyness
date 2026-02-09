///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "../Main.hpp"
#include <Langulus/CT/Convertible.hpp>

using namespace Langulus;

namespace
{
   template<class T>
   struct SheddableType { using CTTI_Sheddable = T; };
   struct IncompleteType;

   /// Built-in convertible from int by using constructor                     
   struct BuiltinConvertibleFromIntViaConstructor {
      int inner = 0;
      BuiltinConvertibleFromIntViaConstructor(int x) : inner {x} {}
   };

   /// Built-in convertible from int by using explicit constructor            
   struct BuiltinConvertibleFromIntViaExplicitConstructor {
      int inner = 0;
      explicit BuiltinConvertibleFromIntViaExplicitConstructor(int x) : inner {x} {}
   };

   /// Proper types, reflected as convertible from int                        
   struct ConvertibleFromIntExternallyMisingConverter {};
   class  ConvertibleFromIntExternally {
      int inner = 0;
   public:
      static ConvertibleFromIntExternally Init(int i) { 
         ConvertibleFromIntExternally temp;
         temp.inner = i;
         return temp;
      }
   };

   struct ConvertibleFromIntInternallyMissingConverter {
      using CTTI_MapsFrom = int;
   };

   class ConvertibleFromIntInternally {
      int inner = 0;
   public:
      using CTTI_MapsFrom = int;
      static ConvertibleFromIntInternally Init(int i) { 
         ConvertibleFromIntInternally temp;
         temp.inner = i;
         return temp;
      }
   };

   /// Types that inherit convertible properties                              
   struct InheritedConvertibleFromInt1
      : ConvertibleFromIntInternally {};
   struct InheritedConvertibleFromInt1Disabled
      : ConvertibleFromIntInternally { using CTTI_MapsFrom = void; };
   struct InheritedConvertibleFromInt2
      : ConvertibleFromIntExternally {};
   struct InheritedConvertibleFromInt3
      : BuiltinConvertibleFromIntViaConstructor {};
   struct InheritedConvertibleFromInt4
      : BuiltinConvertibleFromIntViaExplicitConstructor {};

   /// Types that inherit convertible properties privately                    
   struct InheritedConvertibleFromInt1ButPrivate : private ConvertibleFromIntInternally {};
   struct InheritedConvertibleFromInt2ButPrivate : private BuiltinConvertibleFromIntViaConstructor {};
   struct InheritedConvertibleFromInt3ButPrivate : private BuiltinConvertibleFromIntViaExplicitConstructor {};
   struct InheritedConvertibleFromIntExternally : ConvertibleFromIntExternally {};
}

namespace Langulus::CTTI
{
   template<>
   struct MapsTo<ConvertibleFromIntExternallyMisingConverter> {
      using From = int;
   };
   template<>
   struct MapsTo<ConvertibleFromIntExternally> {
      using From = int;
   };
   template<>
   struct Converter<int, ConvertibleFromIntExternally> {
      static constexpr auto Convert(int const& from) -> ConvertibleFromIntExternally {
         return ConvertibleFromIntExternally::Init(from);
      }
   };
   template<>
   struct Converter<int, ConvertibleFromIntInternally> {
      static constexpr auto Convert(int const& from) -> ConvertibleFromIntInternally {
         return ConvertibleFromIntInternally::Init(from);
      }
   };
}


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
   , SheddableType<ConvertibleFromIntExternallyMisingConverter*>
   , SheddableType<ConvertibleFromIntExternallyMisingConverter* const>
   , SheddableType<ConvertibleFromIntExternallyMisingConverter* const&>
   , ConvertibleFromIntExternallyMisingConverter*
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
) {
   static_assert(CT::ConvertibleExplicit<int, TestType>);
}

TEST_CASE_TEMPLATE("Testing not CT::ConvertibleExplicit from int", TestType
   //, IncompleteType                // shouldn't compile
   //, IncompleteType const          // shouldn't compile
   //, SheddableType<IncompleteType> // shouldn't compile
   , void
   , void*
   , SheddableType<ConvertibleFromIntExternallyMisingConverter*>
   , SheddableType<ConvertibleFromIntExternallyMisingConverter* const>
   , SheddableType<ConvertibleFromIntExternallyMisingConverter* const&>
   , ConvertibleFromIntExternallyMisingConverter*
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
   , SheddableType<ConvertibleFromIntExternallyMisingConverter*>
   , SheddableType<ConvertibleFromIntExternallyMisingConverter* const>
   , SheddableType<ConvertibleFromIntExternallyMisingConverter* const&>
   , ConvertibleFromIntExternallyMisingConverter*
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
) {
   static_assert(not CT::ConvertibleCustom<int, TestType>);
}

//static_assert(    CT::ConvertibleCustom<>);     // shouldn't compile at all
//static_assert(    CT::ConvertibleCustom<int>);  // shouldn't compile at all
static_assert(    CT::ConvertibleCustom<int, ConvertibleFromIntExternally, ConvertibleFromIntInternally>);
static_assert(not CT::ConvertibleCustom<int, ConvertibleFromIntExternally, int>);


///                                                                           
/// CT::Convertible                                                           
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
   , SheddableType<ConvertibleFromIntExternallyMisingConverter*>
   , SheddableType<ConvertibleFromIntExternallyMisingConverter* const>
   , SheddableType<ConvertibleFromIntExternallyMisingConverter* const&>
   , ConvertibleFromIntExternallyMisingConverter*
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
) {
   static_assert(not CT::Convertible<int, TestType>);
   static_assert(not CT::ConvertibleToOneOf<int, TestType, SheddableType<TestType>>);
}

//static_assert(    CT::Convertible<>);     // shouldn't compile at all
//static_assert(    CT::Convertible<int>);  // shouldn't compile at all
static_assert(    CT::Convertible<int, int, float, bool>);
static_assert(not CT::Convertible<int, int, float, bool, void>);