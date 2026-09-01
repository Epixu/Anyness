///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "../Main.hpp"
#include <Langulus/CT/Number.hpp>

using namespace Langulus;

namespace
{
   template<class T>
   struct SheddableType { using CTTI_Sheddable = T; };
   struct IncompleteType;

   struct NumberExternally {};
   struct NotNumberExternally {};
   struct NumberInternally { using CTTI_Number = Yes<>; };
   struct InheritedNumber : NumberInternally {};
   struct InheritedNumberDisabled : NumberInternally { using CTTI_Number = No; };
   struct InheritedNumberButPrivate : private NumberInternally {};
   struct InheritedNumberExternally : NumberExternally {};
}

namespace Langulus::CTTI
{
   template<>
   struct Number<NumberExternally> {};
   template<>
   struct Number<NotNumberExternally> {
      static constexpr bool Enabled = false;
   };
}


///                                                                           
/// CT::Number                                                                
///                                                                           
TEST_CASE_TEMPLATE("Testing CT::Number types", TestType
   //, IncompleteType                  // shouldn't compile
   //, IncompleteType const            // shouldn't compile
   //, IncompleteType const&           // shouldn't compile
   //, SheddableType<IncompleteType>   // shouldn't compile
   //, SheddableType<IncompleteType>&  // shouldn't compile
   , NumberExternally
   , NumberExternally const
   , NumberExternally&
   , NumberInternally
   , NumberInternally const
   , NumberInternally&
   , InheritedNumber
   , InheritedNumber const
   , InheritedNumber&
   , float, double
   , int8_t,  int16_t,  int32_t,  int64_t
   , uint8_t, uint16_t, uint32_t, uint64_t
   , char, wchar_t, char8_t, char16_t, char32_t
) {
   static_assert(    CT::Number<TestType>);
   static_assert(not CT::NotNumber<TestType>);
}

TEST_CASE_TEMPLATE("Testing CT::NotNumber types", TestType
   //, IncompleteType                  // shouldn't compile
   //, IncompleteType const            // shouldn't compile
   //, IncompleteType const&           // shouldn't compile
   //, SheddableType<IncompleteType>   // shouldn't compile
   //, SheddableType<IncompleteType>&  // shouldn't compile
   , IncompleteType*
   , void, void*
   , bool
   , Types<void*>
   , SheddableType<NumberInternally*>
   , SheddableType<NumberInternally* const>
   , SheddableType<NumberInternally* const&>
   , SheddableType<InheritedNumberDisabled>
   , SheddableType<InheritedNumberDisabled const>
   , SheddableType<InheritedNumberDisabled const&>
   , InheritedNumberDisabled
   , InheritedNumberExternally
   , InheritedNumberButPrivate
   , NotNumberExternally
) {
   static_assert(not CT::Number<TestType>);
   static_assert(    CT::NotNumber<TestType>);
}

//static_assert(    CT::Number<>); // shouldn't compile at all
static_assert(    CT::Number<NumberExternally, NumberInternally, float>);
static_assert(not CT::Number<NumberExternally, NumberInternally, bool>);

//static_assert(    CT::NotNumber<>); // shouldn't compile at all
static_assert(    CT::NotNumber<InheritedNumberDisabled, InheritedNumberExternally, bool>);
static_assert(not CT::NotNumber<InheritedNumberDisabled, InheritedNumberExternally, float>);
