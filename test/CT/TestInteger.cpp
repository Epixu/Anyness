///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "../Main.hpp"
#include <Langulus/CT/Integer.hpp>

using namespace Langulus;

namespace
{
   template<class T>
   struct SheddableType { using CTTI_Sheddable = T; };
   struct IncompleteType;

   struct IntExternally {};
   struct NotIntExternally {};
   struct IntInternally { using CTTI_Integer = Yes<>; };
   struct InheritedInt : IntInternally {};
   struct InheritedIntDisabled : IntInternally { using CTTI_Integer = No; };
   struct InheritedIntButPrivate : private IntInternally {};
   struct InheritedIntExternally : IntExternally {};
}

namespace Langulus::CTTI
{
   template<>
   struct Integer<IntExternally> {};
   template<>
   struct Integer<NotIntExternally> {
      static constexpr bool Enabled = false;
   };
}


///                                                                           
/// CT::Integer                                                               
///                                                                           
TEST_CASE_TEMPLATE("Testing CT::Integer types", TestType
   //, IncompleteType                  // shouldn't compile
   //, IncompleteType const            // shouldn't compile
   //, IncompleteType const&           // shouldn't compile
   //, SheddableType<IncompleteType>   // shouldn't compile
   //, SheddableType<IncompleteType>&  // shouldn't compile
   , IntExternally
   , IntExternally const
   , IntExternally&
   , IntInternally
   , IntInternally const
   , IntInternally&
   , InheritedInt
   , InheritedInt const
   , InheritedInt&
   , int8_t,  int16_t,  int32_t,  int64_t
   , uint8_t, uint16_t, uint32_t, uint64_t
   , char, wchar_t, char8_t, char16_t, char32_t
) {
   static_assert(    CT::Integer<TestType>);
   static_assert(not CT::NotInteger<TestType>);
}

TEST_CASE_TEMPLATE("Testing CT::NotInteger types", TestType
   //, IncompleteType                  // shouldn't compile
   //, IncompleteType const            // shouldn't compile
   //, IncompleteType const&           // shouldn't compile
   //, SheddableType<IncompleteType>   // shouldn't compile
   //, SheddableType<IncompleteType>&  // shouldn't compile
   , IncompleteType*
   , void, void*
   , bool
   , Types<void*>
   , SheddableType<IntInternally*>
   , SheddableType<IntInternally* const>
   , SheddableType<IntInternally* const&>
   , SheddableType<InheritedIntDisabled>
   , SheddableType<InheritedIntDisabled const>
   , SheddableType<InheritedIntDisabled const&>
   , InheritedIntDisabled
   , InheritedIntExternally
   , InheritedIntButPrivate
   , NotIntExternally
) {
   static_assert(not CT::Integer<TestType>);
   static_assert(    CT::NotInteger<TestType>);
}

//static_assert(    CT::Integer<>); // shouldn't compile at all
static_assert(    CT::Integer<IntExternally, IntInternally, int>);
static_assert(not CT::Integer<IntExternally, IntInternally, bool>);

//static_assert(    CT::NotInteger<>); // shouldn't compile at all
static_assert(    CT::NotInteger<InheritedIntDisabled, InheritedIntExternally, bool>);
static_assert(not CT::NotInteger<InheritedIntDisabled, InheritedIntExternally, int>);
