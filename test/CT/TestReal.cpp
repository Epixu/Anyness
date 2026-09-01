///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "../Main.hpp"
#include <Langulus/CT/Real.hpp>

using namespace Langulus;

namespace
{
   template<class T>
   struct SheddableType { using CTTI_Sheddable = T; };
   struct IncompleteType;

   struct RealExternally {};
   struct NotRealExternally {};
   struct RealInternally { using CTTI_Real = Yes<>; };
   struct InheritedReal : RealInternally {};
   struct InheritedRealDisabled : RealInternally { using CTTI_Real = No; };
   struct InheritedRealButPrivate : private RealInternally {};
   struct InheritedRealExternally : RealExternally {};
}

namespace Langulus::CTTI
{
   template<>
   struct Real<RealExternally> {};
   template<>
   struct Real<NotRealExternally> {
      static constexpr bool Enabled = false;
   };
}


///                                                                           
/// CT::Real                                                                  
///                                                                           
TEST_CASE_TEMPLATE("Testing CT::Real types", TestType
   //, IncompleteType                  // shouldn't compile
   //, IncompleteType const            // shouldn't compile
   //, IncompleteType const&           // shouldn't compile
   //, SheddableType<IncompleteType>   // shouldn't compile
   //, SheddableType<IncompleteType>&  // shouldn't compile
   , RealExternally
   , RealExternally const
   , RealExternally&
   , RealInternally
   , RealInternally const
   , RealInternally&
   , InheritedReal
   , InheritedReal const
   , InheritedReal&
   , float, double
) {
   static_assert(    CT::Real<TestType>);
   static_assert(not CT::NotReal<TestType>);
}

TEST_CASE_TEMPLATE("Testing CT::NotReal types", TestType
   //, IncompleteType                  // shouldn't compile
   //, IncompleteType const            // shouldn't compile
   //, IncompleteType const&           // shouldn't compile
   //, SheddableType<IncompleteType>   // shouldn't compile
   //, SheddableType<IncompleteType>&  // shouldn't compile
   , IncompleteType*
   , void, void*
   , bool
   , int8_t,  int16_t,  int32_t,  int64_t
   , uint8_t, uint16_t, uint32_t, uint64_t
   , char, wchar_t, char8_t, char16_t, char32_t
   , Types<void*>
   , SheddableType<RealInternally*>
   , SheddableType<RealInternally* const>
   , SheddableType<RealInternally* const&>
   , SheddableType<InheritedRealDisabled>
   , SheddableType<InheritedRealDisabled const>
   , SheddableType<InheritedRealDisabled const&>
   , InheritedRealDisabled
   , InheritedRealExternally
   , InheritedRealButPrivate
   , NotRealExternally
) {
   static_assert(not CT::Real<TestType>);
   static_assert(    CT::NotReal<TestType>);
}

//static_assert(    CT::Real<>); // shouldn't compile at all
static_assert(    CT::Real<RealExternally, RealInternally, float>);
static_assert(not CT::Real<RealExternally, RealInternally, bool>);

//static_assert(    CT::NotReal<>); // shouldn't compile at all
static_assert(    CT::NotReal<InheritedRealDisabled, InheritedRealExternally, bool>);
static_assert(not CT::NotReal<InheritedRealDisabled, InheritedRealExternally, float>);
