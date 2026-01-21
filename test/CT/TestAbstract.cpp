///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "../Main.hpp"
#include <Langulus/CT/Abstract.hpp>

using namespace Langulus;

namespace
{
   template<class T>
   struct SheddableType { using CTTI_Sheddable = T; };

   struct IncompleteType;

   /// Built-in abstract type via a pure virtual function                     
   struct PureAbstract {
      PureAbstract() = delete;
      virtual ~PureAbstract() {}
      PureAbstract(void*) {}
      [[maybe_unused]] virtual auto PureVirtualMethod() -> size_t = 0;
   };

   /// Proper type, reflected as abstract                                     
   struct ForcedAbstractExternally {};
   struct ForcedAbstractInternally {
      // ReSharper disable once CppTypeAliasNeverUsed
      using CTTI_Abstract = Yes<>;
   };

   /// Types that can inherit abstractness                                    
   struct InheritedAbstract1 : ForcedAbstractInternally {};
   // ReSharper disable once CppTypeAliasNeverUsed
   struct InheritedAbstract1Disabled : ForcedAbstractInternally { using CTTI_Abstract = No; };
   struct InheritedAbstract2 : PureAbstract {};

   /// Types that can inherit abstractness privately                          
   struct ImpureVirtual {
      virtual ~ImpureVirtual() {}
   };
   struct InheritedAbstract1ButPrivate : private ForcedAbstractInternally {};
   struct InheritedAbstract2ButPrivate : private PureAbstract {};
   struct InheritedAbstractExternally : ForcedAbstractExternally {};
}

namespace Langulus::CTTI
{
   template<>
   struct Abstract<ForcedAbstractExternally> {};
}


///                                                                           
/// CT::Abstract                                                              
///                                                                           
TEST_CASE_TEMPLATE("Testing CT::Abstract types", TestType
   //, IncompleteType                // shouldn't compile
   //, IncompleteType const          // shouldn't compile
   //, SheddableType<IncompleteType> // shouldn't compile
   , SheddableType<PureAbstract>
   , SheddableType<PureAbstract const>
   , SheddableType<PureAbstract const&>
   , PureAbstract
   , PureAbstract const
   , PureAbstract&
   , ForcedAbstractExternally
   , ForcedAbstractExternally const
   , ForcedAbstractExternally&
   , ForcedAbstractInternally
   , ForcedAbstractInternally const
   , ForcedAbstractInternally&
   , InheritedAbstract1
   , InheritedAbstract1 const
   , InheritedAbstract1&
   , InheritedAbstract2ButPrivate
   , InheritedAbstract2
   , InheritedAbstract2 const
   , InheritedAbstract2&
) {
   static_assert(    CT::Abstract<TestType>);
   static_assert(not CT::NotAbstract<TestType>);
}

TEST_CASE_TEMPLATE("Testing CT::NotAbstract types", TestType
   //, IncompleteType                // shouldn't compile
   //, IncompleteType const          // shouldn't compile
   //, SheddableType<IncompleteType> // shouldn't compile
   , void
   , void*
   , SheddableType<PureAbstract*>
   , SheddableType<PureAbstract* const>
   , SheddableType<PureAbstract* const&>
   , PureAbstract*
   , ImpureVirtual
   , InheritedAbstract1ButPrivate
   , InheritedAbstractExternally
   , IncompleteType*
   , int
   , int const
   , int const&
   , int&
   , Types<void*>
   , InheritedAbstract1Disabled
) {
   static_assert(not CT::Abstract<TestType>);
   static_assert(    CT::NotAbstract<TestType>);
}

//static_assert(    CT::Abstract<>); // shouldn't compile at all
static_assert(    CT::Abstract<ForcedAbstractExternally, PureAbstract, ForcedAbstractInternally>);
static_assert(not CT::Abstract<ForcedAbstractExternally, PureAbstract, int>);

//static_assert(    CT::NotAbstract<>); // shouldn't compile at all
static_assert(    CT::NotAbstract<void*, ImpureVirtual, int>);
static_assert(not CT::NotAbstract<void*, ImpureVirtual, ForcedAbstractInternally>);
