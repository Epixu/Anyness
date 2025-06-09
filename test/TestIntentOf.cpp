///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "Main.hpp"
#include <Langulus/IntentOf.hpp>

using namespace Langulus;

namespace
{
   struct EmptyType {};

   struct AggregateType {
      int i;
      float f;
   };

   struct DestructibleType {
      char* p {};

      ~DestructibleType() { if (p) delete p; }
   };
}

///                                                                           
/// CT::Intent / CT::NoIntent                                                 
///                                                                           
namespace
{
   template<class T>
   struct SheddableType {
      using CTTI_Sheddable = Yes;
      using CTTI_Typed = T;

      T instance;

      SheddableType(T t) : instance {FWD(t)} {}
   };

   enum TypedEnum : int64_t {one1, two2};
   enum class TypedEnumClass : int64_t {one1, two2};
   struct IncompleteType;
}

TEMPLATE_TEST_CASE("Testing intent type", "[ct]",
   Refer<int>,
   Copy<int>,
   Clone<int>,
   Abandon<int>,
   Move<int>,
   Disown<int>,
   Disown<int>&,
   Disown<int> const&,
   Disown<int>&&,
   Disown<int> const&&
) {
   static_assert(    CT::Intent<TestType>);
   static_assert(not CT::NoIntent<TestType>);
}

TEMPLATE_TEST_CASE("Testing non-intent type", "[ct]",
   Refer<int>*,
   SheddableType<int>,
   SheddableType<Refer<int>>,
   IncompleteType,
   TypedEnum,
   void, int, int*, ::std::nullptr_t
) {
   static_assert(not CT::Intent<TestType>);
   static_assert(    CT::NoIntent<TestType>);
}

//static_assert(CT::Intent<>); // shouldn't compile at all
static_assert(    CT::Intent<Refer<int>, Copy<int>, Clone<int>>);
static_assert(not CT::Intent<Refer<int>, Copy<int>, int>);

//static_assert(CT::NoIntent<>); // shouldn't compile at all
static_assert(    CT::NoIntent<Refer<int>*, SheddableType<int>, SheddableType<Refer<int>>>);
static_assert(not CT::NoIntent<Refer<int>*, SheddableType<int>, Refer<int>>);


///                                                                           
/// IntentOf                                                                  
///                                                                           
TEST_CASE("Testing IntentOf", "[ct]") {
   static_assert(::std::same_as<IntentOf<int>,                 Refer<int>>);
   static_assert(::std::same_as<IntentOf<int&&>,               Move<int>>);
   static_assert(::std::same_as<IntentOf<int const&&>,         Refer<const int>>);
   static_assert(::std::same_as<IntentOf<int&>,                Refer<int>>);
   static_assert(::std::same_as<IntentOf<int const&>,          Refer<const int>>);

   static_assert(::std::same_as<IntentOf<Copy<int>>,           Copy<int>>);
   static_assert(::std::same_as<IntentOf<Copy<int>&>,          Copy<int>>);
   static_assert(::std::same_as<IntentOf<Copy<int>&&>,         Copy<int>>);
   static_assert(::std::same_as<IntentOf<Copy<int> const&>,    Copy<int>>);
                                                               
   static_assert(::std::same_as<IntentOf<Refer<int>>,          Refer<int>>);
   static_assert(::std::same_as<IntentOf<Refer<int>&>,         Refer<int>>);
   static_assert(::std::same_as<IntentOf<Refer<int>&&>,        Refer<int>>);
   static_assert(::std::same_as<IntentOf<Refer<int> const&>,   Refer<int>>);
                                                               
   static_assert(::std::same_as<IntentOf<Move<int>>,           Move<int>>);
   static_assert(::std::same_as<IntentOf<Move<int>&>,          Move<int>>);
   static_assert(::std::same_as<IntentOf<Move<int>&&>,         Move<int>>);
   static_assert(::std::same_as<IntentOf<Move<int> const&>,    Move<int>>);

   static_assert(::std::same_as<IntentOf<Abandon<int>>,        Abandon<int>>);
   static_assert(::std::same_as<IntentOf<Abandon<int>&>,       Abandon<int>>);
   static_assert(::std::same_as<IntentOf<Abandon<int>&&>,      Abandon<int>>);
   static_assert(::std::same_as<IntentOf<Abandon<int> const&>, Abandon<int>>);

   static_assert(::std::same_as<IntentOf<Disown<int>>,         Disown<int>>);
   static_assert(::std::same_as<IntentOf<Disown<int>&>,        Disown<int>>);
   static_assert(::std::same_as<IntentOf<Disown<int>&&>,       Disown<int>>);
   static_assert(::std::same_as<IntentOf<Disown<int> const&>,  Disown<int>>);

   static_assert(::std::same_as<IntentOf<Clone<int>>,          Clone<int>>);
   static_assert(::std::same_as<IntentOf<Clone<int>&>,         Clone<int>>);
   static_assert(::std::same_as<IntentOf<Clone<int>&&>,        Clone<int>>);
   static_assert(::std::same_as<IntentOf<Clone<int> const&>,   Clone<int>>);

   const std::string_view anArrayOfStrings[] {
      "one", "two", "three", "four"
   };

   using AOS = decltype(anArrayOfStrings);
   static_assert(::std::same_as<IntentOf<AOS>,   Refer<AOS>>);
   static_assert(::std::same_as<IntentOf<AOS&&>, Refer<AOS>>);
}


///                                                                           
/// Deint                                                                     
///                                                                           
TEST_CASE("Testing Deint", "[ct]") {
   static_assert(::std::same_as<Deint<   Copy<int>>, int const&>);
   static_assert(::std::same_as<Deint<  Refer<int>>, int const&>);
   static_assert(::std::same_as<Deint<   Move<int>>, int&&>);
   static_assert(::std::same_as<Deint<Abandon<int>>, int&&>);
   static_assert(::std::same_as<Deint< Disown<int>>, int const&>);
   static_assert(::std::same_as<Deint<  Clone<int>>, int const&>);
   
   static_assert(::std::same_as<Deint<int&&>,        int&&>);
   static_assert(::std::same_as<Deint<int const&&>,  int const&&>);
   static_assert(::std::same_as<Deint<int&>,         int&>);
   static_assert(::std::same_as<Deint<int const&>,   int const&>);
}


///                                                                           
///   Refer intent                                                            
///                                                                           
TEMPLATE_TEST_CASE("Testing refer-makable types", "[ct]",
   AggregateType,
   EmptyType,
   DestructibleType,
   NonIntentConstructible,
   AllIntentConstructible,
   AllIntentConstructibleAndAssignable,
   PartiallyIntentConstructible,
   DescriptorConstructible,
   Complex, ContainsComplex,
   ReferConstructibleButNotAssignable,
   ForcefullyPod,
   int
) {
   using T = TestType;

   static_assert(    CT::Referred<  Refer<T>>);
   static_assert(not CT::Referred<   Move<T>>);
   static_assert(not CT::Referred<   Copy<T>>);
   static_assert(not CT::Referred<Abandon<T>>);
   static_assert(not CT::Referred< Disown<T>>);
   static_assert(not CT::Referred<  Clone<T>>);

   static_assert(    CT::ReferConstructible<T>);
   static_assert(    CT::ReferConstructible<T*>);
   static_assert(    CT::IntentConstructible<Refer, T>);
   static_assert(    CT::IntentConstructible<Refer, T*>);
   static_assert(    CT::IntentConstructibleAlt<Refer<T>>);
   static_assert(    CT::IntentConstructibleAlt<Refer<T*>>);

   auto meta1 = MetaData::Of<T>();
   REQUIRE(meta1);
   REQUIRE(meta1->mReferConstructor);

   auto meta2 = MetaData::Of<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2->mReferConstructor);
}

TEMPLATE_TEST_CASE("Testing non-refer-makable types", "[ct]",
   IncompleteType,
   NonDestructible,
   PrivatelyConstructible,
   CopyConstructibleButNotAssignable,
   MoveConstructibleButNotAssignable,
   AbandonConstructibleButNotAssignable,
   DisownConstructibleButNotAssignable,
   CloneConstructibleButNotAssignable
) {
   using T = TestType;
   static_assert(not CT::ReferConstructible<T>);
   static_assert(    CT::ReferConstructible<T*>);
   static_assert(not CT::IntentConstructible<Refer, T>);
   static_assert(    CT::IntentConstructible<Refer, T*>);
   static_assert(not CT::IntentConstructibleAlt<Refer<T>>);
   static_assert(    CT::IntentConstructibleAlt<Refer<T*>>);

   auto meta = MetaData::Of<Conditional<CT::Complete<T>, T, T*>>();
   REQUIRE(meta);
   REQUIRE_FALSE(meta->mReferConstructor);
}

TEMPLATE_TEST_CASE("Testing refer-assignable types", "[ct]",
   AggregateType,
   ImplicitlyConstructible,
   NonDestructible,
   DestructibleType,
   NonIntentConstructible,
   AllIntentConstructible,
   AllIntentConstructibleAndAssignable,
   PartiallyIntentConstructible,
   DescriptorConstructible,
   ForcefullyPod,
   int
) {
   using T = TestType;
   static_assert(    CT::ReferAssignable<T>);
   static_assert(not CT::ReferAssignable<T const>);
   static_assert(    CT::ReferAssignable<T*>);
   static_assert(    CT::ReferAssignable<T const*>);
   static_assert(    CT::IntentAssignable<Refer, T>);
   static_assert(not CT::IntentAssignable<Refer, T const>);
   static_assert(    CT::IntentAssignable<Refer, T*>);
   static_assert(    CT::IntentAssignable<Refer, T const*>);
   static_assert(    CT::IntentAssignableAlt<Refer<T>>);
   static_assert(not CT::IntentAssignableAlt<Refer<T const>>);
   static_assert(    CT::IntentAssignableAlt<Refer<T*>>);
   static_assert(    CT::IntentAssignableAlt<Refer<T const*>>);

   auto meta1 = MetaData::Of<T>();
   REQUIRE(meta1);
   REQUIRE(meta1->mReferAssigner);

   auto meta2 = MetaData::Of<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2->mReferAssigner);

   auto meta3 = MetaData::Of<const T>();
   REQUIRE(meta3);
   REQUIRE(meta3->mReferAssigner);
}

TEMPLATE_TEST_CASE("Testing non-refer-assignable types", "[ct]",
   IncompleteType,
   Complex,
   ContainsComplex,
   PrivatelyConstructible,
   ReferConstructibleButNotAssignable,
   CopyConstructibleButNotAssignable,
   MoveConstructibleButNotAssignable,
   AbandonConstructibleButNotAssignable,
   DisownConstructibleButNotAssignable,
   CloneConstructibleButNotAssignable
) {
   using T = TestType;
   static_assert(not CT::ReferAssignable<T>);
   static_assert(not CT::ReferAssignable<T const>);
   static_assert(    CT::ReferAssignable<T*>);
   static_assert(    CT::ReferAssignable<T const*>);
   static_assert(not CT::IntentAssignable<Refer, T>);
   static_assert(not CT::IntentAssignable<Refer, T const>);
   static_assert(    CT::IntentAssignable<Refer, T*>);
   static_assert(    CT::IntentAssignable<Refer, T const*>);
   static_assert(not CT::IntentAssignableAlt<Refer<T>>);
   static_assert(not CT::IntentAssignableAlt<Refer<T const>>);
   static_assert(    CT::IntentAssignableAlt<Refer<T*>>);
   static_assert(    CT::IntentAssignableAlt<Refer<T const*>>);

   auto meta = MetaData::Of<Conditional<CT::Complete<T>, T, T*>>();
   REQUIRE(meta);
   REQUIRE_FALSE(meta->mReferAssigner);
}


///                                                                           
///   Move intents                                                            
///                                                                           
TEMPLATE_TEST_CASE("Testing move-makable types", "[ct]",
   AggregateType,
   ImplicitlyConstructible,
   DestructibleType,
   NonIntentConstructible,
   AllIntentConstructible,
   AllIntentConstructibleAndAssignable,
   PartiallyIntentConstructible,
   DescriptorConstructible,
   Complex,
   ContainsComplex,
   MoveConstructibleButNotAssignable,
   ForcefullyPod,
   int
) {
   using T = TestType;

   static_assert(    CT::Moved<   Move<int>>);
   static_assert(not CT::Moved<  Refer<int>>);
   static_assert(not CT::Moved<   Copy<int>>);
   static_assert(not CT::Moved<Abandon<int>>);
   static_assert(not CT::Moved< Disown<int>>);
   static_assert(not CT::Moved<  Clone<int>>);

   static_assert(    CT::MoveConstructible<T>);
   static_assert(    CT::MoveConstructible<T*>);
   static_assert(    CT::IntentConstructible<Move, T>);
   static_assert(    CT::IntentConstructible<Move, T*>);
   static_assert(    CT::IntentConstructibleAlt<Move<T>>);
   static_assert(    CT::IntentConstructibleAlt<Move<T*>>);

   auto meta1 = MetaData::Of<T>();
   REQUIRE(meta1);
   REQUIRE(meta1->mMoveConstructor);

   auto meta2 = MetaData::Of<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2->mMoveConstructor);
}

TEMPLATE_TEST_CASE("Testing non-move-makable types", "[ct]",
   IncompleteType,
   NonDestructible,
   PrivatelyConstructible,
   ReferConstructibleButNotAssignable,
   CopyConstructibleButNotAssignable,
   AbandonConstructibleButNotAssignable,
   DisownConstructibleButNotAssignable,
   CloneConstructibleButNotAssignable
) {
   using T = TestType;
   static_assert(not CT::MoveConstructible<T>);
   static_assert(    CT::MoveConstructible<T*>);
   static_assert(not CT::IntentConstructible<Move, T>);
   static_assert(    CT::IntentConstructible<Move, T*>);
   static_assert(not CT::IntentConstructibleAlt<Move<T>>);
   static_assert(    CT::IntentConstructibleAlt<Move<T*>>);

   auto meta = MetaData::Of<Conditional<CT::Complete<T>, T, T*>>();
   REQUIRE(meta);
   REQUIRE_FALSE(meta->mMoveConstructor);
}

TEMPLATE_TEST_CASE("Testing move-assignable types", "[ct]",
   NonDestructible,
   AggregateType,
   ImplicitlyConstructible,
   DestructibleType,
   NonIntentConstructible,
   AllIntentConstructible,
   AllIntentConstructibleAndAssignable,
   PartiallyIntentConstructible,
   DescriptorConstructible,
   ForcefullyPod,
   int
) {
   using T = TestType;
   static_assert(    CT::MoveAssignable<T>);
   static_assert(not CT::MoveAssignable<T const>);
   static_assert(    CT::MoveAssignable<T*>);
   static_assert(    CT::MoveAssignable<T const*>);
   static_assert(    CT::IntentAssignable<Move, T>);
   static_assert(not CT::IntentAssignable<Move, T const>);
   static_assert(    CT::IntentAssignable<Move, T*>);
   static_assert(    CT::IntentAssignable<Move, T const*>);
   static_assert(    CT::IntentAssignableAlt<Move<T>>);
   static_assert(not CT::IntentAssignableAlt<Move<T const>>);
   static_assert(    CT::IntentAssignableAlt<Move<T*>>);
   static_assert(    CT::IntentAssignableAlt<Move<T const*>>);

   auto meta1 = MetaData::Of<T>();
   REQUIRE(meta1);
   REQUIRE(meta1->mMoveAssigner);

   auto meta2 = MetaData::Of<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2->mMoveAssigner);

   auto meta3 = MetaData::Of<const T>();
   REQUIRE(meta3);
   REQUIRE(meta3->mMoveAssigner);
}

TEMPLATE_TEST_CASE("Testing non-move-assignable types", "[ct]",
   IncompleteType,
   Complex,
   ContainsComplex,
   PrivatelyConstructible,
   ReferConstructibleButNotAssignable,
   MoveConstructibleButNotAssignable,
   CopyConstructibleButNotAssignable,
   AbandonConstructibleButNotAssignable,
   DisownConstructibleButNotAssignable,
   CloneConstructibleButNotAssignable
) {
   using T = TestType;
   static_assert(not CT::MoveAssignable<T>);
   static_assert(not CT::MoveAssignable<T const>);
   static_assert(    CT::MoveAssignable<T*>);
   static_assert(    CT::MoveAssignable<T const*>);
   static_assert(not CT::IntentAssignable<Move, T>);
   static_assert(not CT::IntentAssignable<Move, T const>);
   static_assert(    CT::IntentAssignable<Move, T*>);
   static_assert(    CT::IntentAssignable<Move, T const*>);
   static_assert(not CT::IntentAssignableAlt<Move<T>>);
   static_assert(not CT::IntentAssignableAlt<Move<T const>>);
   static_assert(    CT::IntentAssignableAlt<Move<T*>>);
   static_assert(    CT::IntentAssignableAlt<Move<T const*>>);

   auto meta = MetaData::Of<Conditional<CT::Complete<T>, T, T*>>();
   REQUIRE(meta);
   REQUIRE_FALSE(meta->mMoveAssigner);
}


///                                                                           
///   Copy intents                                                            
///                                                                           
TEMPLATE_TEST_CASE("Testing copy-makable types", "[ct]",
   ImplicitlyConstructible,
   AggregateType,
   AllIntentConstructible,
   AllIntentConstructibleImplicit,
   AllIntentConstructibleAndAssignable,
   PartiallyIntentConstructible,
   CopyConstructibleButNotAssignable,
   ForcefullyPod,
   int
) {
   using T = TestType;

   static_assert(    CT::Copied<   Copy<int>>);
   static_assert(not CT::Copied<   Move<int>>);
   static_assert(not CT::Copied<  Refer<int>>);
   static_assert(not CT::Copied<Abandon<int>>);
   static_assert(not CT::Copied< Disown<int>>);
   static_assert(not CT::Copied<  Clone<int>>);

   static_assert(CT::CopyConstructible<T>);
   static_assert(CT::CopyConstructible<T*>);
   static_assert(CT::IntentConstructible<Copy, T>);
   static_assert(CT::IntentConstructible<Copy, T*>);
   static_assert(CT::IntentConstructibleAlt<Copy<T>>);
   static_assert(CT::IntentConstructibleAlt<Copy<T*>>);

   auto meta1 = MetaData::Of<T>();
   REQUIRE(meta1);
   REQUIRE(meta1->mCopyConstructor);

   auto meta2 = MetaData::Of<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2->mCopyConstructor);
}

TEMPLATE_TEST_CASE("Testing non-copy-makable types", "[ct]",
   IncompleteType,
   NonDestructible,
   DestructibleType,
   Complex,
   ContainsComplex,
   PrivatelyConstructible,
   NonIntentConstructible,
   DescriptorConstructible,
   AggregateTypeComplex,
   ReferConstructibleButNotAssignable,
   MoveConstructibleButNotAssignable,
   AbandonConstructibleButNotAssignable,
   DisownConstructibleButNotAssignable,
   CloneConstructibleButNotAssignable
) {
   using T = TestType;
   static_assert(not CT::CopyConstructible<T>);
   static_assert(    CT::CopyConstructible<T*>);
   static_assert(not CT::IntentConstructible<Copy, T>);
   static_assert(    CT::IntentConstructible<Copy, T*>);
   static_assert(not CT::IntentConstructibleAlt<Copy<T>>);
   static_assert(    CT::IntentConstructibleAlt<Copy<T*>>);

   auto meta = MetaData::Of<Conditional<CT::Complete<T>, T, T*>>();
   REQUIRE(meta);
   REQUIRE_FALSE(meta->mCopyConstructor);
}

TEMPLATE_TEST_CASE("Testing copy-assignable types", "[ct]",
   ImplicitlyConstructible,
   AggregateType,
   AllIntentConstructibleImplicit,
   AllIntentConstructibleAndAssignable,
   ForcefullyPod,
   int
) {
   using T = TestType;
   static_assert(    CT::CopyAssignable<T>);
   static_assert(not CT::CopyAssignable<T const>);
   static_assert(    CT::CopyAssignable<T*>);
   static_assert(    CT::CopyAssignable<T const*>);
   static_assert(    CT::IntentAssignable<Copy, T>);
   static_assert(not CT::IntentAssignable<Copy, T const>);
   static_assert(    CT::IntentAssignable<Copy, T*>);
   static_assert(    CT::IntentAssignable<Copy, T const*>);
   static_assert(    CT::IntentAssignableAlt<Copy<T>>);
   static_assert(not CT::IntentAssignableAlt<Copy<T const>>);
   static_assert(    CT::IntentAssignableAlt<Copy<T*>>);
   static_assert(    CT::IntentAssignableAlt<Copy<T const*>>);

   auto meta1 = MetaData::Of<T>();
   REQUIRE(meta1);
   REQUIRE(meta1->mCopyAssigner);

   auto meta2 = MetaData::Of<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2->mCopyAssigner);

   auto meta3 = MetaData::Of<const T>();
   REQUIRE(meta3);
   REQUIRE(meta3->mCopyAssigner);
}

TEMPLATE_TEST_CASE("Testing non-copy-assignable types", "[ct]",
   IncompleteType,
   NonDestructible,
   DestructibleType,
   Complex,
   ContainsComplex,
   PrivatelyConstructible,
   NonIntentConstructible,
   DescriptorConstructible,
   AllIntentConstructible,
   PartiallyIntentConstructible,
   AggregateTypeComplex,
   ReferConstructibleButNotAssignable,
   MoveConstructibleButNotAssignable,
   CopyConstructibleButNotAssignable,
   AbandonConstructibleButNotAssignable,
   DisownConstructibleButNotAssignable,
   CloneConstructibleButNotAssignable
) {
   using T = TestType;
   static_assert(not CT::CopyAssignable<T>);
   static_assert(not CT::CopyAssignable<T const>);
   static_assert(    CT::CopyAssignable<T*>);
   static_assert(    CT::CopyAssignable<T const*>);
   static_assert(not CT::IntentAssignable<Copy, T>);
   static_assert(not CT::IntentAssignable<Copy, T const>);
   static_assert(    CT::IntentAssignable<Copy, T*>);
   static_assert(    CT::IntentAssignable<Copy, T const*>);
   static_assert(not CT::IntentAssignableAlt<Copy<T>>);
   static_assert(not CT::IntentAssignableAlt<Copy<T const>>);
   static_assert(    CT::IntentAssignableAlt<Copy<T*>>);
   static_assert(    CT::IntentAssignableAlt<Copy<T const*>>);

   auto meta = MetaData::Of<Conditional<CT::Complete<T>, T, T*>>();
   REQUIRE(meta);
   REQUIRE_FALSE(meta->mCopyAssigner);
}


///                                                                           
///   Clone intents                                                           
///                                                                           
TEMPLATE_TEST_CASE("Testing clone-makable types", "[ct]",
   ImplicitlyConstructible,
   AllIntentConstructible,
   AllIntentConstructibleAndAssignable,
   PartiallyIntentConstructible,
   CloneConstructibleButNotAssignable,
   ForcefullyPod,
   AggregateType,
   int
) {
   using T = TestType;
   
   static_assert(    CT::Cloned<  Clone<int>>);
   static_assert(not CT::Cloned<  Refer<int>>);
   static_assert(not CT::Cloned<   Copy<int>>);
   static_assert(not CT::Cloned<   Move<int>>);
   static_assert(not CT::Cloned<Abandon<int>>);
   static_assert(not CT::Cloned< Disown<int>>);

   static_assert(CT::CloneConstructible<T>);
   static_assert(CT::CloneConstructible<T*>);
   static_assert(CT::IntentConstructible<Clone, T>);
   static_assert(CT::IntentConstructible<Clone, T*>);
   static_assert(CT::IntentConstructibleAlt<Clone<T>>);
   static_assert(CT::IntentConstructibleAlt<Clone<T*>>);

   auto meta1 = MetaData::Of<T>();
   REQUIRE(meta1);
   REQUIRE(meta1->mCloneConstructor);

   auto meta2 = MetaData::Of<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2->mCloneConstructor);
}

TEMPLATE_TEST_CASE("Testing non-clone-makable types", "[ct]",
   IncompleteType,
   NonDestructible,
   DestructibleType,
   PrivatelyConstructible,
   NonIntentConstructible,
   DescriptorConstructible,
   Complex,
   ContainsComplex,
   AggregateTypeComplex,
   ReferConstructibleButNotAssignable,
   MoveConstructibleButNotAssignable,
   CopyConstructibleButNotAssignable,
   AbandonConstructibleButNotAssignable,
   DisownConstructibleButNotAssignable
) {
   using T = TestType;
   static_assert(not CT::CloneConstructible<T>);
   static_assert(not CT::CloneConstructible<T*>);
   static_assert(not CT::IntentConstructible<Clone, T>);
   static_assert(not CT::IntentConstructible<Clone, T*>);
   static_assert(not CT::IntentConstructibleAlt<Clone<T>>);
   static_assert(not CT::IntentConstructibleAlt<Clone<T*>>);

   auto meta = MetaData::Of<Conditional<CT::Complete<T>, T, T*>>();
   REQUIRE(meta);
   REQUIRE_FALSE(meta->mCloneConstructor);
}

TEMPLATE_TEST_CASE("Testing clone-assignable types", "[ct]",
   ImplicitlyConstructible,
   AllIntentConstructibleImplicit,
   AllIntentConstructibleAndAssignable,
   ForcefullyPod,
   AggregateType,
   int
) {
   using T = TestType;
   static_assert(    CT::CloneAssignable<T>);
   static_assert(not CT::CloneAssignable<T const>);
   static_assert(    CT::CloneAssignable<T*>);
   static_assert(not CT::CloneAssignable<T const*>);
   static_assert(    CT::IntentAssignable<Clone, T>);
   static_assert(not CT::IntentAssignable<Clone, T const>);
   static_assert(    CT::IntentAssignable<Clone, T*>);
   static_assert(not CT::IntentAssignable<Clone, T const*>);
   static_assert(    CT::IntentAssignableAlt<Clone<T>>);
   static_assert(not CT::IntentAssignableAlt<Clone<T const>>);
   static_assert(    CT::IntentAssignableAlt<Clone<T*>>);
   static_assert(not CT::IntentAssignableAlt<Clone<T const*>>);

   auto meta1 = MetaData::Of<T>();
   REQUIRE(meta1);
   REQUIRE(meta1->mCloneAssigner);

   auto meta2 = MetaData::Of<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2->mCloneAssigner);

   auto meta3 = MetaData::Of<const T>();
   REQUIRE(meta3);
   REQUIRE(meta3->mCloneAssigner);
}

TEMPLATE_TEST_CASE("Testing non-clone-assignable types", "[ct]",
   IncompleteType,
   NonDestructible,
   DestructibleType,
   PrivatelyConstructible,
   NonIntentConstructible,
   Complex,
   ContainsComplex,
   AllIntentConstructible,
   PartiallyIntentConstructible,
   DescriptorConstructible,
   AggregateTypeComplex,
   ReferConstructibleButNotAssignable,
   MoveConstructibleButNotAssignable,
   CopyConstructibleButNotAssignable,
   AbandonConstructibleButNotAssignable,
   DisownConstructibleButNotAssignable,
   CloneConstructibleButNotAssignable
) {
   using T = TestType;
   static_assert(not CT::CloneAssignable<T>);
   static_assert(not CT::CloneAssignable<T const>);
   static_assert(not CT::CloneAssignable<T*>);
   static_assert(not CT::CloneAssignable<T const*>);
   static_assert(not CT::IntentAssignable<Clone, T>);
   static_assert(not CT::IntentAssignable<Clone, T const>);
   static_assert(not CT::IntentAssignable<Clone, T*>);
   static_assert(not CT::IntentAssignable<Clone, T const*>);
   static_assert(not CT::IntentAssignableAlt<Clone<T>>);
   static_assert(not CT::IntentAssignableAlt<Clone<T const>>);
   static_assert(not CT::IntentAssignableAlt<Clone<T*>>);
   static_assert(not CT::IntentAssignableAlt<Clone<T const*>>);

   auto meta = MetaData::Of<Conditional<CT::Complete<T>, T, T*>>();
   REQUIRE(meta);
   REQUIRE_FALSE(meta->mCloneAssigner);
}


///                                                                           
///   Disown intents                                                          
///                                                                           
TEMPLATE_TEST_CASE("Testing disown-makable types", "[ct]",
   ImplicitlyConstructible,
   AllIntentConstructible,
   AllIntentConstructibleAndAssignable,
   PartiallyIntentConstructible,
   DisownConstructibleButNotAssignable,
   ForcefullyPod,
   AggregateType,
   int
) {
   using T = TestType;
   
   static_assert(    CT::Disowned< Disown<int>>);
   static_assert(not CT::Disowned<  Refer<int>>);
   static_assert(not CT::Disowned<   Copy<int>>);
   static_assert(not CT::Disowned<   Move<int>>);
   static_assert(not CT::Disowned<Abandon<int>>);
   static_assert(not CT::Disowned<  Clone<int>>);

   static_assert(CT::DisownConstructible<T>);
   static_assert(CT::DisownConstructible<T*>);
   static_assert(CT::IntentConstructible<Disown, T>);
   static_assert(CT::IntentConstructible<Disown, T*>);
   static_assert(CT::IntentConstructibleAlt<Disown<T>>);
   static_assert(CT::IntentConstructibleAlt<Disown<T*>>);

   auto meta1 = MetaData::Of<T>();
   REQUIRE(meta1);
   REQUIRE(meta1->mDisownConstructor);

   auto meta2 = MetaData::Of<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2->mDisownConstructor);
}

TEMPLATE_TEST_CASE("Testing non-disown-makable types", "[ct]",
   IncompleteType,
   NonDestructible,
   DestructibleType,
   PrivatelyConstructible,
   NonIntentConstructible,
   DescriptorConstructible,
   Complex,
   ContainsComplex,
   AggregateTypeComplex,
   ReferConstructibleButNotAssignable,
   MoveConstructibleButNotAssignable,
   CopyConstructibleButNotAssignable,
   AbandonConstructibleButNotAssignable,
   CloneConstructibleButNotAssignable
) {
   using T = TestType;
   static_assert(not CT::DisownConstructible<T>);
   static_assert(    CT::DisownConstructible<T*>);
   static_assert(not CT::IntentConstructible<Disown, T>);
   static_assert(    CT::IntentConstructible<Disown, T*>);
   static_assert(not CT::IntentConstructibleAlt<Disown<T>>);
   static_assert(    CT::IntentConstructibleAlt<Disown<T*>>);

   auto meta = MetaData::Of<Conditional<CT::Complete<T>, T, T*>>();
   REQUIRE(meta);
   REQUIRE_FALSE(meta->mDisownConstructor);
}

TEMPLATE_TEST_CASE("Testing disown-assignable types", "[ct]",
   ImplicitlyConstructible,
   AllIntentConstructibleImplicit,
   AllIntentConstructibleAndAssignable,
   ForcefullyPod,
   AggregateType,
   int
) {
   using T = TestType;
   static_assert(    CT::DisownAssignable<T>);
   static_assert(not CT::DisownAssignable<T const>);
   static_assert(    CT::DisownAssignable<T*>);
   static_assert(    CT::DisownAssignable<T const*>);
   static_assert(    CT::IntentAssignable<Disown, T>);
   static_assert(not CT::IntentAssignable<Disown, T const>);
   static_assert(    CT::IntentAssignable<Disown, T*>);
   static_assert(    CT::IntentAssignable<Disown, T const*>);
   static_assert(    CT::IntentAssignableAlt<Disown<T>>);
   static_assert(not CT::IntentAssignableAlt<Disown<T const>>);
   static_assert(    CT::IntentAssignableAlt<Disown<T*>>);
   static_assert(    CT::IntentAssignableAlt<Disown<T const*>>);

   auto meta1 = MetaData::Of<T>();
   REQUIRE(meta1);
   REQUIRE(meta1->mDisownAssigner);

   auto meta2 = MetaData::Of<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2->mDisownAssigner);

   auto meta3 = MetaData::Of<const T>();
   REQUIRE(meta3);
   REQUIRE(meta3->mDisownAssigner);
}

TEMPLATE_TEST_CASE("Testing non-disown-assignable types", "[ct]",
   IncompleteType,
   NonDestructible,
   DestructibleType,
   PrivatelyConstructible,
   NonIntentConstructible,
   DescriptorConstructible,
   Complex,
   ContainsComplex,
   AllIntentConstructible,
   PartiallyIntentConstructible,
   ReferConstructibleButNotAssignable,
   MoveConstructibleButNotAssignable,
   CopyConstructibleButNotAssignable,
   AbandonConstructibleButNotAssignable,
   DisownConstructibleButNotAssignable,
   CloneConstructibleButNotAssignable
) {
   using T = TestType;
   static_assert(not CT::DisownAssignable<T>);
   static_assert(not CT::DisownAssignable<T const>);
   static_assert(    CT::DisownAssignable<T*>);
   static_assert(    CT::DisownAssignable<T const*>);
   static_assert(not CT::IntentAssignable<Disown, T>);
   static_assert(not CT::IntentAssignable<Disown, T const>);
   static_assert(    CT::IntentAssignable<Disown, T*>);
   static_assert(    CT::IntentAssignable<Disown, T const*>);
   static_assert(not CT::IntentAssignableAlt<Disown<T>>);
   static_assert(not CT::IntentAssignableAlt<Disown<T const>>);
   static_assert(    CT::IntentAssignableAlt<Disown<T*>>);
   static_assert(    CT::IntentAssignableAlt<Disown<T const*>>);

   auto meta = MetaData::Of<Conditional<CT::Complete<T>, T, T*>>();
   REQUIRE(meta);
   REQUIRE_FALSE(meta->mDisownAssigner);
}


///                                                                           
///   Abandon semantics                                                       
///                                                                           
TEMPLATE_TEST_CASE("Testing abandon-makable types", "[ct]",
   ImplicitlyConstructible,
   DestructibleType,
   NonIntentConstructible,
   AllIntentConstructible,
   AllIntentConstructibleAndAssignable,
   PartiallyIntentConstructible,
   DescriptorConstructible,
   Complex,
   ContainsComplex,
   AbandonConstructibleButNotAssignable,
   ForcefullyPod,
   AggregateType,
   int
) {
   using T = TestType;
   
   static_assert(    CT::Abandoned<Abandon<int>>);
   static_assert(not CT::Abandoned<  Refer<int>>);
   static_assert(not CT::Abandoned<   Copy<int>>);
   static_assert(not CT::Abandoned<   Move<int>>);
   static_assert(not CT::Abandoned< Disown<int>>);
   static_assert(not CT::Abandoned<  Clone<int>>);

   static_assert(CT::AbandonConstructible<T>);
   static_assert(CT::AbandonConstructible<T*>);
   static_assert(CT::IntentConstructible<Abandon, T>);
   static_assert(CT::IntentConstructible<Abandon, T*>);
   static_assert(CT::IntentConstructibleAlt<Abandon<T>>);
   static_assert(CT::IntentConstructibleAlt<Abandon<T*>>);

   auto meta1 = MetaData::Of<T>();
   REQUIRE(meta1);
   REQUIRE(meta1->mAbandonConstructor);

   auto meta2 = MetaData::Of<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2->mAbandonConstructor);
}

TEMPLATE_TEST_CASE("Testing non-abandon-makable types", "[ct]",
   IncompleteType,
   NonDestructible,
   PrivatelyConstructible,
   ReferConstructibleButNotAssignable,
   MoveConstructibleButNotAssignable,
   CopyConstructibleButNotAssignable,
   DisownConstructibleButNotAssignable,
   CloneConstructibleButNotAssignable
) {
   using T = TestType;
   static_assert(not CT::AbandonConstructible<T>);
   static_assert(    CT::AbandonConstructible<T*>);
   static_assert(not CT::IntentConstructible<Abandon, T>);
   static_assert(    CT::IntentConstructible<Abandon, T*>);
   static_assert(not CT::IntentConstructibleAlt<Abandon<T>>);
   static_assert(    CT::IntentConstructibleAlt<Abandon<T*>>);

   auto meta = MetaData::Of<Conditional<CT::Complete<T>, T, T*>>();
   REQUIRE(meta);
   REQUIRE_FALSE(meta->mAbandonConstructor);
}

TEMPLATE_TEST_CASE("Testing abandon-assignable types", "[ct]",
   ImplicitlyConstructible,
   DestructibleType,
   NonIntentConstructible,
   AllIntentConstructible,
   AllIntentConstructibleAndAssignable,
   PartiallyIntentConstructible,
   DescriptorConstructible,
   ForcefullyPod,
   AggregateType,
   int
) {
   using T = TestType;
   static_assert(    CT::AbandonAssignable<T>);
   static_assert(not CT::AbandonAssignable<T const>);
   static_assert(    CT::AbandonAssignable<T*>);
   static_assert(    CT::AbandonAssignable<T const*>);
   static_assert(    CT::IntentAssignable<Abandon, T>);
   static_assert(not CT::IntentAssignable<Abandon, T const>);
   static_assert(    CT::IntentAssignable<Abandon, T*>);
   static_assert(    CT::IntentAssignable<Abandon, T const*>);
   static_assert(    CT::IntentAssignableAlt<Abandon<T>>);
   static_assert(not CT::IntentAssignableAlt<Abandon<T const>>);
   static_assert(    CT::IntentAssignableAlt<Abandon<T*>>);
   static_assert(    CT::IntentAssignableAlt<Abandon<T const*>>);

   auto meta1 = MetaData::Of<T>();
   REQUIRE(meta1);
   REQUIRE(meta1->mAbandonAssigner);

   auto meta2 = MetaData::Of<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2->mAbandonAssigner);

   auto meta3 = MetaData::Of<const T>();
   REQUIRE(meta3);
   REQUIRE(meta3->mAbandonAssigner);
}

TEMPLATE_TEST_CASE("Testing non-abandon-assignable types", "[ct]",
   IncompleteType,
   NonDestructible,
   Complex,
   ContainsComplex,
   PrivatelyConstructible,
   ReferConstructibleButNotAssignable,
   MoveConstructibleButNotAssignable,
   CopyConstructibleButNotAssignable,
   AbandonConstructibleButNotAssignable,
   DisownConstructibleButNotAssignable,
   CloneConstructibleButNotAssignable
) {
   using T = TestType;
   static_assert(not CT::AbandonAssignable<T>);
   static_assert(not CT::AbandonAssignable<T const>);
   static_assert(    CT::AbandonAssignable<T*>);
   static_assert(    CT::AbandonAssignable<T const*>);
   static_assert(not CT::IntentAssignable<Abandon, T>);
   static_assert(not CT::IntentAssignable<Abandon, T const>);
   static_assert(    CT::IntentAssignable<Abandon, T*>);
   static_assert(    CT::IntentAssignable<Abandon, T const*>);
   static_assert(not CT::IntentAssignableAlt<Abandon<T>>);
   static_assert(not CT::IntentAssignableAlt<Abandon<T const>>);
   static_assert(    CT::IntentAssignableAlt<Abandon<T*>>);
   static_assert(    CT::IntentAssignableAlt<Abandon<T const*>>);

   auto meta = MetaData::Of<Conditional<CT::Complete<T>, T, T*>>();
   REQUIRE(meta);
   REQUIRE_FALSE(meta->mAbandonAssigner);
}


///                                                                           
///   Descriptor intents                                                      
///                                                                           
TEMPLATE_TEST_CASE("Testing descriptor-makable types", "[ct]",
   AllIntentConstructible,
   AllIntentConstructibleAndAssignable,
   DescriptorConstructible
) {
   using T = TestType;
   static_assert(    CT::DescriptorConstructible<T>);
   static_assert(not CT::DescriptorConstructible<T*>);
   static_assert(not CT::IntentConstructibleAlt<Describe>);

   auto meta1 = MetaData::Of<T>();
   REQUIRE(meta1);
   REQUIRE(meta1->mDescriptorConstructor);

   auto meta2 = MetaData::Of<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2->mDescriptorConstructor);
}

TEMPLATE_TEST_CASE("Testing non-descriptor-makable types", "[ct]",
   IncompleteType,
   ImplicitlyConstructible,
   NonDestructible,
   DestructibleType,
   PrivatelyConstructible,
   NonIntentConstructible,
   PartiallyIntentConstructible,
   Complex,
   ContainsComplex,
   ForcefullyPod,
   AggregateType,
   int
) {
   using T = TestType;
   static_assert(not CT::DescriptorConstructible<T>);
   static_assert(not CT::DescriptorConstructible<T*>);
   static_assert(not CT::IntentConstructibleAlt<Describe>);

   auto meta = MetaData::Of<Conditional<CT::Complete<T>, T, T*>>();
   REQUIRE(meta);
   REQUIRE_FALSE(meta->mDescriptorConstructor);
}

TEMPLATE_TEST_CASE("Testing DeintCast (non-moving)", "[ct]",
   const int&,
   Copy<int>,
   Refer<int>,
   Disown<int>,
   Clone<int>
) {
   const int* value = new int {656};
   const TestType i {*value};
   static_assert(::std::same_as<decltype(DeintCast(i)), const int&>);
   delete value;
}

TEMPLATE_TEST_CASE("Testing DeintCast (moving)", "[ct]",
   int&&,
   Move<int>,
   Abandon<int>
) {
   const int* value = new int {656};
   const TestType i {*value};
   static_assert(::std::same_as<decltype(DeintCast(i)), const int&>);
   delete value;
}