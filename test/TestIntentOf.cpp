///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "Main.hpp"
#include <Langulus/IntentOf.hpp>
#include <Langulus/MetaOf.hpp>

using namespace Langulus;

namespace
{
   struct EmptyType {};

   struct AggregateType {
      int i;
      float f;
   };

   /// Explicitly deleted destructor                                          
   class NonDestructible {
      ~NonDestructible() = delete;
   };

   struct DestructibleType {
      char* p {};

      ~DestructibleType() { if (p) delete p; }
   };

   /// Has no explicit intent constructors and assigners                      
   /// Has only implicit refer & move constructors and assigners              
   struct NonIntentConstructible {
      NonIntentConstructible(CT::NoIntent auto&&) {}
   };
   
   /// Default-constructible, but only privately                              
   class PrivatelyConstructible {
      PrivatelyConstructible() = default;
      PrivatelyConstructible(const PrivatelyConstructible&) = default;
      PrivatelyConstructible(PrivatelyConstructible&&) = default;
   };

   /// Has explicit copy, move, refer, clone, abandon, disown constructors    
   /// Has implicit refer & move constructors, too                            
   /// Has no explicit intent assigners, only implicit refer & move           
   struct PartiallyIntentConstructible {
      template<template<class> class S, class T>
      PartiallyIntentConstructible(S<T>&&) requires CT::Intent<S<T>> {}
   };

   /// Has all intent constructors + implicit refer & move ones               
   /// Has no explicit intent assigners, only implicit refer & move ones      
   /// Making constructor explicit makes sure, that no implicit intent assign 
   /// happens                                                                
   struct AllIntentConstructible {
      explicit AllIntentConstructible(CT::Intent auto&&) {}
   };
   
   /// Has all intent constructors + implicit refer & move ones               
   /// Has no explicit intent assigners, only implicit refer & move ones      
   /// Making constructor implicit also allows for intent assignments         
   struct AllIntentConstructibleImplicit {
      AllIntentConstructibleImplicit(CT::Intent auto&&) {}
   };

   /// Has all intnet constructors and assigners + implicit refer & move ones 
   struct AllIntentConstructibleAndAssignable {
      AllIntentConstructibleAndAssignable(CT::Intent auto&&) {}
      AllIntentConstructibleAndAssignable& operator = (CT::Intent auto&&) { return *this; }
   };

   template<class T>
   struct SheddableType {
      using CTTI_Sheddable = Yes;
      using CTTI_Typed = T;

      T instance;

      SheddableType(T t) : instance {FWD(t)} {}
   };
   
   /// Doesn't have implicit copy/move, so it is abandon-makable by explicit  
   /// move but not abandon-assignable                                        
   struct alignas(128) Complex {
      int  member;
      bool anotherMember {};
      int  anotherMemberArray [12] {};
      int* sparseMember {};

      Complex(const Complex& s) : member(s.member) {}
      Complex(Complex&& s) : member(s.member) {}
      Complex(int stuff) : member(stuff) {}

      ~Complex() { if (sparseMember) delete sparseMember; }
   };

   class ContainsComplex {
      Complex mData;
   };

   /// A complex aggregate type                                               
   struct AggregateTypeComplex {
      int m1, m2, m3, m4;
      bool m5;
      Complex mData;
   };
   
   struct ReferConstructibleButNotAssignable {
      int m;
      ReferConstructibleButNotAssignable(Refer<ReferConstructibleButNotAssignable>&& a) : m {a->m} {}
      ReferConstructibleButNotAssignable& operator = (const ReferConstructibleButNotAssignable&) = delete;
      ReferConstructibleButNotAssignable& operator = (ReferConstructibleButNotAssignable&&) = delete;
   };

   struct CopyConstructibleButNotAssignable {
      int m;
      CopyConstructibleButNotAssignable(Copy<CopyConstructibleButNotAssignable>&& a) : m {a->m} {}
      CopyConstructibleButNotAssignable& operator = (const CopyConstructibleButNotAssignable&) = delete;
      CopyConstructibleButNotAssignable& operator = (CopyConstructibleButNotAssignable&&) = delete;
   };

   struct MoveConstructibleButNotAssignable {
      int m;
      MoveConstructibleButNotAssignable(Move<MoveConstructibleButNotAssignable>&& a) : m {a->m} {}
      MoveConstructibleButNotAssignable& operator = (const MoveConstructibleButNotAssignable&) = delete;
      MoveConstructibleButNotAssignable& operator = (MoveConstructibleButNotAssignable&&) = delete;
   };

   struct AbandonConstructibleButNotAssignable {
      int m;
      AbandonConstructibleButNotAssignable(Abandon<AbandonConstructibleButNotAssignable>&& a) : m {a->m} {}
      AbandonConstructibleButNotAssignable& operator = (const AbandonConstructibleButNotAssignable&) = delete;
      AbandonConstructibleButNotAssignable& operator = (AbandonConstructibleButNotAssignable&&) = delete;
   };

   struct DisownConstructibleButNotAssignable {
      int m;
      DisownConstructibleButNotAssignable(Disown<DisownConstructibleButNotAssignable>&& a) : m {a->m} {}
      DisownConstructibleButNotAssignable& operator = (const DisownConstructibleButNotAssignable&) = delete;
      DisownConstructibleButNotAssignable& operator = (DisownConstructibleButNotAssignable&&) = delete;
   };

   struct CloneConstructibleButNotAssignable {
      int m;
      CloneConstructibleButNotAssignable(Clone<CloneConstructibleButNotAssignable>&& a) : m {a->m} {}
      CloneConstructibleButNotAssignable& operator = (const CloneConstructibleButNotAssignable&) = delete;
      CloneConstructibleButNotAssignable& operator = (CloneConstructibleButNotAssignable&&) = delete;
   };

   struct ForcefullyPod {
      using CTTI_POD = Yes;
      Complex mData;
   };

   enum TypedEnum : int64_t {one1, two2};
   enum class TypedEnumClass : int64_t {one1, two2};
   struct IncompleteType;
}

///                                                                           
/// CT::Intent / CT::NoIntent                                                 
///                                                                           
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
/// CT::HasReferConstructor                                                   
///                                                                           
TEMPLATE_TEST_CASE("Testing CT::HasReferConstructor", "[ct]",
   AggregateType,
   EmptyType,
   DestructibleType,
   NonIntentConstructible,
   AllIntentConstructible,
   AllIntentConstructibleAndAssignable,
   PartiallyIntentConstructible,
   Complex, ContainsComplex,
   ReferConstructibleButNotAssignable,
   ForcefullyPod,
   int
) {
   static_assert(CT::HasIntentConstructor<Refer, TestType>);
   static_assert(CT::HasIntentConstructorAlt<Refer<TestType>>);
   static_assert(CT::HasReferConstructor<TestType>);
}

TEMPLATE_TEST_CASE("Testing not CT::HasReferConstructor", "[ct]",
   //IncompleteType, // should not compile at all
   NonDestructible,  // modifying destructor disables implicit copy-construction
   PrivatelyConstructible,
   CopyConstructibleButNotAssignable,
   MoveConstructibleButNotAssignable,
   AbandonConstructibleButNotAssignable,
   DisownConstructibleButNotAssignable,
   CloneConstructibleButNotAssignable
) {
   static_assert(not CT::HasIntentConstructor<Refer, TestType>);
   static_assert(not CT::HasIntentConstructorAlt<Refer<TestType>>);
   static_assert(not CT::HasReferConstructor<TestType>);
}

static_assert(    CT::HasReferConstructor<AggregateType, EmptyType, AllIntentConstructible>);
static_assert(not CT::HasReferConstructor<AggregateType, EmptyType, PrivatelyConstructible>);


///                                                                           
/// CT::HasCopyConstructor                                                    
///                                                                           
TEMPLATE_TEST_CASE("Testing CT::HasCopyConstructor", "[ct]",
   AggregateType,
   EmptyType,
   DestructibleType,
   NonIntentConstructible,
   AllIntentConstructible,
   AllIntentConstructibleAndAssignable,
   CopyConstructibleButNotAssignable,
   PartiallyIntentConstructible,
   ForcefullyPod,
   int
) {
   static_assert(CT::HasIntentConstructor<Copy, TestType>);
   static_assert(CT::HasIntentConstructorAlt<Copy<TestType>>);
   static_assert(CT::HasCopyConstructor<TestType>);
}

TEMPLATE_TEST_CASE("Testing not CT::HasCopyConstructor", "[ct]",
   //IncompleteType, // should not compile at all
   NonDestructible,  // modifying destructor disables implicit copy-construction
   PrivatelyConstructible,
   ReferConstructibleButNotAssignable,
   MoveConstructibleButNotAssignable,
   AbandonConstructibleButNotAssignable,
   DisownConstructibleButNotAssignable,
   CloneConstructibleButNotAssignable,
   Complex, ContainsComplex
) {
   static_assert(not CT::HasIntentConstructor<Copy, TestType>);
   static_assert(not CT::HasIntentConstructorAlt<Copy<TestType>>);
   static_assert(not CT::HasCopyConstructor<TestType>);
}

static_assert(    CT::HasCopyConstructor<AggregateType, EmptyType, AllIntentConstructible>);
static_assert(not CT::HasCopyConstructor<AggregateType, EmptyType, PrivatelyConstructible>);


///                                                                           
///   Refer intent                                                            
///                                                                           
TEMPLATE_TEST_CASE("Testing refer-constructible types", "[ct]",
   AggregateType,
   EmptyType,
   DestructibleType,
   NonIntentConstructible,
   AllIntentConstructible,
   AllIntentConstructibleAndAssignable,
   PartiallyIntentConstructible,
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



   /*template<class...T>
   concept HasDisownConstructor

   template<class...T>
   concept HasCloneConstructor

   template<class...T>
   concept HasAbandonConstructor

   template<class...T>
   concept HasCopyConstructor

   template<class...T>
   concept HasMoveConstructor

   template<template<class> class S, class...T>
   concept HasIntentAssign

   template<class...S>
   concept HasIntentAssignAlt

   template<class...T>
   concept HasDisownAssign

   template<class...T>
   concept HasCloneAssign

   template<class...T>
   concept HasAbandonAssign

   template<class...T>
   concept HasReferAssign

   template<class...T>
   concept HasCopyAssign

   template<class...T>
   concept HasMoveAssign*/

   static_assert(    CT::ReferConstructible<T>);
   static_assert(    CT::ReferConstructible<T*>);
   static_assert(    CT::IntentConstructible<Refer, T>);
   static_assert(    CT::IntentConstructible<Refer, T*>);
   static_assert(    CT::IntentConstructibleAlt<Refer<T>>);
   static_assert(    CT::IntentConstructibleAlt<Refer<T*>>);

   auto meta1 = MetaDataOf<T>();
   REQUIRE(meta1);
   REQUIRE(meta1.HasReferConstructor());

   auto meta2 = MetaDataOf<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2.HasReferConstructor());
}

TEMPLATE_TEST_CASE("Testing non-refer-constructible types", "[ct]",
   //IncompleteType, // should not compile at all
   NonDestructible, // modifying destructor disables implicit copy-construction
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

   auto meta = MetaDataOf<Tif<CT::Complete<T>, T, T*>>();
   REQUIRE(meta);
   REQUIRE_FALSE(meta.HasReferConstructor());
}

TEMPLATE_TEST_CASE("Testing refer-assignable types", "[ct]",
   AggregateType,
   EmptyType,
   NonDestructible,
   DestructibleType,
   NonIntentConstructible,
   AllIntentConstructible,
   AllIntentConstructibleAndAssignable,
   PartiallyIntentConstructible,
   int
) {
   using T = TestType;
   static_assert(    CT::ReferAssignable<T>);
   //static_assert(not CT::ReferAssignable<T const>); // shouldn't compile
   static_assert(    CT::ReferAssignable<T*>);
   static_assert(    CT::ReferAssignable<T const*>);
   static_assert(    CT::IntentAssignable<Refer, T>);
   //static_assert(not CT::IntentAssignable<Refer, T const>); // shouldn't compile
   static_assert(    CT::IntentAssignable<Refer, T*>);
   static_assert(    CT::IntentAssignable<Refer, T const*>);
   static_assert(    CT::IntentAssignableAlt<Refer<T>>);
   //static_assert(not CT::IntentAssignableAlt<Refer<T const>>); // shouldn't compile
   static_assert(    CT::IntentAssignableAlt<Refer<T*>>);
   static_assert(    CT::IntentAssignableAlt<Refer<T const*>>);

   auto meta1 = MetaDataOf<T>();
   REQUIRE(meta1);
   REQUIRE(meta1.HasReferAssigner());

   auto meta2 = MetaDataOf<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2.HasReferAssigner());

   auto meta3 = MetaDataOf<const T>();
   REQUIRE(meta3);
   REQUIRE(meta3.HasReferAssigner());
}

TEMPLATE_TEST_CASE("Testing non-refer-assignable types", "[ct]",
   //IncompleteType, // should not compile at all
   Complex,
   ContainsComplex,
   PrivatelyConstructible,
   ReferConstructibleButNotAssignable,
   CopyConstructibleButNotAssignable,
   MoveConstructibleButNotAssignable,
   AbandonConstructibleButNotAssignable,
   DisownConstructibleButNotAssignable,
   CloneConstructibleButNotAssignable,
   ForcefullyPod // implicit assignment is disabled due to custom constructors
) {
   using T = TestType;
   static_assert(not CT::ReferAssignable<T>);
   //static_assert(not CT::ReferAssignable<T const>); // shouldn't compile
   static_assert(    CT::ReferAssignable<T*>);
   static_assert(    CT::ReferAssignable<T const*>);
   static_assert(not CT::IntentAssignable<Refer, T>);
   //static_assert(not CT::IntentAssignable<Refer, T const>); // shouldn't compile
   static_assert(    CT::IntentAssignable<Refer, T*>);
   static_assert(    CT::IntentAssignable<Refer, T const*>);
   static_assert(not CT::IntentAssignableAlt<Refer<T>>);
   //static_assert(not CT::IntentAssignableAlt<Refer<T const>>); // shouldn't compile
   static_assert(    CT::IntentAssignableAlt<Refer<T*>>);
   static_assert(    CT::IntentAssignableAlt<Refer<T const*>>);

   auto meta = MetaDataOf<Tif<CT::Complete<T>, T, T*>>();
   REQUIRE(meta);
   REQUIRE_FALSE(meta.HasReferAssigner());
}


///                                                                           
///   Move intents                                                            
///                                                                           
TEMPLATE_TEST_CASE("Testing move-constructible types", "[ct]",
   AggregateType,
   EmptyType,
   DestructibleType,
   NonIntentConstructible,
   AllIntentConstructible,
   AllIntentConstructibleAndAssignable,
   PartiallyIntentConstructible,
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

   auto meta1 = MetaDataOf<T>();
   REQUIRE(meta1);
   REQUIRE(meta1.HasMoveConstructor());

   auto meta2 = MetaDataOf<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2.HasMoveConstructor());
}

TEMPLATE_TEST_CASE("Testing non-move-constructible types", "[ct]",
   //IncompleteType, // should not compile at all
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

   auto meta = MetaDataOf<Tif<CT::Complete<T>, T, T*>>();
   REQUIRE(meta);
   REQUIRE_FALSE(meta.HasMoveConstructor());
}

TEMPLATE_TEST_CASE("Testing move-assignable types", "[ct]",
   NonDestructible,
   AggregateType,
   EmptyType,
   DestructibleType,
   NonIntentConstructible,
   AllIntentConstructible,
   AllIntentConstructibleAndAssignable,
   PartiallyIntentConstructible,
   int
) {
   using T = TestType;
   static_assert(    CT::MoveAssignable<T>);
   //static_assert(not CT::MoveAssignable<T const>); // shouldn't compile
   static_assert(    CT::MoveAssignable<T*>);
   static_assert(    CT::MoveAssignable<T const*>);
   static_assert(    CT::IntentAssignable<Move, T>);
   //static_assert(not CT::IntentAssignable<Move, T const>); // shouldn't compile
   static_assert(    CT::IntentAssignable<Move, T*>);
   static_assert(    CT::IntentAssignable<Move, T const*>);
   static_assert(    CT::IntentAssignableAlt<Move<T>>);
   //static_assert(not CT::IntentAssignableAlt<Move<T const>>); // shouldn't compile
   static_assert(    CT::IntentAssignableAlt<Move<T*>>);
   static_assert(    CT::IntentAssignableAlt<Move<T const*>>);

   auto meta1 = MetaDataOf<T>();
   REQUIRE(meta1);
   REQUIRE(meta1.HasMoveAssigner());

   auto meta2 = MetaDataOf<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2.HasMoveAssigner());

   auto meta3 = MetaDataOf<const T>();
   REQUIRE(meta3);
   REQUIRE(meta3.HasMoveAssigner());
}

TEMPLATE_TEST_CASE("Testing non-move-assignable types", "[ct]",
   //IncompleteType, // should not compile at all
   Complex,
   ContainsComplex,
   PrivatelyConstructible,
   ReferConstructibleButNotAssignable,
   MoveConstructibleButNotAssignable,
   CopyConstructibleButNotAssignable,
   AbandonConstructibleButNotAssignable,
   DisownConstructibleButNotAssignable,
   CloneConstructibleButNotAssignable,
   ForcefullyPod // implicit assignment is disabled due to custom constructors
) {
   using T = TestType;
   static_assert(not CT::MoveAssignable<T>);
   //static_assert(not CT::MoveAssignable<T const>); // shouldn't compile
   static_assert(    CT::MoveAssignable<T*>);
   static_assert(    CT::MoveAssignable<T const*>);
   static_assert(not CT::IntentAssignable<Move, T>);
   //static_assert(not CT::IntentAssignable<Move, T const>); // shouldn't compile
   static_assert(    CT::IntentAssignable<Move, T*>);
   static_assert(    CT::IntentAssignable<Move, T const*>);
   static_assert(not CT::IntentAssignableAlt<Move<T>>);
   //static_assert(not CT::IntentAssignableAlt<Move<T const>>); // shouldn't compile
   static_assert(    CT::IntentAssignableAlt<Move<T*>>);
   static_assert(    CT::IntentAssignableAlt<Move<T const*>>);

   auto meta = MetaDataOf<Tif<CT::Complete<T>, T, T*>>();
   REQUIRE(meta);
   REQUIRE_FALSE(meta.HasMoveAssigner());
}


///                                                                           
///   Copy intents                                                            
///                                                                           
TEMPLATE_TEST_CASE("Testing copy-constructible types", "[ct]",
   EmptyType,
   AggregateType,
   DestructibleType,
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

   auto meta1 = MetaDataOf<T>();
   REQUIRE(meta1);
   REQUIRE(meta1.HasCopyConstructor());

   auto meta2 = MetaDataOf<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2.HasCopyConstructor());
}

TEMPLATE_TEST_CASE("Testing non-copy-constructible types", "[ct]",
   //IncompleteType, // should not compile at all
   NonDestructible,
   Complex,
   ContainsComplex,
   PrivatelyConstructible,
   NonIntentConstructible,
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

   auto meta = MetaDataOf<Tif<CT::Complete<T>, T, T*>>();
   REQUIRE(meta);
   REQUIRE_FALSE(meta.HasCopyConstructor());
}

TEMPLATE_TEST_CASE("Testing copy-assignable types", "[ct]",
   EmptyType,
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

   auto meta1 = MetaDataOf<T>();
   REQUIRE(meta1);
   REQUIRE(meta1.HasCopyAssigner());

   auto meta2 = MetaDataOf<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2.HasCopyAssigner());

   auto meta3 = MetaDataOf<const T>();
   REQUIRE(meta3);
   REQUIRE(meta3.HasCopyAssigner());
}

TEMPLATE_TEST_CASE("Testing non-copy-assignable types", "[ct]",
   IncompleteType,
   NonDestructible,
   DestructibleType,
   Complex,
   ContainsComplex,
   PrivatelyConstructible,
   NonIntentConstructible,
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

   auto meta = MetaDataOf<Tif<CT::Complete<T>, T, T*>>();
   REQUIRE(meta);
   REQUIRE_FALSE(meta.HasCopyAssigner());
}


///                                                                           
///   Clone intents                                                           
///                                                                           
TEMPLATE_TEST_CASE("Testing clone-constructible types", "[ct]",
   EmptyType,
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

   auto meta1 = MetaDataOf<T>();
   REQUIRE(meta1);
   REQUIRE(meta1.HasCloneConstructor());

   auto meta2 = MetaDataOf<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2.HasCloneConstructor());
}

TEMPLATE_TEST_CASE("Testing non-clone-constructible types", "[ct]",
   IncompleteType,
   NonDestructible,
   DestructibleType,
   PrivatelyConstructible,
   NonIntentConstructible,
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

   auto meta = MetaDataOf<Tif<CT::Complete<T>, T, T*>>();
   REQUIRE(meta);
   REQUIRE_FALSE(meta.HasCloneConstructor());
}

TEMPLATE_TEST_CASE("Testing clone-assignable types", "[ct]",
   EmptyType,
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

   auto meta1 = MetaDataOf<T>();
   REQUIRE(meta1);
   REQUIRE(meta1.HasCloneAssigner());

   auto meta2 = MetaDataOf<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2.HasCloneAssigner());

   auto meta3 = MetaDataOf<const T>();
   REQUIRE(meta3);
   REQUIRE(meta3.HasCloneAssigner());
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

   auto meta = MetaDataOf<Tif<CT::Complete<T>, T, T*>>();
   REQUIRE(meta);
   REQUIRE_FALSE(meta.HasCloneAssigner());
}


///                                                                           
///   Disown intents                                                          
///                                                                           
TEMPLATE_TEST_CASE("Testing disown-constructible types", "[ct]",
   EmptyType,
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

   auto meta1 = MetaDataOf<T>();
   REQUIRE(meta1);
   REQUIRE(meta1.HasDisownConstructor());

   auto meta2 = MetaDataOf<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2.HasDisownConstructor());
}

TEMPLATE_TEST_CASE("Testing non-disown-constructible types", "[ct]",
   IncompleteType,
   NonDestructible,
   DestructibleType,
   PrivatelyConstructible,
   NonIntentConstructible,
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

   auto meta = MetaDataOf<Tif<CT::Complete<T>, T, T*>>();
   REQUIRE(meta);
   REQUIRE_FALSE(meta.HasDisownConstructor());
}

TEMPLATE_TEST_CASE("Testing disown-assignable types", "[ct]",
   EmptyType,
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

   auto meta1 = MetaDataOf<T>();
   REQUIRE(meta1);
   REQUIRE(meta1.HasDisownAssigner());

   auto meta2 = MetaDataOf<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2.HasDisownAssigner());

   auto meta3 = MetaDataOf<const T>();
   REQUIRE(meta3);
   REQUIRE(meta3.HasDisownAssigner());
}

TEMPLATE_TEST_CASE("Testing non-disown-assignable types", "[ct]",
   IncompleteType,
   NonDestructible,
   DestructibleType,
   PrivatelyConstructible,
   NonIntentConstructible,
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

   auto meta = MetaDataOf<Tif<CT::Complete<T>, T, T*>>();
   REQUIRE(meta);
   REQUIRE_FALSE(meta.HasDisownAssigner());
}


///                                                                           
///   Abandon semantics                                                       
///                                                                           
TEMPLATE_TEST_CASE("Testing abandon-constructible types", "[ct]",
   EmptyType,
   DestructibleType,
   NonIntentConstructible,
   AllIntentConstructible,
   AllIntentConstructibleAndAssignable,
   PartiallyIntentConstructible,
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

   auto meta1 = MetaDataOf<T>();
   REQUIRE(meta1);
   REQUIRE(meta1.HasAbandonConstructor());

   auto meta2 = MetaDataOf<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2.HasAbandonConstructor());
}

TEMPLATE_TEST_CASE("Testing non-abandon-constructible types", "[ct]",
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

   auto meta = MetaDataOf<Tif<CT::Complete<T>, T, T*>>();
   REQUIRE(meta);
   REQUIRE_FALSE(meta.HasAbandonConstructor());
}

TEMPLATE_TEST_CASE("Testing abandon-assignable types", "[ct]",
   EmptyType,
   DestructibleType,
   NonIntentConstructible,
   AllIntentConstructible,
   AllIntentConstructibleAndAssignable,
   PartiallyIntentConstructible,
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

   auto meta1 = MetaDataOf<T>();
   REQUIRE(meta1);
   REQUIRE(meta1.HasAbandonAssigner());

   auto meta2 = MetaDataOf<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2.HasAbandonAssigner());

   auto meta3 = MetaDataOf<const T>();
   REQUIRE(meta3);
   REQUIRE(meta3.HasAbandonAssigner());
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

   auto meta = MetaDataOf<Tif<CT::Complete<T>, T, T*>>();
   REQUIRE(meta);
   REQUIRE_FALSE(meta.HasAbandonAssigner());
}


///                                                                           
///   Descriptor intents                                                      
///                                                                           
/*TEMPLATE_TEST_CASE("Testing descriptor-makable types", "[ct]",
   AllIntentConstructible,
   AllIntentConstructibleAndAssignable
) {
   using T = TestType;
   static_assert(    CT::DescriptorConstructible<T>);
   static_assert(not CT::DescriptorConstructible<T*>);
   static_assert(not CT::IntentConstructibleAlt<Describe>);

   auto meta1 = MetaDataOf<T>();
   REQUIRE(meta1);
   REQUIRE(meta1->mDescriptorConstructor);

   auto meta2 = MetaDataOf<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2->mDescriptorConstructor);
}

TEMPLATE_TEST_CASE("Testing non-descriptor-makable types", "[ct]",
   IncompleteType,
   EmptyType,
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

   auto meta = MetaDataOf<Tif<CT::Complete<T>, T, T*>>();
   REQUIRE(meta);
   REQUIRE_FALSE(meta->mDescriptorConstructor);
}*/

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