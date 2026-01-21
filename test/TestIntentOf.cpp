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
   static_assert(CT::POD<EmptyType>);
   static_assert(::std::is_copy_constructible_v<EmptyType>);
   static_assert(::std::is_move_constructible_v<EmptyType>);
   static_assert(::std::is_copy_assignable_v<EmptyType>);
   static_assert(::std::is_move_assignable_v<EmptyType>);

   struct AggregateType {
      int i;
      float f;
   };
   static_assert(CT::POD<AggregateType>);
   static_assert(::std::is_copy_constructible_v<AggregateType>);
   static_assert(::std::is_move_constructible_v<AggregateType>);
   static_assert(::std::is_copy_assignable_v<AggregateType>);
   static_assert(::std::is_move_assignable_v<AggregateType>);

   /// Explicitly deleted destructor                                          
   /// @attention this hits a nasty compiler bug on MSVC v143 when intents    
   ///   are implicitly cast to built-in move/copy semantics                  
   ///   They are disabled because of this, as well as other compiler bugs    
   ///   https://stackoverflow.com/questions/79665049                         
   struct NonDestructible {
      ~NonDestructible() = delete;
   };
   static_assert(::std::is_standard_layout_v<NonDestructible>);
   static_assert(not CT::POD<NonDestructible>);
   static_assert(not ::std::is_copy_constructible_v<NonDestructible>);
   static_assert(not ::std::is_move_constructible_v<NonDestructible>);
   static_assert(    ::std::is_copy_assignable_v<NonDestructible>);
   static_assert(    ::std::is_move_assignable_v<NonDestructible>);

   struct DestructibleType {
      char* p {};

      ~DestructibleType() { if (p) delete p; }
   };
   static_assert(not CT::POD<DestructibleType>);
   static_assert(::std::is_copy_constructible_v<DestructibleType>);
   static_assert(::std::is_move_constructible_v<DestructibleType>);
   static_assert(::std::is_copy_assignable_v<DestructibleType>);
   static_assert(::std::is_move_assignable_v<DestructibleType>);

   /// Has no explicit intent constructors and assigners                      
   struct NonIntentConstructible {
      NonIntentConstructible(CT::NoIntent auto&&) {}
   };
   static_assert(not CT::POD<NonIntentConstructible>);
   static_assert(::std::is_copy_constructible_v<NonIntentConstructible>);
   static_assert(::std::is_move_constructible_v<NonIntentConstructible>);
   static_assert(::std::is_copy_assignable_v<NonIntentConstructible>);
   static_assert(::std::is_move_assignable_v<NonIntentConstructible>);

   /// Default-constructible, but only privately                              
   class PrivatelyConstructible {
      PrivatelyConstructible() = default;
      PrivatelyConstructible(PrivatelyConstructible const&) = default;
      PrivatelyConstructible(PrivatelyConstructible&&) = default;
   };
   static_assert(CT::POD<PrivatelyConstructible>);
   static_assert(not ::std::is_copy_constructible_v<PrivatelyConstructible>);
   static_assert(not ::std::is_move_constructible_v<PrivatelyConstructible>);
   static_assert(not ::std::is_copy_assignable_v<PrivatelyConstructible>);
   static_assert(not ::std::is_move_assignable_v<PrivatelyConstructible>);

   /// Has explicit copy, move, refer, clone, abandon, disown constructors    
   /// Because they're explicit, there are no implicit intent-assigners       
   struct PartiallyIntentConstructible {
      template<template<class> class S, class T>
      explicit PartiallyIntentConstructible(S<T>&&) requires CT::Intent<S<T>> {}
   };
   static_assert(not CT::POD<PartiallyIntentConstructible>);
   static_assert(::std::is_copy_constructible_v<PartiallyIntentConstructible>);
   static_assert(::std::is_move_constructible_v<PartiallyIntentConstructible>);
   static_assert(::std::is_copy_assignable_v<PartiallyIntentConstructible>);
   static_assert(::std::is_move_assignable_v<PartiallyIntentConstructible>);

   /// Has implicit copy, move, refer, clone, abandon, disown constructors    
   /// Because they're implicit, the type should also have all intent-assigs  
   ///   @attention this hits a lot of compiler bugs on different compilers:  
   ///   - it causes ambiguity on Clang 19.1 for refer intents, because       
   ///     the compiler can't decide whether to implicit-cast to && or        
   ///     const&
   ///   - it causes ambiguity on GCC 14.2 for move/abandon intents, because  
   ///     the compiler can't decide how to implicit-cast to && or            
   ///     const&
   ///   @note implicit coversion of intents has been disabled to cope        
   struct PartiallyIntentConstructibleButImplicitly {
      template<template<class> class S, class T>
      PartiallyIntentConstructibleButImplicitly(S<T>&&) requires CT::Intent<S<T>> {}
   };
   static_assert(not CT::POD<PartiallyIntentConstructibleButImplicitly>);
   static_assert(::std::is_copy_constructible_v<PartiallyIntentConstructibleButImplicitly>);
   static_assert(::std::is_move_constructible_v<PartiallyIntentConstructibleButImplicitly>);
   static_assert(::std::is_copy_assignable_v<PartiallyIntentConstructibleButImplicitly>);
   static_assert(::std::is_move_assignable_v<PartiallyIntentConstructibleButImplicitly>);

   /// Has all intent constructors                                            
   /// Making constructor explicit makes sure, that no implicit intent assign 
   /// happens                                                                
   struct AllIntentConstructible {
      explicit AllIntentConstructible(CT::Intent auto&&) {}
   };
   
   /// Has all intent constructors                                            
   /// Making constructor implicit also allows for implicit intent assignments
   struct AllIntentConstructibleImplicit {
      AllIntentConstructibleImplicit(CT::Intent auto&&) {}
   };

   /// Has all intent constructors and assigners                              
   struct AllIntentConstructibleAndAssignable {
      AllIntentConstructibleAndAssignable(CT::Intent auto&&) {}
      AllIntentConstructibleAndAssignable& operator = (CT::Intent auto&&) { return *this; }
   };

   template<class T>
   struct SheddableType {
      using CTTI_Sheddable = T;
      using CTTI_Typed = T;

      T instance;

      SheddableType(T t) : instance {FWD(t)} {}
   };
   
   /// Implicit assignment is disabled due to custom copy/move constructors   
   struct alignas(128) Complex {
      int  member;
      bool anotherMember {};
      int  anotherMemberArray [12] {};
      int* sparseMember {};

      Complex(const Complex& s) : member(s.member) {}
      Complex(Complex&& s) : member(s.member) {}
      Complex(int stuff) : member(stuff) {}

      ~Complex() {
         if (sparseMember) delete sparseMember;
      }
   };
   static_assert(not CT::POD<Complex>);
   static_assert(    ::std::is_copy_constructible_v<Complex>);
   static_assert(    ::std::is_move_constructible_v<Complex>);
   static_assert(not ::std::is_copy_assignable_v<Complex>);
   static_assert(not ::std::is_move_assignable_v<Complex>);

   class ContainsComplex {
      Complex mData;
   };

   /// A complex aggregate type                                               
   struct AggregateTypeComplex {
      int m1, m2, m3, m4;
      bool m5;
      Complex mData;
   };
   
   /// Constructible but not assignable                                       
   struct ReferConstructibleButNotAssignable {
      int m;
      explicit ReferConstructibleButNotAssignable(const ReferConstructibleButNotAssignable& a) : m {a.m} {}
      ReferConstructibleButNotAssignable(Refer<ReferConstructibleButNotAssignable>&& a) : m {a->m} {}
      ReferConstructibleButNotAssignable& operator = (ReferConstructibleButNotAssignable const&) = delete;
      ReferConstructibleButNotAssignable& operator = (ReferConstructibleButNotAssignable&&) = delete;
   };
   static_assert(not ::std::is_trivially_copy_constructible_v<ReferConstructibleButNotAssignable>);
   static_assert(not ::std::is_trivially_move_constructible_v<ReferConstructibleButNotAssignable>);
   static_assert(not ::std::is_trivially_copy_assignable_v<ReferConstructibleButNotAssignable>);
   static_assert(not ::std::is_trivially_move_assignable_v<ReferConstructibleButNotAssignable>);

   struct CopyConstructibleButNotAssignable {
      int m;
      CopyConstructibleButNotAssignable(Copy<CopyConstructibleButNotAssignable>&& a) : m {a->m} {}
      CopyConstructibleButNotAssignable& operator = (CopyConstructibleButNotAssignable const&) = delete;
      CopyConstructibleButNotAssignable& operator = (CopyConstructibleButNotAssignable&&) = delete;
   };
   static_assert(not ::std::is_trivially_copy_constructible_v<CopyConstructibleButNotAssignable>);
   static_assert(not ::std::is_trivially_move_constructible_v<CopyConstructibleButNotAssignable>);
   static_assert(not ::std::is_trivially_copy_assignable_v<CopyConstructibleButNotAssignable>);
   static_assert(not ::std::is_trivially_move_assignable_v<CopyConstructibleButNotAssignable>);

   struct MoveConstructibleButNotAssignable {
      int m;
      explicit MoveConstructibleButNotAssignable(MoveConstructibleButNotAssignable&& a) : m {a.m} {}
      MoveConstructibleButNotAssignable(Move<MoveConstructibleButNotAssignable>&& a) : m {a->m} {}
      MoveConstructibleButNotAssignable& operator = (MoveConstructibleButNotAssignable const&) = delete;
      MoveConstructibleButNotAssignable& operator = (MoveConstructibleButNotAssignable&&) = delete;
   };
   static_assert(not ::std::is_trivially_copy_constructible_v<MoveConstructibleButNotAssignable>);
   static_assert(not ::std::is_trivially_move_constructible_v<MoveConstructibleButNotAssignable>);
   static_assert(not ::std::is_trivially_copy_assignable_v<MoveConstructibleButNotAssignable>);
   static_assert(not ::std::is_trivially_move_assignable_v<MoveConstructibleButNotAssignable>);

   struct AbandonConstructibleButNotAssignable {
      int m;
      AbandonConstructibleButNotAssignable(Abandon<AbandonConstructibleButNotAssignable>&& a) : m {a->m} {}
      AbandonConstructibleButNotAssignable& operator = (AbandonConstructibleButNotAssignable const&) = delete;
      AbandonConstructibleButNotAssignable& operator = (AbandonConstructibleButNotAssignable&&) = delete;
   };
   static_assert(not ::std::is_trivially_copy_constructible_v<AbandonConstructibleButNotAssignable>);
   static_assert(not ::std::is_trivially_move_constructible_v<AbandonConstructibleButNotAssignable>);
   static_assert(not ::std::is_trivially_copy_assignable_v<AbandonConstructibleButNotAssignable>);
   static_assert(not ::std::is_trivially_move_assignable_v<AbandonConstructibleButNotAssignable>);

   struct DisownConstructibleButNotAssignable {
      int m;
      DisownConstructibleButNotAssignable(Disown<DisownConstructibleButNotAssignable>&& a) : m {a->m} {}
      DisownConstructibleButNotAssignable& operator = (DisownConstructibleButNotAssignable const&) = delete;
      DisownConstructibleButNotAssignable& operator = (DisownConstructibleButNotAssignable&&) = delete;
   };
   static_assert(not ::std::is_trivially_copy_constructible_v<DisownConstructibleButNotAssignable>);
   static_assert(not ::std::is_trivially_move_constructible_v<DisownConstructibleButNotAssignable>);
   static_assert(not ::std::is_trivially_copy_assignable_v<DisownConstructibleButNotAssignable>);
   static_assert(not ::std::is_trivially_move_assignable_v<DisownConstructibleButNotAssignable>);

   struct CloneConstructibleButNotAssignable {
      int m;
      CloneConstructibleButNotAssignable(Clone<CloneConstructibleButNotAssignable>&& a) : m {a->m} {}
      CloneConstructibleButNotAssignable& operator = (CloneConstructibleButNotAssignable const&) = delete;
      CloneConstructibleButNotAssignable& operator = (CloneConstructibleButNotAssignable&&) = delete;
   };
   static_assert(not ::std::is_trivially_copy_constructible_v<CloneConstructibleButNotAssignable>);
   static_assert(not ::std::is_trivially_move_constructible_v<CloneConstructibleButNotAssignable>);
   static_assert(not ::std::is_trivially_copy_assignable_v<CloneConstructibleButNotAssignable>);
   static_assert(not ::std::is_trivially_move_assignable_v<CloneConstructibleButNotAssignable>);

   /// Assignable but not constructible                                       
   struct ReferAssignableButNotConstructible {
      int m;
      ReferAssignableButNotConstructible& operator = (Refer<ReferAssignableButNotConstructible>&& a) {
         m = a->m;
         return *this;
      }
   };
   static_assert(::std::is_assignable_v<ReferAssignableButNotConstructible, Refer<ReferAssignableButNotConstructible>>);

   struct CopyAssignableButNotConstructible {
      int m;
      CopyAssignableButNotConstructible& operator = (Copy<CopyAssignableButNotConstructible>&& a) {
         m = a->m;
         return *this;
      }
   };
   static_assert(::std::is_assignable_v<CopyAssignableButNotConstructible, Copy<CopyAssignableButNotConstructible>>);

   struct MoveAssignableButNotConstructible {
      int m;
      MoveAssignableButNotConstructible& operator = (Move<MoveAssignableButNotConstructible>&& a) {
         m = a->m;
         return *this;
      }
   };
   static_assert(::std::is_assignable_v<MoveAssignableButNotConstructible, Move<MoveAssignableButNotConstructible>>);

   struct AbandonAssignableButNotConstructible {
      int m;
      AbandonAssignableButNotConstructible& operator = (Abandon<AbandonAssignableButNotConstructible>&& a) {
         m = a->m;
         return *this;
      }
   };
   static_assert(::std::is_assignable_v<AbandonAssignableButNotConstructible, Abandon<AbandonAssignableButNotConstructible>>);

   struct DisownAssignableButNotConstructible {
      int m;
      DisownAssignableButNotConstructible& operator = (Disown<DisownAssignableButNotConstructible>&& a) {
         m = a->m;
         return *this;
      }
   };
   static_assert(::std::is_assignable_v<DisownAssignableButNotConstructible, Disown<DisownAssignableButNotConstructible>>);

   struct CloneAssignableButNotConstructible {
      int m;
      CloneAssignableButNotConstructible& operator = (Clone<CloneAssignableButNotConstructible>&& a) {
         m = a->m;
         return *this;
      }
   };
   static_assert(::std::is_assignable_v<CloneAssignableButNotConstructible, Clone<CloneAssignableButNotConstructible>>);

   /// Custom POD type                                                        
   struct ForcefullyPod {
      using CTTI_POD = Yes<>;
      Complex mData;
   };
   static_assert(CT::POD<ForcefullyPod>);
   static_assert(    ::std::is_copy_constructible_v<ForcefullyPod>);
   static_assert(    ::std::is_move_constructible_v<ForcefullyPod>);
   static_assert(not ::std::is_copy_assignable_v<ForcefullyPod>); // not available due to missing in mData (implicitly deleted because of custom constructor)
   static_assert(not ::std::is_move_assignable_v<ForcefullyPod>); // not available due to missing in mData (implicitly deleted because of custom constructor)

   enum TypedEnum : int64_t {one1, two2};
   enum class TypedEnumClass : int64_t {one1, two2};
   struct IncompleteType;
}


///                                                                           
/// CT::Intent / CT::NoIntent                                                 
///                                                                           
TEST_CASE_TEMPLATE("Testing intent type", TestType
   , Refer<int>
   , Copy<int>
   , Clone<int>
   , Abandon<int>
   , Move<int>
   , Disown<int>
   , Disown<int>&
   , Disown<int> const&
   , Disown<int>&&
   , Disown<int> const&&
) {
   static_assert(    CT::Intent<TestType>);
   static_assert(not CT::NoIntent<TestType>);
}

TEST_CASE_TEMPLATE("Testing non-intent type", TestType
   , Refer<int>*
   , SheddableType<int>
   , SheddableType<Refer<int>>
   //, IncompleteType // shouldn't compile
   , TypedEnum
   , void, int, int&&, int*, nullptr_t
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
TEST_CASE("Testing IntentOf") {
   static_assert(::std::same_as<IntentOfT<int>,                 Refer<int>>);
   static_assert(::std::same_as<IntentOfT<int&&>,               Move<int>>);
   static_assert(::std::same_as<IntentOfT<int const&&>,         Refer<int>>);
   static_assert(::std::same_as<IntentOfT<int&>,                Refer<int>>);
   static_assert(::std::same_as<IntentOfT<int const&>,          Refer<int>>);

   static_assert(::std::same_as<IntentOfT<Copy<int>>,           Copy<int>>);
   static_assert(::std::same_as<IntentOfT<Copy<int>&>,          Copy<int>>);
   static_assert(::std::same_as<IntentOfT<Copy<int>&&>,         Copy<int>>);
   static_assert(::std::same_as<IntentOfT<Copy<int> const&>,    Copy<int>>);
                                                               
   static_assert(::std::same_as<IntentOfT<Refer<int>>,          Refer<int>>);
   static_assert(::std::same_as<IntentOfT<Refer<int>&>,         Refer<int>>);
   static_assert(::std::same_as<IntentOfT<Refer<int>&&>,        Refer<int>>);
   static_assert(::std::same_as<IntentOfT<Refer<int> const&>,   Refer<int>>);
                                                               
   static_assert(::std::same_as<IntentOfT<Move<int>>,           Move<int>>);
   static_assert(::std::same_as<IntentOfT<Move<int>&>,          Move<int>>);
   static_assert(::std::same_as<IntentOfT<Move<int>&&>,         Move<int>>);
   static_assert(::std::same_as<IntentOfT<Move<int> const&>,    Move<int>>);

   static_assert(::std::same_as<IntentOfT<Abandon<int>>,        Abandon<int>>);
   static_assert(::std::same_as<IntentOfT<Abandon<int>&>,       Abandon<int>>);
   static_assert(::std::same_as<IntentOfT<Abandon<int>&&>,      Abandon<int>>);
   static_assert(::std::same_as<IntentOfT<Abandon<int> const&>, Abandon<int>>);

   static_assert(::std::same_as<IntentOfT<Disown<int>>,         Disown<int>>);
   static_assert(::std::same_as<IntentOfT<Disown<int>&>,        Disown<int>>);
   static_assert(::std::same_as<IntentOfT<Disown<int>&&>,       Disown<int>>);
   static_assert(::std::same_as<IntentOfT<Disown<int> const&>,  Disown<int>>);

   static_assert(::std::same_as<IntentOfT<Clone<int>>,          Clone<int>>);
   static_assert(::std::same_as<IntentOfT<Clone<int>&>,         Clone<int>>);
   static_assert(::std::same_as<IntentOfT<Clone<int>&&>,        Clone<int>>);
   static_assert(::std::same_as<IntentOfT<Clone<int> const&>,   Clone<int>>);

   const std::string_view anArrayOfStrings[] {
      "one", "two", "three", "four"
   };
   using AOS = decltype(anArrayOfStrings);
   static_assert(::std::same_as<IntentOfT<AOS>,   Refer<Decq<AOS>>>);
   static_assert(::std::same_as<IntentOfT<AOS&>,  Refer<Decq<AOS>>>);
   static_assert(::std::same_as<IntentOfT<AOS&&>, Refer<Decq<AOS>>>);

   std::string_view anArrayOfStringsMut[] {
      "one", "two", "three", "four"
   };
   using AOSmut = decltype(anArrayOfStringsMut);
   static_assert(::std::same_as<IntentOfT<AOSmut>,   Refer<AOSmut>>);
   static_assert(::std::same_as<IntentOfT<AOSmut&>,  Refer<AOSmut>>);
   static_assert(::std::same_as<IntentOfT<AOSmut&&>, Move<AOSmut>>);

   const char* nullter = "stuff";
   using NTS = decltype(nullter);
   static_assert(::std::same_as<IntentOfT<NTS>,   Refer<NTS>>);
   static_assert(::std::same_as<IntentOfT<NTS&>,  Refer<NTS>>);
   static_assert(::std::same_as<IntentOfT<NTS&&>, Move<NTS>>);
}


///                                                                           
/// Deint                                                                     
///                                                                           
TEST_CASE("Testing Deint") {
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
TEST_CASE_TEMPLATE("Testing CT::HasReferConstructor", TestType
   , AllIntentConstructible
   , AllIntentConstructibleImplicit
   , AllIntentConstructibleAndAssignable
   , PartiallyIntentConstructible
   , PartiallyIntentConstructibleButImplicitly
   , ReferConstructibleButNotAssignable
) {
   alignas(TestType) char storage1[sizeof(TestType)] {};
   alignas(TestType) char storage2[sizeof(TestType)] {};
   auto test1 = reinterpret_cast<TestType*>(storage1);
   auto test2 = reinterpret_cast<TestType*>(storage2);
   new (test2) TestType {Refer(*test1)};

   static_assert(CT::HasIntentConstructor<Refer, TestType>);
   static_assert(CT::HasIntentConstructorAlt<Refer<TestType>>);
   static_assert(CT::HasReferConstructor<TestType>);
}

TEST_CASE_TEMPLATE("Testing not CT::HasReferConstructor", TestType
   //, IncompleteType             // should not compile at all
   , NonIntentConstructible
   , DestructibleType
   , EmptyType, AggregateType
   , NonDestructible
   , PrivatelyConstructible
   , ForcefullyPod
   , Complex, ContainsComplex
   , int
   
   , CopyConstructibleButNotAssignable
   , MoveConstructibleButNotAssignable
   , AbandonConstructibleButNotAssignable
   , DisownConstructibleButNotAssignable
   , CloneConstructibleButNotAssignable
   
   , ReferAssignableButNotConstructible
   , CopyAssignableButNotConstructible
   , MoveAssignableButNotConstructible
   , AbandonAssignableButNotConstructible
   , DisownAssignableButNotConstructible
   , CloneAssignableButNotConstructible
) {
   static_assert(not CT::HasIntentConstructor<Refer, TestType>);
   static_assert(not CT::HasIntentConstructorAlt<Refer<TestType>>);
   static_assert(not CT::HasReferConstructor<TestType>);
}

static_assert(    CT::HasReferConstructor<AllIntentConstructible, AllIntentConstructibleAndAssignable, ReferConstructibleButNotAssignable>);
static_assert(not CT::HasReferConstructor<AllIntentConstructible, AllIntentConstructibleAndAssignable, PrivatelyConstructible>);


///                                                                           
/// CT::HasCopyConstructor                                                    
///                                                                           
TEST_CASE_TEMPLATE("Testing CT::HasCopyConstructor", TestType
   , AllIntentConstructible
   , AllIntentConstructibleImplicit
   , AllIntentConstructibleAndAssignable
   , CopyConstructibleButNotAssignable
   , PartiallyIntentConstructibleButImplicitly
   , PartiallyIntentConstructible
) {
   alignas(TestType) char storage1[sizeof(TestType)] {};
   alignas(TestType) char storage2[sizeof(TestType)] {};
   auto test1 = reinterpret_cast<TestType*>(storage1);
   auto test2 = reinterpret_cast<TestType*>(storage2);
   new (test2) TestType {Copy(*test1)};

   static_assert(CT::HasIntentConstructor<Copy, TestType>);
   static_assert(CT::HasIntentConstructorAlt<Copy<TestType>>);
   static_assert(CT::HasCopyConstructor<TestType>);
}

TEST_CASE_TEMPLATE("Testing not CT::HasCopyConstructor", TestType
   //, IncompleteType // should not compile at all
   , AggregateType
   , EmptyType
   , DestructibleType
   , NonIntentConstructible
   , NonDestructible
   , PrivatelyConstructible
   , ForcefullyPod
   , Complex, ContainsComplex
   , int

   , ReferConstructibleButNotAssignable
   , MoveConstructibleButNotAssignable
   , AbandonConstructibleButNotAssignable
   , DisownConstructibleButNotAssignable
   , CloneConstructibleButNotAssignable
   
   , ReferAssignableButNotConstructible
   , CopyAssignableButNotConstructible
   , MoveAssignableButNotConstructible
   , AbandonAssignableButNotConstructible
   , DisownAssignableButNotConstructible
   , CloneAssignableButNotConstructible
) {
   static_assert(not requires (Copy<TestType>&& a) { TestType {FWD(a)}; });
   static_assert(not CT::HasIntentConstructor<Copy, TestType>);
   static_assert(not CT::HasIntentConstructorAlt<Copy<TestType>>);
   static_assert(not CT::HasCopyConstructor<TestType>);
}

static_assert(    CT::HasCopyConstructor<AllIntentConstructible, AllIntentConstructibleAndAssignable, CopyConstructibleButNotAssignable>);
static_assert(not CT::HasCopyConstructor<AllIntentConstructible, AllIntentConstructibleAndAssignable, PrivatelyConstructible>);


///                                                                           
/// CT::HasCloneConstructor                                                   
///                                                                           
TEST_CASE_TEMPLATE("Testing CT::HasCloneConstructor", TestType
   , AllIntentConstructible
   , AllIntentConstructibleImplicit
   , AllIntentConstructibleAndAssignable
   , CloneConstructibleButNotAssignable
   , PartiallyIntentConstructibleButImplicitly
   , PartiallyIntentConstructible
) {
   alignas(TestType) char storage1[sizeof(TestType)] {};
   alignas(TestType) char storage2[sizeof(TestType)] {};
   auto test1 = reinterpret_cast<TestType*>(storage1);
   auto test2 = reinterpret_cast<TestType*>(storage2);
   new (test2) TestType {Clone(*test1)};

   static_assert(CT::HasIntentConstructor<Clone, TestType>);
   static_assert(CT::HasIntentConstructorAlt<Clone<TestType>>);
   static_assert(CT::HasCloneConstructor<TestType>);
}

TEST_CASE_TEMPLATE("Testing not CT::HasCloneConstructor", TestType
   //, IncompleteType // should not compile at all
   , AggregateType
   , EmptyType
   , DestructibleType
   , NonIntentConstructible
   , NonDestructible
   , PrivatelyConstructible
   , Complex, ContainsComplex
   , ForcefullyPod
   , int
   
   , ReferConstructibleButNotAssignable
   , MoveConstructibleButNotAssignable
   , AbandonConstructibleButNotAssignable
   , DisownConstructibleButNotAssignable
   , CopyConstructibleButNotAssignable
   
   , ReferAssignableButNotConstructible
   , CopyAssignableButNotConstructible
   , MoveAssignableButNotConstructible
   , AbandonAssignableButNotConstructible
   , DisownAssignableButNotConstructible
   , CloneAssignableButNotConstructible
) {
   static_assert(not requires (Clone<TestType>&& a) { TestType {FWD(a)}; });
   static_assert(not CT::HasIntentConstructor<Clone, TestType>);
   static_assert(not CT::HasIntentConstructorAlt<Clone<TestType>>);
   static_assert(not CT::HasCloneConstructor<TestType>);
}

static_assert(    CT::HasCloneConstructor<AllIntentConstructible, AllIntentConstructibleAndAssignable, CloneConstructibleButNotAssignable>);
static_assert(not CT::HasCloneConstructor<AllIntentConstructible, AllIntentConstructibleAndAssignable, PrivatelyConstructible>);


///                                                                           
/// CT::HasDisownConstructor                                                  
///                                                                           
TEST_CASE_TEMPLATE("Testing CT::HasDisownConstructor", TestType
   , AllIntentConstructible
   , AllIntentConstructibleImplicit
   , AllIntentConstructibleAndAssignable
   , DisownConstructibleButNotAssignable
   , PartiallyIntentConstructibleButImplicitly
   , PartiallyIntentConstructible
) {
   alignas(TestType) char storage1[sizeof(TestType)] {};
   alignas(TestType) char storage2[sizeof(TestType)] {};
   auto test1 = reinterpret_cast<TestType*>(storage1);
   auto test2 = reinterpret_cast<TestType*>(storage2);
   new (test2) TestType {Disown(*test1)};

   static_assert(CT::HasIntentConstructor<Disown, TestType>);
   static_assert(CT::HasIntentConstructorAlt<Disown<TestType>>);
   static_assert(CT::HasDisownConstructor<TestType>);
}

TEST_CASE_TEMPLATE("Testing not CT::HasDisownConstructor", TestType
   //, IncompleteType // should not compile at all
   , AggregateType
   , EmptyType
   , DestructibleType
   , NonIntentConstructible
   , NonDestructible
   , PrivatelyConstructible
   , Complex, ContainsComplex
   , ForcefullyPod
   , int
   
   , ReferConstructibleButNotAssignable
   , MoveConstructibleButNotAssignable
   , AbandonConstructibleButNotAssignable
   , CloneConstructibleButNotAssignable
   , CopyConstructibleButNotAssignable
   
   , ReferAssignableButNotConstructible
   , CopyAssignableButNotConstructible
   , MoveAssignableButNotConstructible
   , AbandonAssignableButNotConstructible
   , DisownAssignableButNotConstructible
   , CloneAssignableButNotConstructible
) {
   static_assert(not CT::HasIntentConstructor<Disown, TestType>);
   static_assert(not CT::HasIntentConstructorAlt<Disown<TestType>>);
   static_assert(not CT::HasDisownConstructor<TestType>);
}

static_assert(    CT::HasDisownConstructor<AllIntentConstructible, AllIntentConstructibleAndAssignable, DisownConstructibleButNotAssignable>);
static_assert(not CT::HasDisownConstructor<AllIntentConstructible, AllIntentConstructibleAndAssignable, PrivatelyConstructible>);


///                                                                           
/// CT::HasAbandonConstructor                                                 
///                                                                           
TEST_CASE_TEMPLATE("Testing CT::HasAbandonConstructor", TestType
   , AllIntentConstructible
   , AllIntentConstructibleImplicit
   , AllIntentConstructibleAndAssignable
   , AbandonConstructibleButNotAssignable
   , PartiallyIntentConstructible
   , PartiallyIntentConstructibleButImplicitly
) {
   alignas(TestType) char storage1[sizeof(TestType)] {};
   alignas(TestType) char storage2[sizeof(TestType)] {};
   auto test1 = reinterpret_cast<TestType*>(storage1);
   auto test2 = reinterpret_cast<TestType*>(storage2);
   new (test2) TestType {Abandon(*test1)};

   static_assert(CT::HasIntentConstructor<Abandon, TestType>);
   static_assert(CT::HasIntentConstructorAlt<Abandon<TestType>>);
   static_assert(CT::HasAbandonConstructor<TestType>);
}

TEST_CASE_TEMPLATE("Testing not CT::HasAbandonConstructor", TestType
   //, IncompleteType // should not compile at all
   , AggregateType
   , EmptyType
   , DestructibleType
   , NonDestructible
   , PrivatelyConstructible
   , ForcefullyPod
   , NonIntentConstructible
   , Complex, ContainsComplex
   , int
   
   , ReferConstructibleButNotAssignable
   , MoveConstructibleButNotAssignable
   , CloneConstructibleButNotAssignable
   , DisownConstructibleButNotAssignable
   , CopyConstructibleButNotAssignable
   
   , ReferAssignableButNotConstructible
   , CopyAssignableButNotConstructible
   , MoveAssignableButNotConstructible
   , AbandonAssignableButNotConstructible
   , DisownAssignableButNotConstructible
   , CloneAssignableButNotConstructible
) {
   static_assert(not CT::HasIntentConstructor<Abandon, TestType>);
   static_assert(not CT::HasIntentConstructorAlt<Abandon<TestType>>);
   static_assert(not CT::HasAbandonConstructor<TestType>);
}

static_assert(    CT::HasAbandonConstructor<AllIntentConstructible, AllIntentConstructibleAndAssignable, AbandonConstructibleButNotAssignable>);
static_assert(not CT::HasAbandonConstructor<AllIntentConstructible, AllIntentConstructibleAndAssignable, PrivatelyConstructible>);


///                                                                           
/// CT::HasMoveConstructor                                                    
///                                                                           
TEST_CASE_TEMPLATE("Testing CT::HasMoveConstructor", TestType
   , AllIntentConstructible
   , AllIntentConstructibleImplicit
   , AllIntentConstructibleAndAssignable
   , MoveConstructibleButNotAssignable
   , PartiallyIntentConstructible
   , PartiallyIntentConstructibleButImplicitly
) {
   alignas(TestType) char storage1[sizeof(TestType)] {};
   alignas(TestType) char storage2[sizeof(TestType)] {};
   auto test1 = reinterpret_cast<TestType*>(storage1);
   auto test2 = reinterpret_cast<TestType*>(storage2);
   new (test2) TestType {Move(*test1)};

   static_assert(CT::HasIntentConstructor<Move, TestType>);
   static_assert(CT::HasIntentConstructorAlt<Move<TestType>>);
   static_assert(CT::HasMoveConstructor<TestType>);
}

TEST_CASE_TEMPLATE("Testing not CT::HasMoveConstructor", TestType
   //, IncompleteType // should not compile at all
   , AggregateType
   , EmptyType
   , DestructibleType
   , NonDestructible
   , PrivatelyConstructible
   , ForcefullyPod
   , NonIntentConstructible
   , Complex, ContainsComplex
   , int
   
   , ReferConstructibleButNotAssignable
   , CloneConstructibleButNotAssignable
   , AbandonConstructibleButNotAssignable
   , DisownConstructibleButNotAssignable
   , CopyConstructibleButNotAssignable
   
   , ReferAssignableButNotConstructible
   , CopyAssignableButNotConstructible
   , MoveAssignableButNotConstructible
   , AbandonAssignableButNotConstructible
   , DisownAssignableButNotConstructible
   , CloneAssignableButNotConstructible
) {
   static_assert(not CT::HasIntentConstructor<Move, TestType>);
   static_assert(not CT::HasIntentConstructorAlt<Move<TestType>>);
   static_assert(not CT::HasMoveConstructor<TestType>);
}

static_assert(    CT::HasMoveConstructor<AllIntentConstructible, AllIntentConstructibleAndAssignable, MoveConstructibleButNotAssignable>);
static_assert(not CT::HasMoveConstructor<AllIntentConstructible, AllIntentConstructibleAndAssignable, PrivatelyConstructible>);


///                                                                           
/// CT::HasReferAssign                                                        
///                                                                           
TEST_CASE_TEMPLATE("Testing CT::HasReferAssign", TestType
   , AllIntentConstructibleImplicit
   , AllIntentConstructibleAndAssignable
   , ReferAssignableButNotConstructible
   , PartiallyIntentConstructibleButImplicitly
) {
   alignas(TestType) char storage1[sizeof(TestType)] {};
   alignas(TestType) char storage2[sizeof(TestType)] {};
   auto test1 = reinterpret_cast<TestType*>(storage1);
   auto test2 = reinterpret_cast<TestType*>(storage2);
   *test2 = Refer(*test1);

   static_assert(CT::HasIntentAssign<Refer, TestType>);
   static_assert(CT::HasIntentAssignAlt<Refer<TestType>>);
   static_assert(CT::HasReferAssign<TestType>);
}

TEST_CASE_TEMPLATE("Testing not CT::HasReferAssign", TestType
   //, IncompleteType   // should not compile at all
   , DestructibleType
   , EmptyType, AggregateType
   , NonDestructible
   , AllIntentConstructible
   , NonIntentConstructible
   , PartiallyIntentConstructible
   , PrivatelyConstructible
   , ForcefullyPod
   , Complex, ContainsComplex
   , int
   
   , ReferConstructibleButNotAssignable
   , CopyConstructibleButNotAssignable
   , MoveConstructibleButNotAssignable
   , AbandonConstructibleButNotAssignable
   , DisownConstructibleButNotAssignable
   , CloneConstructibleButNotAssignable
   
   , CopyAssignableButNotConstructible
   , MoveAssignableButNotConstructible
   , AbandonAssignableButNotConstructible
   , DisownAssignableButNotConstructible
   , CloneAssignableButNotConstructible
) {
   static_assert(not CT::HasIntentAssign<Refer, TestType>);
   static_assert(not CT::HasIntentAssignAlt<Refer<TestType>>);
   static_assert(not CT::HasReferAssign<TestType>);
}

static_assert(    CT::HasReferAssign<AllIntentConstructibleAndAssignable, ReferAssignableButNotConstructible>);
static_assert(not CT::HasReferAssign<AllIntentConstructibleAndAssignable, PrivatelyConstructible>);


///                                                                           
/// CT::HasCopyAssign                                                         
///                                                                           
TEST_CASE_TEMPLATE("Testing CT::HasCopyAssign", TestType
   , AllIntentConstructibleImplicit
   , AllIntentConstructibleAndAssignable
   , PartiallyIntentConstructibleButImplicitly
   , CopyAssignableButNotConstructible
) {
   alignas(TestType) char storage1[sizeof(TestType)] {};
   alignas(TestType) char storage2[sizeof(TestType)] {};
   auto test1 = reinterpret_cast<TestType*>(storage1);
   auto test2 = reinterpret_cast<TestType*>(storage2);
   *test2 = Copy(*test1);

   static_assert(CT::HasIntentAssign<Copy, TestType>);
   static_assert(CT::HasIntentAssignAlt<Copy<TestType>>);
   static_assert(CT::HasCopyAssign<TestType>);
}

TEST_CASE_TEMPLATE("Testing not CT::HasCopyAssign", TestType
   //, IncompleteType        // should not compile at all
   , PartiallyIntentConstructible
   , Complex, ContainsComplex
   , AllIntentConstructible
   , NonIntentConstructible
   , DestructibleType
   , EmptyType, AggregateType
   , NonDestructible
   , PrivatelyConstructible
   , ForcefullyPod
   , int
   
   , ReferConstructibleButNotAssignable
   , CopyConstructibleButNotAssignable
   , MoveConstructibleButNotAssignable
   , AbandonConstructibleButNotAssignable
   , DisownConstructibleButNotAssignable
   , CloneConstructibleButNotAssignable
   
   , ReferAssignableButNotConstructible
   , MoveAssignableButNotConstructible
   , AbandonAssignableButNotConstructible
   , DisownAssignableButNotConstructible
   , CloneAssignableButNotConstructible
) {
   static_assert(not CT::HasIntentAssign<Copy, TestType>);
   static_assert(not CT::HasIntentAssignAlt<Copy<TestType>>);
   static_assert(not CT::HasCopyAssign<TestType>);
}

static_assert(    CT::HasCopyAssign<AllIntentConstructibleAndAssignable, CopyAssignableButNotConstructible>);
static_assert(not CT::HasCopyAssign<AllIntentConstructibleAndAssignable, PrivatelyConstructible>);


///                                                                           
/// CT::HasCloneAssign                                                        
///                                                                           
TEST_CASE_TEMPLATE("Testing CT::HasCloneAssign", TestType
   , AllIntentConstructibleImplicit
   , AllIntentConstructibleAndAssignable
   , PartiallyIntentConstructibleButImplicitly
   , CloneAssignableButNotConstructible
) {
   alignas(TestType) char storage1[sizeof(TestType)] {};
   alignas(TestType) char storage2[sizeof(TestType)] {};
   auto test1 = reinterpret_cast<TestType*>(storage1);
   auto test2 = reinterpret_cast<TestType*>(storage2);
   *test2 = Clone(*test1);

   static_assert(CT::HasIntentAssign<Clone, TestType>);
   static_assert(CT::HasIntentAssignAlt<Clone<TestType>>);
   static_assert(CT::HasCloneAssign<TestType>);
}

TEST_CASE_TEMPLATE("Testing not CT::HasCloneAssign", TestType
   //, IncompleteType    // should not compile at all
   , PartiallyIntentConstructible
   , Complex, ContainsComplex
   , NonIntentConstructible
   , AllIntentConstructible
   , DestructibleType
   , EmptyType, AggregateType
   , NonDestructible
   , PrivatelyConstructible
   , ForcefullyPod
   , int
   
   , ReferConstructibleButNotAssignable
   , CopyConstructibleButNotAssignable
   , MoveConstructibleButNotAssignable
   , AbandonConstructibleButNotAssignable
   , DisownConstructibleButNotAssignable
   , CloneConstructibleButNotAssignable
   
   , CopyAssignableButNotConstructible
   , MoveAssignableButNotConstructible
   , AbandonAssignableButNotConstructible
   , DisownAssignableButNotConstructible
   , ReferAssignableButNotConstructible
) {
   static_assert(not CT::HasIntentAssign<Clone, TestType>);
   static_assert(not CT::HasIntentAssignAlt<Clone<TestType>>);
   static_assert(not CT::HasCloneAssign<TestType>);
}

static_assert(    CT::HasCloneAssign<AllIntentConstructibleAndAssignable, CloneAssignableButNotConstructible>);
static_assert(not CT::HasCloneAssign<AllIntentConstructibleAndAssignable, PrivatelyConstructible>);


///                                                                           
/// CT::HasDisownAssign                                                       
///                                                                           
TEST_CASE_TEMPLATE("Testing CT::HasDisownAssign", TestType
   , AllIntentConstructibleImplicit
   , AllIntentConstructibleAndAssignable
   , PartiallyIntentConstructibleButImplicitly
   , DisownAssignableButNotConstructible
) {
   alignas(TestType) char storage1[sizeof(TestType)] {};
   alignas(TestType) char storage2[sizeof(TestType)] {};
   auto test1 = reinterpret_cast<TestType*>(storage1);
   auto test2 = reinterpret_cast<TestType*>(storage2);
   *test2 = Disown(*test1);

   static_assert(CT::HasIntentAssign<Disown, TestType>);
   static_assert(CT::HasIntentAssignAlt<Disown<TestType>>);
   static_assert(CT::HasDisownAssign<TestType>);
}

TEST_CASE_TEMPLATE("Testing not CT::HasDisownAssign", TestType
   //, IncompleteType  // should not compile at all
   , PartiallyIntentConstructible
   , Complex, ContainsComplex
   , NonIntentConstructible
   , AllIntentConstructible
   , DestructibleType
   , EmptyType, AggregateType
   , NonDestructible
   , PrivatelyConstructible
   , ForcefullyPod
   , int
   
   , ReferConstructibleButNotAssignable
   , CopyConstructibleButNotAssignable
   , MoveConstructibleButNotAssignable
   , AbandonConstructibleButNotAssignable
   , DisownConstructibleButNotAssignable
   , CloneConstructibleButNotAssignable
   
   , CopyAssignableButNotConstructible
   , MoveAssignableButNotConstructible
   , AbandonAssignableButNotConstructible
   , ReferAssignableButNotConstructible
   , CloneAssignableButNotConstructible
) {
   static_assert(not CT::HasIntentAssign<Disown, TestType>);
   static_assert(not CT::HasIntentAssignAlt<Disown<TestType>>);
   static_assert(not CT::HasDisownAssign<TestType>);
}

static_assert(    CT::HasDisownAssign<AllIntentConstructibleAndAssignable, DisownAssignableButNotConstructible>);
static_assert(not CT::HasDisownAssign<AllIntentConstructibleAndAssignable, PrivatelyConstructible>);


///                                                                           
/// CT::HasAbandonAssign                                                      
///                                                                           
TEST_CASE_TEMPLATE("Testing CT::HasAbandonAssign", TestType
   , AllIntentConstructibleImplicit
   , AllIntentConstructibleAndAssignable
   , PartiallyIntentConstructibleButImplicitly
   , AbandonAssignableButNotConstructible
) {
   alignas(TestType) char storage1[sizeof(TestType)] {};
   alignas(TestType) char storage2[sizeof(TestType)] {};
   auto test1 = reinterpret_cast<TestType*>(storage1);
   auto test2 = reinterpret_cast<TestType*>(storage2);
   *test2 = Abandon(*test1);

   static_assert(CT::HasIntentAssign<Abandon, TestType>);
   static_assert(CT::HasIntentAssignAlt<Abandon<TestType>>);
   static_assert(CT::HasAbandonAssign<TestType>);
}

TEST_CASE_TEMPLATE("Testing not CT::HasAbandonAssign", TestType
   //, IncompleteType     // should not compile at all
   , AllIntentConstructible
   , PartiallyIntentConstructible
   , NonDestructible
   , NonIntentConstructible
   , Complex, ContainsComplex
   , PrivatelyConstructible
   , ForcefullyPod
   , DestructibleType
   , EmptyType, AggregateType
   , int
   
   , ReferConstructibleButNotAssignable
   , CopyConstructibleButNotAssignable
   , MoveConstructibleButNotAssignable
   , AbandonConstructibleButNotAssignable
   , DisownConstructibleButNotAssignable
   , CloneConstructibleButNotAssignable
   
   , MoveAssignableButNotConstructible
   , CopyAssignableButNotConstructible
   , ReferAssignableButNotConstructible
   , DisownAssignableButNotConstructible
   , CloneAssignableButNotConstructible
) {
   static_assert(not CT::HasIntentAssign<Abandon, TestType>);
   static_assert(not CT::HasIntentAssignAlt<Abandon<TestType>>);
   static_assert(not CT::HasAbandonAssign<TestType>);
}

static_assert(    CT::HasAbandonAssign<AllIntentConstructibleAndAssignable, AbandonAssignableButNotConstructible>);
static_assert(not CT::HasAbandonAssign<AllIntentConstructibleAndAssignable, PrivatelyConstructible>);


///                                                                           
/// CT::HasMoveAssign                                                         
///                                                                           
TEST_CASE_TEMPLATE("Testing CT::HasMoveAssign", TestType
   , AllIntentConstructibleImplicit
   , AllIntentConstructibleAndAssignable
   , MoveAssignableButNotConstructible
   , PartiallyIntentConstructibleButImplicitly
) {
   alignas(TestType) char storage1[sizeof(TestType)] {};
   alignas(TestType) char storage2[sizeof(TestType)] {};
   auto test1 = reinterpret_cast<TestType*>(storage1);
   auto test2 = reinterpret_cast<TestType*>(storage2);
   *test2 = Move(*test1);

   static_assert(CT::HasIntentAssign<Move, TestType>);
   static_assert(CT::HasIntentAssignAlt<Move<TestType>>);
   static_assert(CT::HasMoveAssign<TestType>);
}

TEST_CASE_TEMPLATE("Testing not CT::HasMoveAssign", TestType
   //IncompleteType,             // should not compile at all
   , NonDestructible
   , NonIntentConstructible
   , AllIntentConstructible
   , PartiallyIntentConstructible
   , Complex, ContainsComplex
   , PrivatelyConstructible
   , ForcefullyPod
   , DestructibleType
   , EmptyType, AggregateType
   , int
   
   , ReferConstructibleButNotAssignable
   , CopyConstructibleButNotAssignable
   , MoveConstructibleButNotAssignable
   , AbandonConstructibleButNotAssignable
   , DisownConstructibleButNotAssignable
   , CloneConstructibleButNotAssignable
   
   , AbandonAssignableButNotConstructible
   , CopyAssignableButNotConstructible
   , ReferAssignableButNotConstructible
   , DisownAssignableButNotConstructible
   , CloneAssignableButNotConstructible
) {
   static_assert(not CT::HasIntentAssign<Move, TestType>);
   static_assert(not CT::HasIntentAssignAlt<Move<TestType>>);
   static_assert(not CT::HasMoveAssign<TestType>);
}

static_assert(    CT::HasMoveAssign<AllIntentConstructibleAndAssignable, MoveAssignableButNotConstructible>);
static_assert(not CT::HasMoveAssign<AllIntentConstructibleAndAssignable, PrivatelyConstructible>);


///                                                                           
///   Refer intent                                                            
///                                                                           
TEST_CASE_TEMPLATE("Testing refer-constructible types", T
   , AggregateType
   , EmptyType
   , DestructibleType
   , NonIntentConstructible
   , AllIntentConstructible
   , AllIntentConstructibleAndAssignable
   , PartiallyIntentConstructible
   , PartiallyIntentConstructibleButImplicitly
   , Complex, ContainsComplex
   , ReferConstructibleButNotAssignable
   , ForcefullyPod
   , int
) {
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

   auto meta1 = MetaDataOf<T>();
   REQUIRE(meta1);
   REQUIRE(meta1.GetReferConstructor());

   auto meta2 = MetaDataOf<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2.GetReferConstructor());

   auto meta3 = MetaDataOf<const T>();
   REQUIRE(meta3);
   REQUIRE(meta3.GetReferConstructor());

   alignas(T) char storage1[sizeof(T)] {};
   alignas(T) char storage2[sizeof(T)] {};
   auto test1 = reinterpret_cast<T*>(storage1);
   auto test2 = reinterpret_cast<T*>(storage2);
   new (test1) T {*test2};
}

TEST_CASE_TEMPLATE("Testing non-refer-constructible types", T
   //IncompleteType, // should not compile at all
   , NonDestructible
   , PrivatelyConstructible
   , CopyConstructibleButNotAssignable
   , MoveConstructibleButNotAssignable
   , AbandonConstructibleButNotAssignable
   , DisownConstructibleButNotAssignable
   , CloneConstructibleButNotAssignable
) {
   static_assert(not CT::ReferConstructible<T>);
   static_assert(    CT::ReferConstructible<T*>);
   static_assert(not CT::IntentConstructible<Refer, T>);
   static_assert(    CT::IntentConstructible<Refer, T*>);
   static_assert(not CT::IntentConstructibleAlt<Refer<T>>);
   static_assert(    CT::IntentConstructibleAlt<Refer<T*>>);

   auto meta1 = MetaDataOf<T>();
   REQUIRE(meta1);
   REQUIRE_FALSE(meta1.GetReferConstructor());

   auto meta2 = MetaDataOf<const T>();
   REQUIRE(meta2);
   REQUIRE_FALSE(meta2.GetReferConstructor());
}

TEST_CASE_TEMPLATE("Testing refer-assignable types", T
   , AggregateType
   , EmptyType
   , NonDestructible
   , DestructibleType
   , NonIntentConstructible
   , AllIntentConstructible
   , AllIntentConstructibleAndAssignable
   , PartiallyIntentConstructible
   , PartiallyIntentConstructibleButImplicitly
   , int
) {
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

   auto meta1 = MetaDataOf<T>();
   REQUIRE(meta1);
   REQUIRE(meta1.GetReferAssigner());

   auto meta2 = MetaDataOf<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2.GetReferAssigner());

   auto meta3 = MetaDataOf<const T>();
   REQUIRE(meta3);
   REQUIRE_FALSE(meta3.GetReferAssigner());

   alignas(T) char storage1[sizeof(T)] {};
   alignas(T) char storage2[sizeof(T)] {};
   auto test1 = reinterpret_cast<T*>(storage1);
   auto test2 = reinterpret_cast<T*>(storage2);
   *test1 = *test2;
}

TEST_CASE_TEMPLATE("Testing non-refer-assignable types", T
   //, IncompleteType, // should not compile at all
   , Complex
   , ContainsComplex
   , PrivatelyConstructible
   , ReferConstructibleButNotAssignable
   , CopyConstructibleButNotAssignable
   , MoveConstructibleButNotAssignable
   , AbandonConstructibleButNotAssignable
   , DisownConstructibleButNotAssignable
   , CloneConstructibleButNotAssignable
   , ForcefullyPod
) {
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

   auto meta1 = MetaDataOf<T>();
   REQUIRE(meta1);
   REQUIRE_FALSE(meta1.GetReferAssigner());

   auto meta2 = MetaDataOf<const T>();
   REQUIRE(meta2);
   REQUIRE_FALSE(meta2.GetReferAssigner());
}


///                                                                           
///   Move intents                                                            
///                                                                           
TEST_CASE_TEMPLATE("Testing move-constructible types", T
   , AggregateType
   , EmptyType
   , DestructibleType
   , NonIntentConstructible
   , AllIntentConstructible
   , AllIntentConstructibleAndAssignable
   , PartiallyIntentConstructible
   , PartiallyIntentConstructibleButImplicitly
   , Complex
   , ContainsComplex
   , MoveConstructibleButNotAssignable
   , ForcefullyPod
   , int
) {
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
   REQUIRE(meta1.GetMoveConstructor());

   auto meta2 = MetaDataOf<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2.GetMoveConstructor());

   auto meta3 = MetaDataOf<const T>();
   REQUIRE(meta3);
   REQUIRE(meta3.GetMoveConstructor());

   alignas(T) char storage1[sizeof(T)] {};
   alignas(T) char storage2[sizeof(T)] {};
   auto test1 = reinterpret_cast<T*>(storage1);
   auto test2 = reinterpret_cast<T*>(storage2);
   new (test1) T {MOV(*test2)};
}

TEST_CASE_TEMPLATE("Testing non-move-constructible types", T
   //IncompleteType, // should not compile at all
   , NonDestructible
   , PrivatelyConstructible
   , ReferConstructibleButNotAssignable
   , CopyConstructibleButNotAssignable
   , AbandonConstructibleButNotAssignable
   , DisownConstructibleButNotAssignable
   , CloneConstructibleButNotAssignable
) {
   static_assert(not CT::MoveConstructible<T>);
   static_assert(    CT::MoveConstructible<T*>);
   static_assert(not CT::IntentConstructible<Move, T>);
   static_assert(    CT::IntentConstructible<Move, T*>);
   static_assert(not CT::IntentConstructibleAlt<Move<T>>);
   static_assert(    CT::IntentConstructibleAlt<Move<T*>>);

   auto meta1 = MetaDataOf<T>();
   REQUIRE(meta1);
   REQUIRE_FALSE(meta1.GetMoveConstructor());

   auto meta2 = MetaDataOf<const T>();
   REQUIRE(meta2);
   REQUIRE_FALSE(meta2.GetMoveConstructor());
}

TEST_CASE_TEMPLATE("Testing move-assignable types", T
   , NonDestructible
   , AggregateType
   , EmptyType
   , DestructibleType
   , NonIntentConstructible
   , AllIntentConstructible
   , AllIntentConstructibleAndAssignable
   , PartiallyIntentConstructible
   , PartiallyIntentConstructibleButImplicitly
   , int
) {
   static_assert(    CT::MoveAssignable<T>);
   static_assert(not CT::MoveAssignable<T const>);
   static_assert(    CT::MoveAssignable<T*>);
   static_assert(    CT::MoveAssignable<T const*>);
   static_assert(    CT::IntentAssignable<Move, T>);
   static_assert(not CT::IntentAssignable<Move, T const>);
   static_assert(    CT::IntentAssignable<Move, T*>);
   static_assert(    CT::IntentAssignable<Move, T const*>);
   static_assert(    CT::IntentAssignableAlt<Move<T>>);
   //static_assert(not CT::IntentAssignableAlt<Move<T const>>); // should not compile
   static_assert(    CT::IntentAssignableAlt<Move<T*>>);
   static_assert(    CT::IntentAssignableAlt<Move<T const*>>);

   auto meta1 = MetaDataOf<T>();
   REQUIRE(meta1);
   REQUIRE(meta1.GetMoveAssigner());

   auto meta2 = MetaDataOf<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2.GetMoveAssigner());

   auto meta3 = MetaDataOf<const T>();
   REQUIRE(meta3);
   REQUIRE_FALSE(meta3.GetMoveAssigner());

   alignas(T) char storage1[sizeof(T)] {};
   alignas(T) char storage2[sizeof(T)] {};
   auto test1 = reinterpret_cast<T*>(storage1);
   auto test2 = reinterpret_cast<T*>(storage2);
   *test1 = MOV(*test2);
}

TEST_CASE_TEMPLATE("Testing non-move-assignable types", T
   //, IncompleteType // should not compile at all
   , Complex
   , ContainsComplex
   , PrivatelyConstructible
   , ReferConstructibleButNotAssignable
   , MoveConstructibleButNotAssignable
   , CopyConstructibleButNotAssignable
   , AbandonConstructibleButNotAssignable
   , DisownConstructibleButNotAssignable
   , CloneConstructibleButNotAssignable
   , ForcefullyPod
) {
   static_assert(not CT::MoveAssignable<T>);
   static_assert(not CT::MoveAssignable<T const>);
   static_assert(    CT::MoveAssignable<T*>);
   static_assert(    CT::MoveAssignable<T const*>);
   static_assert(not CT::IntentAssignable<Move, T>);
   static_assert(not CT::IntentAssignable<Move, T const>);
   static_assert(    CT::IntentAssignable<Move, T*>);
   static_assert(    CT::IntentAssignable<Move, T const*>);
   static_assert(not CT::IntentAssignableAlt<Move<T>>);
   //static_assert(not CT::IntentAssignableAlt<Move<T const>>); // should not compile
   static_assert(    CT::IntentAssignableAlt<Move<T*>>);
   static_assert(    CT::IntentAssignableAlt<Move<T const*>>);

   auto meta1 = MetaDataOf<T>();
   REQUIRE(meta1);
   REQUIRE_FALSE(meta1.GetMoveAssigner());

   auto meta2 = MetaDataOf<const T>();
   REQUIRE(meta2);
   REQUIRE_FALSE(meta2.GetMoveAssigner());
}


///                                                                           
///   Copy intents                                                            
///                                                                           
TEST_CASE_TEMPLATE("Testing copy-constructible types", T
   , EmptyType
   , AggregateType
   , AllIntentConstructible
   , AllIntentConstructibleImplicit
   , AllIntentConstructibleAndAssignable
   , PartiallyIntentConstructible
   , PartiallyIntentConstructibleButImplicitly
   , CopyConstructibleButNotAssignable
   , ForcefullyPod
   , int
) {
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
   REQUIRE(meta1.GetCopyConstructor());

   auto meta2 = MetaDataOf<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2.GetCopyConstructor());

   auto meta3 = MetaDataOf<const T>();
   REQUIRE(meta3);
   REQUIRE(meta3.GetCopyConstructor());
}

TEST_CASE_TEMPLATE("Testing non-copy-constructible types", T
   //, IncompleteType // should not compile at all
   , NonDestructible
   , DestructibleType
   , Complex
   , ContainsComplex
   , PrivatelyConstructible
   , NonIntentConstructible
   , AggregateTypeComplex
   , ReferConstructibleButNotAssignable
   , MoveConstructibleButNotAssignable
   , AbandonConstructibleButNotAssignable
   , DisownConstructibleButNotAssignable
   , CloneConstructibleButNotAssignable
) {
   static_assert(not CT::CopyConstructible<T>);
   static_assert(    CT::CopyConstructible<T*>);
   static_assert(not CT::IntentConstructible<Copy, T>);
   static_assert(    CT::IntentConstructible<Copy, T*>);
   static_assert(not CT::IntentConstructibleAlt<Copy<T>>);
   static_assert(    CT::IntentConstructibleAlt<Copy<T*>>);

   auto meta1 = MetaDataOf<T>();
   REQUIRE(meta1);
   REQUIRE_FALSE(meta1.GetCopyConstructor());

   auto meta2 = MetaDataOf<const T>();
   REQUIRE(meta2);
   REQUIRE_FALSE(meta2.GetCopyConstructor());
}

TEST_CASE_TEMPLATE("Testing copy-assignable types", T
   , EmptyType
   , AggregateType
   , AllIntentConstructibleImplicit
   , AllIntentConstructibleAndAssignable
   , PartiallyIntentConstructibleButImplicitly
   , int
) {
   static_assert(    CT::CopyAssignable<T>);
   static_assert(not CT::CopyAssignable<T const>);
   static_assert(    CT::CopyAssignable<T*>);
   static_assert(    CT::CopyAssignable<T const*>);
   static_assert(    CT::IntentAssignable<Copy, T>);
   static_assert(not CT::IntentAssignable<Copy, T const>);
   static_assert(    CT::IntentAssignable<Copy, T*>);
   static_assert(    CT::IntentAssignable<Copy, T const*>);
   static_assert(    CT::IntentAssignableAlt<Copy<T>>);
   //static_assert(not CT::IntentAssignableAlt<Copy<T const>>); // should not compile
   static_assert(    CT::IntentAssignableAlt<Copy<T*>>);
   static_assert(    CT::IntentAssignableAlt<Copy<T const*>>);

   auto meta1 = MetaDataOf<T>();
   REQUIRE(meta1);
   REQUIRE(meta1.GetCopyAssigner());

   auto meta2 = MetaDataOf<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2.GetCopyAssigner());

   auto meta3 = MetaDataOf<const T>();
   REQUIRE(meta3);
   REQUIRE_FALSE(meta3.GetCopyAssigner());
}

TEST_CASE_TEMPLATE("Testing non-copy-assignable types", T
   //, IncompleteType // should not compile at all
   , NonDestructible
   , DestructibleType
   , Complex
   , ContainsComplex
   , ForcefullyPod
   , PrivatelyConstructible
   , NonIntentConstructible
   , AllIntentConstructible
   , PartiallyIntentConstructible
   , AggregateTypeComplex
   , ReferConstructibleButNotAssignable
   , MoveConstructibleButNotAssignable
   , CopyConstructibleButNotAssignable
   , AbandonConstructibleButNotAssignable
   , DisownConstructibleButNotAssignable
   , CloneConstructibleButNotAssignable
) {
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

   auto meta1 = MetaDataOf<T>();
   REQUIRE(meta1);
   REQUIRE_FALSE(meta1.GetCopyAssigner());

   auto meta2 = MetaDataOf<const T>();
   REQUIRE(meta2);
   REQUIRE_FALSE(meta2.GetCopyAssigner());
}


///                                                                           
///   Clone intents                                                           
///                                                                           
TEST_CASE_TEMPLATE("Testing clone-constructible types", T
   , EmptyType
   , AllIntentConstructible
   , AllIntentConstructibleAndAssignable
   , PartiallyIntentConstructible
   , PartiallyIntentConstructibleButImplicitly
   , CloneConstructibleButNotAssignable
   , ForcefullyPod
   , AggregateType
   , int
) {
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
   REQUIRE(meta1.GetCloneConstructor());

   auto meta2 = MetaDataOf<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2.GetCloneConstructor());

   auto meta3 = MetaDataOf<const T>();
   REQUIRE(meta3);
   REQUIRE(meta3.GetCloneConstructor());
}

TEST_CASE_TEMPLATE("Testing non-clone-constructible types", T
   //, IncompleteType // should not compile at all
   , NonDestructible
   , DestructibleType
   , PrivatelyConstructible
   , Complex
   , ContainsComplex
   , AggregateTypeComplex
   , NonIntentConstructible
   , ReferConstructibleButNotAssignable
   , MoveConstructibleButNotAssignable
   , CopyConstructibleButNotAssignable
   , AbandonConstructibleButNotAssignable
   , DisownConstructibleButNotAssignable
) {
   static_assert(not CT::CloneConstructible<T>);
   static_assert(not CT::CloneConstructible<T*>);
   static_assert(not CT::IntentConstructible<Clone, T>);
   static_assert(not CT::IntentConstructible<Clone, T*>);
   static_assert(not CT::IntentConstructibleAlt<Clone<T>>);
   static_assert(not CT::IntentConstructibleAlt<Clone<T*>>);

   auto meta1 = MetaDataOf<T>();
   REQUIRE(meta1);
   REQUIRE_FALSE(meta1.GetCloneConstructor());

   auto meta2 = MetaDataOf<const T>();
   REQUIRE(meta2);
   REQUIRE_FALSE(meta2.GetCloneConstructor());
}

TEST_CASE_TEMPLATE("Testing clone-assignable types", T
   , EmptyType
   , AllIntentConstructibleImplicit
   , AllIntentConstructibleAndAssignable
   , AggregateType
   , PartiallyIntentConstructibleButImplicitly
   , int
) {
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
   REQUIRE(meta1.GetCloneAssigner());

   auto meta2 = MetaDataOf<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2.GetCloneAssigner());

   auto meta3 = MetaDataOf<const T>();
   REQUIRE(meta3);
   REQUIRE_FALSE(meta3.GetCloneAssigner());
}

TEST_CASE_TEMPLATE("Testing non-clone-assignable types", T
   //, IncompleteType // should not compile at all
   , NonDestructible
   , DestructibleType
   , PrivatelyConstructible
   , NonIntentConstructible
   , Complex
   , ContainsComplex
   , AllIntentConstructible
   , PartiallyIntentConstructible
   , AggregateTypeComplex
   , ForcefullyPod
   , ReferConstructibleButNotAssignable
   , MoveConstructibleButNotAssignable
   , CopyConstructibleButNotAssignable
   , AbandonConstructibleButNotAssignable
   , DisownConstructibleButNotAssignable
   , CloneConstructibleButNotAssignable
) {
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

   auto meta1 = MetaDataOf<T>();
   REQUIRE(meta1);
   REQUIRE_FALSE(meta1.GetCloneAssigner());

   auto meta2 = MetaDataOf<const T>();
   REQUIRE(meta2);
   REQUIRE_FALSE(meta2.GetCloneAssigner());
}


///                                                                           
///   Disown intents                                                          
///                                                                           
TEST_CASE_TEMPLATE("Testing disown-constructible types", T
   , EmptyType
   , AllIntentConstructible
   , AllIntentConstructibleAndAssignable
   , PartiallyIntentConstructible
   , PartiallyIntentConstructibleButImplicitly
   , DisownConstructibleButNotAssignable
   , ForcefullyPod
   , AggregateType
   , int
) {
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
   REQUIRE(meta1.GetDisownConstructor());

   auto meta2 = MetaDataOf<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2.GetDisownConstructor());

   auto meta3 = MetaDataOf<const T>();
   REQUIRE(meta3);
   REQUIRE(meta3.GetDisownConstructor());
}

TEST_CASE_TEMPLATE("Testing non-disown-constructible types", T
   //, IncompleteType // should not compile at all
   , NonDestructible
   , DestructibleType
   , PrivatelyConstructible
   , Complex
   , ContainsComplex
   , AggregateTypeComplex
   , NonIntentConstructible
   , ReferConstructibleButNotAssignable
   , MoveConstructibleButNotAssignable
   , CopyConstructibleButNotAssignable
   , AbandonConstructibleButNotAssignable
   , CloneConstructibleButNotAssignable
) {
   static_assert(not CT::DisownConstructible<T>);
   static_assert(    CT::DisownConstructible<T*>);
   static_assert(not CT::IntentConstructible<Disown, T>);
   static_assert(    CT::IntentConstructible<Disown, T*>);
   static_assert(not CT::IntentConstructibleAlt<Disown<T>>);
   static_assert(    CT::IntentConstructibleAlt<Disown<T*>>);

   auto meta1 = MetaDataOf<T>();
   REQUIRE(meta1);
   REQUIRE_FALSE(meta1.GetDisownConstructor());

   auto meta2 = MetaDataOf<const T>();
   REQUIRE(meta2);
   REQUIRE_FALSE(meta2.GetDisownConstructor());
}

TEST_CASE_TEMPLATE("Testing disown-assignable types", T
   , EmptyType
   , AllIntentConstructibleImplicit
   , AllIntentConstructibleAndAssignable
   , AggregateType
   , PartiallyIntentConstructibleButImplicitly
   , int
) {
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
   REQUIRE(meta1.GetDisownAssigner());

   auto meta2 = MetaDataOf<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2.GetDisownAssigner());

   auto meta3 = MetaDataOf<const T>();
   REQUIRE(meta3);
   REQUIRE_FALSE(meta3.GetDisownAssigner());
}

TEST_CASE_TEMPLATE("Testing non-disown-assignable types", T
   //, IncompleteType // should not compile at all
   , NonDestructible
   , DestructibleType
   , PrivatelyConstructible
   , NonIntentConstructible
   , Complex
   , ContainsComplex
   , ForcefullyPod
   , AllIntentConstructible
   , PartiallyIntentConstructible
   , ReferConstructibleButNotAssignable
   , MoveConstructibleButNotAssignable
   , CopyConstructibleButNotAssignable
   , AbandonConstructibleButNotAssignable
   , DisownConstructibleButNotAssignable
   , CloneConstructibleButNotAssignable
) {
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

   auto meta1 = MetaDataOf<T>();
   REQUIRE(meta1);
   REQUIRE_FALSE(meta1.GetDisownAssigner());

   auto meta2 = MetaDataOf<const T>();
   REQUIRE(meta2);
   REQUIRE_FALSE(meta2.GetDisownAssigner());
}


///                                                                           
///   Abandon semantics                                                       
///                                                                           
TEST_CASE_TEMPLATE("Testing abandon-constructible types", T
   , EmptyType
   , DestructibleType
   , NonIntentConstructible
   , AllIntentConstructible
   , AllIntentConstructibleAndAssignable
   , PartiallyIntentConstructible
   , PartiallyIntentConstructibleButImplicitly
   , Complex
   , ContainsComplex
   , AbandonConstructibleButNotAssignable
   , MoveConstructibleButNotAssignable
   , ForcefullyPod
   , AggregateType
   , int
) {
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
   REQUIRE(meta1.GetAbandonConstructor());

   auto meta2 = MetaDataOf<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2.GetAbandonConstructor());

   auto meta3 = MetaDataOf<const T>();
   REQUIRE(meta3);
   REQUIRE(meta3.GetAbandonConstructor());
}

TEST_CASE_TEMPLATE("Testing non-abandon-constructible types", T
   //, IncompleteType // should not compile at all
   , NonDestructible
   , PrivatelyConstructible
   , ReferConstructibleButNotAssignable
   , CopyConstructibleButNotAssignable
   , DisownConstructibleButNotAssignable
   , CloneConstructibleButNotAssignable
) {
   static_assert(not CT::AbandonConstructible<T>);
   static_assert(    CT::AbandonConstructible<T*>);
   static_assert(not CT::IntentConstructible<Abandon, T>);
   static_assert(    CT::IntentConstructible<Abandon, T*>);
   static_assert(not CT::IntentConstructibleAlt<Abandon<T>>);
   static_assert(    CT::IntentConstructibleAlt<Abandon<T*>>);

   auto meta1 = MetaDataOf<T>();
   REQUIRE(meta1);
   REQUIRE_FALSE(meta1.GetAbandonConstructor());

   auto meta2 = MetaDataOf<const T>();
   REQUIRE(meta2);
   REQUIRE_FALSE(meta2.GetAbandonConstructor());
}

TEST_CASE_TEMPLATE("Testing abandon-assignable types", T
   , EmptyType
   , NonDestructible
   , DestructibleType
   , NonIntentConstructible
   , AllIntentConstructible
   , AllIntentConstructibleAndAssignable
   , PartiallyIntentConstructible
   , PartiallyIntentConstructibleButImplicitly
   , AggregateType
   , int
) {
   static_assert(    CT::AbandonAssignable<T>);
   static_assert(not CT::AbandonAssignable<T const>);
   static_assert(    CT::AbandonAssignable<T*>);
   static_assert(    CT::AbandonAssignable<T const*>);
   static_assert(    CT::IntentAssignable<Abandon, T>);
   static_assert(not CT::IntentAssignable<Abandon, T const>);
   static_assert(    CT::IntentAssignable<Abandon, T*>);
   static_assert(    CT::IntentAssignable<Abandon, T const*>);
   static_assert(    CT::IntentAssignableAlt<Abandon<T>>);
   //static_assert(not CT::IntentAssignableAlt<Abandon<T const>>); // should not compile
   static_assert(    CT::IntentAssignableAlt<Abandon<T*>>);
   static_assert(    CT::IntentAssignableAlt<Abandon<T const*>>);

   auto meta1 = MetaDataOf<T>();
   REQUIRE(meta1);
   REQUIRE(meta1.GetAbandonAssigner());

   auto meta2 = MetaDataOf<T*>();
   REQUIRE(meta2);
   REQUIRE(meta2.GetAbandonAssigner());

   auto meta3 = MetaDataOf<const T>();
   REQUIRE(meta3);
   REQUIRE_FALSE(meta3.GetAbandonAssigner());
}

TEST_CASE_TEMPLATE("Testing non-abandon-assignable types", T
   //IncompleteType, // should not compile at all
   , Complex
   , ContainsComplex
   , ForcefullyPod
   , PrivatelyConstructible
   , ReferConstructibleButNotAssignable
   , MoveConstructibleButNotAssignable
   , CopyConstructibleButNotAssignable
   , AbandonConstructibleButNotAssignable
   , DisownConstructibleButNotAssignable
   , CloneConstructibleButNotAssignable
) {
   static_assert(not CT::AbandonAssignable<T>);
   static_assert(not CT::AbandonAssignable<T const>);
   static_assert(    CT::AbandonAssignable<T*>);
   static_assert(    CT::AbandonAssignable<T const*>);
   static_assert(not CT::IntentAssignable<Abandon, T>);
   static_assert(not CT::IntentAssignable<Abandon, T const>);
   static_assert(    CT::IntentAssignable<Abandon, T*>);
   static_assert(    CT::IntentAssignable<Abandon, T const*>);
   static_assert(not CT::IntentAssignableAlt<Abandon<T>>);
   //static_assert(not CT::IntentAssignableAlt<Abandon<T const>>); // should not compile
   static_assert(    CT::IntentAssignableAlt<Abandon<T*>>);
   static_assert(    CT::IntentAssignableAlt<Abandon<T const*>>);

   auto meta1 = MetaDataOf<T>();
   REQUIRE(meta1);
   REQUIRE_FALSE(meta1.GetAbandonAssigner());

   auto meta2 = MetaDataOf<const T>();
   REQUIRE(meta2);
   REQUIRE_FALSE(meta2.GetAbandonAssigner());
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

TEST_CASE_TEMPLATE("Testing DeintCast (non-moving)", TestType
   , const int&
   , Copy<int>
   , Refer<int>
   , Disown<int>
   , Clone<int>
) {
   const int* value = new int {656};
   const TestType i {*value};
   static_assert(::std::same_as<decltype(DeintCast(i)), const int&>);
   REQUIRE(DeintCast(i) == 656);
   delete value;
}

TEST_CASE_TEMPLATE("Testing DeintCast (moving)", TestType
   , int&&
   , Move<int>
   , Abandon<int>
) {
   int* value = new int {656};
   TestType i {static_cast<int&&>(*value)};
   static_assert(::std::same_as<decltype(DeintCast(FWD(i))), int&&>);
   REQUIRE(DeintCast(FWD(i)) == 656);
   delete value;
}
