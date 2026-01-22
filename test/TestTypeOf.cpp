///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "Main.hpp"
#include <Langulus/TypeOf.hpp>
#include <string_view>
#include <array>
#include <vector>

using namespace Langulus;


///                                                                           
/// CT::Typed / CT::Untyped                                                   
///                                                                           
namespace
{
   template<class T>
   struct SheddableType {
      using CTTI_Sheddable = T;
      using CTTI_Typed = T;

      T instance;

      SheddableType(T t) : instance {FWD(t)} {}
   };

   template<class T>
   struct SheddableTypeCastableExplicit : SheddableType<T> {
      using SheddableType<T>::SheddableType;
      using SheddableType<T>::instance;
      explicit operator T () noexcept { return FWD(instance); }
      explicit operator T () const noexcept { return FWD(const_cast<SheddableTypeCastableExplicit<T>*>(this)->instance); }
   };

   template<class T>
   struct SheddableTypeCastableImplicit : SheddableType<T> {
      using SheddableType<T>::SheddableType;
      using SheddableType<T>::instance;
      operator T () noexcept { return FWD(instance); }
      operator T () const noexcept { return FWD(const_cast<SheddableTypeCastableImplicit<T>*>(this)->instance); }
   };

   template<class T>
   struct SheddableTypeCastableUsingMethod : SheddableType<T> {
      using SheddableType<T>::SheddableType;
      using SheddableType<T>::instance;
      auto TypedCast()       noexcept -> T&       { return instance; }
      auto TypedCast() const noexcept -> T const& { return instance; }
   };

   struct CustomTypedType { using CTTI_Typed = int; };
   struct CustomTypedTypeDerived : CustomTypedType { };
   struct CustomUntypedType : CustomTypedType { using CTTI_Typed = void; };
   enum TypedEnum : int64_t {one1, two2};
   enum class TypedEnumClass : int64_t {one1, two2};
   struct IncompleteType;
}

TEST_CASE_TEMPLATE("Testing typed type", TestType
   , std::vector<bool>
   , std::string_view
   , std::array<double, 5>
   , TypedEnum
   , TypedEnumClass
   , CustomTypedType
   , CustomTypedTypeDerived
   , SheddableType<TypedEnum>
   , SheddableType<int>
) {
   static_assert(    CT::Typed<TestType>);
   static_assert(not CT::Untyped<TestType>);
}

TEST_CASE_TEMPLATE("Testing untyped type", TestType
   , CustomUntypedType
   //, IncompleteType // shouldn't compile
   , void, int
) {
   static_assert(not CT::Typed<TestType>);
   static_assert(    CT::Untyped<TestType>);
}

//static_assert(CT::Typed<>); // shouldn't compile at all
static_assert(    CT::Typed<std::vector<bool>, CustomTypedType, TypedEnum>);
static_assert(not CT::Typed<std::vector<bool>, CustomTypedType, int>);

//static_assert(CT::Untyped<>); // shouldn't compile at all
static_assert(    CT::Untyped<CustomUntypedType, void, int>);
static_assert(not CT::Untyped<CustomUntypedType, void, TypedEnum>);


///                                                                           
/// TypeOf                                                                    
///                                                                           
TEST_CASE("Testing TypeOf") {
   static_assert(::std::same_as<TypeOf<SheddableType<int>>, int>);
   static_assert(::std::same_as<TypeOf<SheddableType<int&>>, int&>);
   static_assert(::std::same_as<TypeOf<SheddableType<int const* const>>, int const* const>);
   static_assert(::std::same_as<TypeOf<CustomTypedType>, int>);
   static_assert(::std::same_as<TypeOf<int>, void>);
   static_assert(::std::same_as<TypeOf<void>, void>);
   static_assert(::std::same_as<TypeOf<::std::nullptr_t>, void>);
   static_assert(::std::same_as<TypeOf<int&>, void>);
   static_assert(::std::same_as<TypeOf<volatile int const* const&>, void>);
   static_assert(::std::same_as<TypeOf<TypedEnum>, int64_t>);
   static_assert(::std::same_as<TypeOf<CustomUntypedType>, void>);
   static_assert(::std::same_as<TypeOf<std::vector<bool>>, bool>);
   static_assert(::std::same_as<TypeOf<std::string_view>, char>);
   static_assert(::std::same_as<TypeOf<std::array<double, 5>>, double>);
   //static_assert(::std::same_as<TypeOf<IncompleteType>, void>); // shouldn't compile
}


///                                                                           
/// TypedCast                                                                 
///                                                                           
TEST_CASE_TEMPLATE("Testing TypedCast", TestType
   , int
   , int&&
   , const int&
   , SheddableTypeCastableExplicit<int>
   , SheddableTypeCastableExplicit<int&&>
   , SheddableTypeCastableExplicit<const int&>
   , SheddableTypeCastableImplicit<int>
   , SheddableTypeCastableImplicit<int&&>
   , SheddableTypeCastableImplicit<const int&>
   , SheddableTypeCastableUsingMethod<const int&>
   , const SheddableTypeCastableExplicit<int>
   , const SheddableTypeCastableExplicit<int&&>
   , const SheddableTypeCastableExplicit<const int&>
   , const SheddableTypeCastableImplicit<int>
   , const SheddableTypeCastableImplicit<int&&>
   , const SheddableTypeCastableImplicit<const int&>
   , const SheddableTypeCastableUsingMethod<const int&>
) {
   int value = 656;
   TestType i {::std::move(value)};

   if constexpr (CT::Typed<TestType>)
      static_assert(::std::same_as<decltype(TypedCast(i)), TypeOf<TestType>>);
   else
      static_assert(::std::same_as<decltype(TypedCast(i)), TestType&>);
}


///                                                                           
/// ShedCast                                                                  
///                                                                           
TEST_CASE_TEMPLATE("Testing ShedCast", TestType
   , int
   , int&&
   , const int&
   , SheddableTypeCastableExplicit<int>
   , SheddableTypeCastableExplicit<int&&>
   , SheddableTypeCastableExplicit<const int&>
   , SheddableTypeCastableImplicit<int>
   , SheddableTypeCastableImplicit<int&&>
   , SheddableTypeCastableImplicit<const int&>
   , SheddableTypeCastableUsingMethod<SheddableTypeCastableExplicit<int>>
   , const SheddableTypeCastableExplicit<int>
   , const SheddableTypeCastableExplicit<int&&>
   , const SheddableTypeCastableExplicit<const int&>
   , const SheddableTypeCastableImplicit<int>
   , const SheddableTypeCastableImplicit<int&&>
   , const SheddableTypeCastableImplicit<const int&>
   , const SheddableTypeCastableUsingMethod<const SheddableTypeCastableExplicit<const int&>&>
) {
   int value = 656;
   TestType i {::std::move(value)};

   static_assert(not CT::Sheddable<decltype(ShedCast(i))>);
}


///                                                                           
/// SparseCast                                                                
///                                                                           
TEST_CASE_TEMPLATE("Testing SparseCast", TestType
   , int
   , int&&
   , const int&
   , const int* const&
   , const int* const* const&
   , const int* const* const* const&
   , const int* const* const*
   //, SheddableTypeCastableExplicit<int>    // shouldn't compile, can't get a pointer out of rvalue
   //, SheddableTypeCastableExplicit<int&&>  // shouldn't compile, can't get a pointer out of rvalue
   , SheddableTypeCastableExplicit<const int&>
   , SheddableTypeCastableExplicit<const int* const&>
   , SheddableTypeCastableExplicit<const int* const* const&>
   , SheddableTypeCastableExplicit<const int* const* const* const&>
   , SheddableTypeCastableExplicit<const int* const* const*>
   , SheddableTypeCastableUsingMethod<int>         // compiles, because method in test always returns a reference
   //, const SheddableTypeCastableExplicit<int>    // shouldn't compile, because operator returns temporary
   //, const SheddableTypeCastableExplicit<int&&>  // shouldn't compile, can't get a pointer out of rvalue
   , const SheddableTypeCastableExplicit<const int&>
   , const SheddableTypeCastableExplicit<const int* const&>
   , const SheddableTypeCastableExplicit<const int* const* const&>
   , const SheddableTypeCastableExplicit<const int* const* const* const&>
   , const SheddableTypeCastableExplicit<const int* const* const*>
   , const SheddableTypeCastableUsingMethod<int>   // compiles, because method in test always returns a reference
) {
   int value = 656;

   if constexpr (CT::Dense<TestType>) {
      TestType i {::std::move(value)};

      static_assert(    CT::Sparse   <decltype(SparseCast(i))>);
      static_assert(not CT::Sheddable<decltype(SparseCast(i))>);
   }
   else if constexpr (IndirectsOf<TestType> == 1) {
      TestType i {&value};

      static_assert(    CT::Sparse   <decltype(SparseCast(i))>);
      static_assert(not CT::Sheddable<decltype(SparseCast(i))>);
   }
   else if constexpr (IndirectsOf<TestType> == 2) {
      int* vp = &value;
      TestType i {&vp};

      static_assert(    CT::Sparse   <decltype(SparseCast(i))>);
      static_assert(not CT::Sheddable<decltype(SparseCast(i))>);
   }
   else if constexpr (IndirectsOf<TestType> == 3) {
      int* vp = &value;
      int** vpp = &vp;
      TestType i {&vpp};

      static_assert(    CT::Sparse   <decltype(SparseCast(i))>);
      static_assert(not CT::Sheddable<decltype(SparseCast(i))>);
   }
   else static_assert(false, "Unhandled case");
}


///                                                                           
/// DenseCast                                                                 
///                                                                           
TEST_CASE_TEMPLATE("Testing DenseCast", TestType
   , int
   , int&&
   , const int&
   , const int* const&
   , const int* const* const&
   , const int* const* const* const&
   , const int* const* const*
   , SheddableTypeCastableExplicit<int>
   , SheddableTypeCastableExplicit<int&&>
   , SheddableTypeCastableExplicit<const int&>
   , SheddableTypeCastableExplicit<const int* const&>
   , SheddableTypeCastableExplicit<const int* const* const&>
   , SheddableTypeCastableExplicit<const int* const* const* const&>
   , SheddableTypeCastableExplicit<const int* const* const*>
   , const SheddableTypeCastableExplicit<int>
   , const SheddableTypeCastableExplicit<int&&>
   , const SheddableTypeCastableExplicit<const int&>
   , const SheddableTypeCastableExplicit<const int* const&>
   , const SheddableTypeCastableExplicit<const int* const* const&>
   , const SheddableTypeCastableExplicit<const int* const* const* const&>
   , const SheddableTypeCastableExplicit<const int* const* const*>
) {
   int value = 656;

   if constexpr (CT::Dense<TestType>) {
      TestType i {::std::move(value)};

      static_assert(    CT::Dense    <decltype(DenseCast(i))>);
      static_assert(not CT::Sheddable<decltype(DenseCast(i))>);
   }
   else if constexpr (IndirectsOf<TestType> == 1) {
      TestType i {&value};

      static_assert(    CT::Dense    <decltype(DenseCast(i))>);
      static_assert(not CT::Sheddable<decltype(DenseCast(i))>);
   }
   else if constexpr (IndirectsOf<TestType> == 2) {
      int* vp = &value;
      TestType i {&vp};

      static_assert(    CT::Dense    <decltype(DenseCast(i))>);
      static_assert(not CT::Sheddable<decltype(DenseCast(i))>);
   }
   else if constexpr (IndirectsOf<TestType> == 3) {
      int* vp = &value;
      int** vpp = &vp;
      TestType i {&vpp};

      static_assert(    CT::Dense    <decltype(DenseCast(i))>);
      static_assert(not CT::Sheddable<decltype(DenseCast(i))>);
   }
   else static_assert(false, "Unhandled case");
}
