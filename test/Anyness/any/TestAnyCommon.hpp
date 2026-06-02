///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../../Main.hpp"
#include "../../TestTypes/ScopedElement.hpp"
#include "../../TestTypes/ReferencedType.hpp"
#include "../../TestTypes/CommonTypes.hpp"
#include <Langulus/Anyness/Any.hpp>
#include <Langulus/Anyness/TAny.hpp>
#include <Langulus/Anyness/SerializeText.hpp>
#include <ranges>
#include <any>

using namespace Langulus;
using namespace Anyness;

#if LANGULUS(BENCHMARK)
   /// Perform a persistent benchmark across build and verify performance     
   #define BenchmarkAny(func, tolerance, my_init, my) { \
      const auto token = ::std::string("Test/") + static_cast<::std::string>(func) + " |" + static_cast<::std::string>(NameOf<T>()) + "|"; \
      volatile int i = 0; \
      for (; i < BenchmarkWarmupCycles; i += 1) { \
         my_init; \
         my; \
      } \
      for (; i < BenchmarkWarmupCycles + BenchmarkMeasureCycles; i += 1) { \
         my_init; \
         { \
            CTRACK_NAME_PERSIST(token.c_str()); \
            my; \
         } \
      } \
      auto results = ctrack::result_get_detail_table(); \
      results.check_highscore(tolerance); \
   }

   /// Perform two persistent benchmarks across builds - one for Any and      
   /// one for std::any. Make sure they don't deviate a lot.                  
   #define BenchmarkAnyStd(func, tolerance_highscore, tolerance, my_init, my, theirs_init, theirs) { \
      const auto token = ::std::string("Test/") + static_cast<::std::string>(func) + " |" + static_cast<::std::string>(NameOf<T>()) + "|"; \
      volatile int i = 0; \
      for (; i < BenchmarkWarmupCycles; i += 1) { \
         my_init; \
         my; \
      } \
      for (; i < BenchmarkWarmupCycles + BenchmarkMeasureCycles; i += 1) { \
         my_init; \
         { \
            CTRACK_NAME_PERSIST(token.c_str()); \
            my; \
         } \
      } \
      i = 0; \
      const auto token_std = ::std::string("Test/") + static_cast<::std::string>(func) + " |std::any|"; \
      for (; i < BenchmarkWarmupCycles; i += 1) { \
         theirs_init; \
         theirs; \
      } \
      for (; i < BenchmarkWarmupCycles + BenchmarkMeasureCycles; i += 1) { \
         theirs_init; \
         { \
            CTRACK_NAME(token_std.c_str()); \
            theirs; \
         } \
      } \
      auto results = ctrack::result_get_detail_table(); \
      results.check_highscore(tolerance_highscore); \
      REQUIRE(results.check_same(token.c_str(), token_std.c_str(), tolerance)); \
   }
#else
   #define BenchmarkAny(func, tolerance, my_init, my)
   #define BenchmarkAnyStd(func, tolerance_highscore, tolerance, my_init, my, theirs_init, theirs)
#endif

#if LANGULUS_FEATURE(MANAGED_MEMORY)
   #include "../../TestTypes/PackedPointers.hpp"
#endif

template<class T, class COMPARE_WITH>
void Common_GapTest() {
   alignas(T) char unininitialized[sizeof(T)];
   memset(unininitialized, 254, sizeof(unininitialized));
   new (unininitialized) T {};
   for (auto b : unininitialized) {
      REQUIRE(b != 254);
   }
   Logger::Info("Size of ", NameOf<COMPARE_WITH>(), " container is: ",
      sizeof(COMPARE_WITH), " bytes");
   auto s = Logger::Section("Size of ", NameOf<T>(), " container is: ",
      sizeof(T), " bytes");

   size_t size = 0;
   size_t stack_size = 0;
   size_t heap_size = 0;
   size_t heap_size_per_element = 0;
   size_t heap_size_per_indirection = 0;
   size_t heap_size_per_element_times_indirection = 0;
   T::ComponentList::ForEach([&]<class C> {
      if constexpr (requires { typename C::StackRequest; }) {
         // Scan all stack requests                                     
         using R = typename C::StackRequest;
         if constexpr (CT::NotVoid<R>) {
            Logger::Info(NameOf<C>(), " component is: ", sizeof(C),
               " bytes (reserves ", sizeof(R), " bytes on the stack)");
            stack_size += sizeof(R);
         }
      }

      if constexpr (requires { typename C::HeapRequest; }) {
         // Scan all heap requests                                      
         using R = typename C::HeapRequest;
         if constexpr (CT::NotVoid<R>) {
            if constexpr (requires { R::AllocatedPerIndirection; }) {
               if constexpr (requires { R::Type::AllocatedPerElement; }) {
                  Logger::Info(NameOf<C>(), " component is: ", sizeof(C),
                     " bytes (reserves ", sizeof(typename R::Type::Type), 
                     " bytes per indirection per element on the heap footer)");
                  heap_size_per_element_times_indirection += sizeof(typename R::Type::Type);
               }
               else {
                  Logger::Info(NameOf<C>(), " component is: ", sizeof(C),
                     " bytes (reserves ", sizeof(typename R::Type), 
                     " bytes per indirection on the heap footer)");
                  heap_size_per_indirection += sizeof(typename R::Type);
               }
            }
            else if constexpr (requires { R::AllocatedPerElement; }) {
               if constexpr (requires { R::Type::AllocatedPerIndirection; }) {
                  Logger::Info(NameOf<C>(), " component is: ", sizeof(C),
                     " bytes (reserves ", sizeof(typename R::Type::Type),
                     " bytes per indirection per element on the heap footer)");
                  heap_size_per_element_times_indirection += sizeof(typename R::Type::Type);
               }
               else {
                  Logger::Info(NameOf<C>(), " component is: ", sizeof(C),
                     " bytes (reserves ", sizeof(typename R::Type),
                     " bytes per element on the heap footer)");
                  heap_size_per_element += sizeof(typename R::Type);
               }
            }
            else {
               Logger::Info(NameOf<C>(), " component is: ", sizeof(C),
                  " bytes (reserves ", sizeof(R), 
                  " bytes on the heap header)");
               heap_size += sizeof(R);
            }
         }
      }

      size += sizeof(C);
   });

   Logger::Info("-----------------------------------------");
   Logger::Info("For a total of ", size,
      " bytes in components (should be optimized-out as empty bases)");
   Logger::Info("For a total of ", stack_size,
      " bytes on the stack");
   Logger::Info("For a total of ", heap_size,
      " bytes on the heap header");
   Logger::Info("For a total of ", heap_size_per_element,
      " bytes per element on the heap footer");
   Logger::Info("For a total of ", heap_size_per_indirection,
      " bytes per indirection on the heap footer");
   Logger::Info("For a total of ", heap_size_per_element_times_indirection,
      " bytes per indirection per element on the heap footer");
}

namespace doctest
{
   template<>
   struct StringMaker<Any> {
      static String convert(Any const& value) {
         return toString(static_cast<::std::string>(
            NameOf<Any>() + "(" + Convert<Text>(value) + ")"
         ));
      }
   };

   template<class T>
   struct StringMaker<TAny<T>> {
      static String convert(TAny<T> const& value) {
         return toString(static_cast<::std::string>(
            NameOf<TAny<T>>() + "(" + Convert<Text>(value) + ")"
         ));
      }
   };
}

template<class E, CT::Container C> requires CT::NoIntent<C>
void Any_Helper_TestType(const C& any) {
   if constexpr (CT::Void<E>) {
      REQUIRE(    any.template IsSame<int>());
      REQUIRE(    any.template IsExact<int>());
      REQUIRE(    any.template Is<int>());
      REQUIRE(not any.IsSparse());
      REQUIRE(not any.IsDeep());
      REQUIRE(    any.GetType() == MetaDataOf<int>());
   }
   else {
      REQUIRE(    any.template IsSame<E>());
      REQUIRE(    any.template IsExact<E>());
      REQUIRE(    any.template Is<E>());
      REQUIRE(    any.IsSparse() == CT::Sparse<E>);
      REQUIRE(    any.IsDeep() == CT::Deep<E>);
      REQUIRE(    any.GetType() == MetaDataOf<E>());      
   }
   
   REQUIRE(any.IsTyped());
}

template<class LHS, class RHS> requires (CT::Container<LHS, RHS> and CT::NoIntent<LHS, RHS>)
void Any_Helper_TestSame(const LHS& lhs, const RHS& rhs) {
   REQUIRE(lhs.GetCount() == rhs.GetCount());
   if (not lhs.IsEmpty())
      REQUIRE(lhs.GetRaw() == rhs.GetRaw()); // not really a requirement when containers are empty
   REQUIRE(lhs.IsExact(rhs.GetType()));
   REQUIRE(lhs == rhs);
   REQUIRE(lhs.IsDeep() == rhs.IsDeep());
   REQUIRE(lhs.IsConstant() == rhs.IsConstant());
   REQUIRE(lhs.GetUnconstrainedState() == rhs.GetUnconstrainedState());
}


///                                                                           
/// Possible state test implementations                                       
template<class E, CT::Container C> requires CT::NoIntent<C>
void Common_CheckState_Default(const C& any, bool typed = false) {
   if constexpr (CT::Typed<C>) {
      static_assert(Exact<TypeOf<C>, E>);
      Any_Helper_TestType<E>(any);
   }
   else if (not typed) {
      REQUIRE_FALSE(any.IsTyped());
      REQUIRE      (any.GetType() == nullptr);
      REQUIRE_FALSE(any.IsSparse());
      REQUIRE_FALSE(any.IsDeep());
   }
   else Any_Helper_TestType<E>(any);

   REQUIRE      (any.IsDefaultState());
   REQUIRE      (any.IsTypeConstrained() == CT::Typed<C>);
   REQUIRE      (any.IsConstant());
   REQUIRE_FALSE(any.IsValid());
   REQUIRE_FALSE(any.GetAllocation());
   REQUIRE      (any.IsEmpty());
   REQUIRE      (any.GetCount() == 0);
   REQUIRE      (any.GetReserved() == 0);
   REQUIRE      (any.GetUses() == 0);
   //REQUIRE      (any.GetRaw() == nullptr); // not really a requirement for the default state. Count being 0 is enough in most cases
   REQUIRE_FALSE(any);
   REQUIRE      (not any);
}

template<class E, CT::Container C> requires CT::NoIntent<C>
void Any_CheckState_Default(const C& any, bool typed = false) {
   Common_CheckState_Default<E>(any, typed);

   if constexpr (requires { any.GetState(); }) {
      REQUIRE_FALSE(any.IsMissing());
      REQUIRE_FALSE(any.IsFuture());
      REQUIRE_FALSE(any.IsPast());      
   }
}

template<class E, CT::Container C> requires CT::NoIntent<C>
void Any_CheckState_OwnedEmpty(const C& any) {
   Any_Helper_TestType<E>(any);

   REQUIRE      (any.IsTypeConstrained() == CT::Typed<C>);
   REQUIRE      (any.IsConstant() == CT::Constant<E>);
   REQUIRE_FALSE(any.IsValid());
   REQUIRE      (any.GetAllocation());
   REQUIRE      (any.IsEmpty());
   REQUIRE      (any.GetCount() == 0);
   REQUIRE      (any.GetReserved() > 0);
   REQUIRE      (any.GetUses() == 1);
   //REQUIRE      (any.GetRaw() == nullptr); // not really a requirement for the owned-empty state. Count being 0 is enough in most cases
   REQUIRE_FALSE(any);
   REQUIRE      (not any);
}

template<class E, CT::Container C> requires CT::NoIntent<C>
void Any_CheckState_OwnedFull(const C& any) {
   Any_Helper_TestType<E>(any);

   REQUIRE      (any.IsTypeConstrained() == CT::Typed<C>);
   REQUIRE      (any.IsConstant() == CT::Constant<E>);
   REQUIRE      (any.IsValid());
   REQUIRE      (any.GetAllocation());
   REQUIRE_FALSE(any.IsEmpty());
   REQUIRE      (any.GetCount() > 0);
   REQUIRE      (any.GetReserved() > 0);
   REQUIRE      (any.GetUses() > 0);
   REQUIRE      (any.GetRaw());
   REQUIRE      (any);
   REQUIRE_FALSE(not any);
}

template<class E, CT::Container C> requires CT::NoIntent<C>
void Any_CheckState_DisownedFull(const C& any) {
   Any_Helper_TestType<E>(any);

   REQUIRE      (any.IsTypeConstrained() == CT::Typed<C>);
   REQUIRE      (any.IsConstant());
   REQUIRE      (any.IsValid());
   REQUIRE_FALSE(any.GetAllocation());
   REQUIRE_FALSE(any.IsEmpty());
   REQUIRE      (any.GetCount() > 0);
   REQUIRE      (any.GetReserved() == 0);
   REQUIRE      (any.GetUses() == 0);
   REQUIRE      (any.GetRaw());
   REQUIRE      (any);
   REQUIRE_FALSE(not any);
}

template<class E, CT::Container C> requires CT::NoIntent<C>
void Any_CheckState_Abandoned(const C& any) {
   REQUIRE_FALSE(any.GetAllocation());
}

template<CT::Container T, CT::Intent I> requires CT::NoIntent<T>
void Any_CheckState_ContainsOne(T const& pack, I&& e_with_intent, int uses = 1) {
   auto& e = e_with_intent.what;
   using E = typename Decay<Deint<I>>::Type;
   REQUIRE(pack.GetCount() == 1);
   REQUIRE(pack.GetUses() == uses);
   REQUIRE(pack.GetReserved() >= (uses ? 1 : 0));

   if constexpr (not CT::CustomPointer<E> or not CT::TypeErased<T>)
      REQUIRE(pack.template As<Decay<E>>() == DenseCast(*e));
   else
      REQUIRE_THROWS(pack.template As<Decay<E>>() == DenseCast(*e));

   if constexpr (CT::Cloned<I> and CT::Sparse<E>) {
      REQUIRE(pack.template As<E>() != *e);
      REQUIRE((*pack.template As<E*>()) != *e);
      REQUIRE(*pack.template GetRawAs<E>() != *e);
   }
   else {
      REQUIRE(pack.template As<E>() == *e);
      REQUIRE((*pack.template As<E*>()) == *e);
      REQUIRE(*pack.template GetRawAs<E>() == *e);
   }

   if constexpr (CT::OwnedDeep<T>) {
      if constexpr (CT::Dense<E>)
         REQUIRE(pack.GetEntries() == nullptr);
      else if (uses) {
         REQUIRE(pack.GetEntries() != nullptr);

         if constexpr (not CT::Disowned<I>) {
            for (size_t i = 0; i < IndirectsOf<E>; ++i) {
               if constexpr (CT::Cloned<I>)
                  REQUIRE(pack.GetEntries()[i] != e.entries[i + 1]);
               else
                  REQUIRE(pack.GetEntries()[i] == e.entries[i + 1]);
            }
         }
         else {
            for (size_t i = 0; i < IndirectsOf<E>; ++i)
               REQUIRE(pack.GetEntries()[i] == nullptr);
         }
      }
   }

   if constexpr (CT::TypeErased<T>) {
      REQUIRE_THROWS(pack.template As<float>() == 0.0f);
      REQUIRE_THROWS(pack.template As<float*>() == nullptr);
   }
}

template<CT::Container T, class E> requires CT::NoIntent<T>
void Any_Helper_CompareOne(const T& pack, const E& e) {
   if constexpr (CT::TypeErased<T>) {
      REQUIRE(pack.CompareOne(e) == Compared::Equal);
      REQUIRE(pack.CompareOneEqual(e) == true);
   }
   else {
      REQUIRE(pack.CompareOne(e) == ::std::partial_ordering::equivalent);
      REQUIRE(pack.CompareOneEqual(e) == true);
   }

   if constexpr (CT::Deep<E> and LANGULUS(SAFE))
      REQUIRE_THROWS(pack == e);
   else
      REQUIRE_NOTHROW(pack == e);
}
