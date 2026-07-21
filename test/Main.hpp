///                                                                           
/// Langulus::Tester                                                          
/// Copyright (c) 2025 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include <Langulus/Core.hpp>
#include <Langulus/Except.hpp>
#include <Langulus/MetaOf.hpp>
#define DOCTEST_CONFIG_VOID_CAST_EXPRESSIONS
#include <doctest/doctest.h>

/*#define BOOL_TYPES            bool
#define CHARACTER_TYPES       char, wchar_t, char8_t, char16_t, char32_t
#define UNSIGNED_TYPES        uint8_t, uint16_t, uint32_t, uint64_t
#define REAL_TYPES            float, double
#define SIGNED_INTEGER_TYPES  int8_t, int16_t, int32_t, int64_t
#define INTEGER_TYPES         UNSIGNED_TYPES, SIGNED_INTEGER_TYPES
#define SIGNED_TYPES          SIGNED_INTEGER_TYPES, REAL_TYPES
#define ALL_TYPES             UNSIGNED_TYPES, SIGNED_TYPES, CHARACTER_TYPES, BOOL_TYPES*/

REGISTER_EXCEPTION_TRANSLATOR(::Langulus::Exception& e) {
   #if LANGULUS(DEBUG)
      return e.mMessage;
   #else
      (void)e;
      return ::Langulus::Exception::DefaultMessage;
   #endif
}

namespace doctest
{
   template<>
   struct StringMaker<::Langulus::RTTI::DMeta> {
      static String convert(::Langulus::RTTI::DMeta const& value) {
         return toString(static_cast<::std::string>(value.GetName()));
      }
   };

   template<>
   struct StringMaker<::Langulus::pot_t> {
      static String convert(::Langulus::pot_t const& value) {
         return toString(static_cast<size_t>(value));
      }
   };
}

#if LANGULUS(BENCHMARK)
   #include <Langulus/Profiler.hpp>

   constexpr int BenchmarkWarmupCycles  =  100;
   constexpr int BenchmarkMeasureCycles = 1000;
#endif

#if LANGULUS(BENCHMARK)
   #define prevent_optimization(a)
#else
   #define prevent_optimization(a) volatile auto a##_optimization_prevention = a; (void) a##_optimization_prevention;
#endif