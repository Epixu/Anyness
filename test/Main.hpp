///                                                                           
/// Langulus::Tester                                                          
/// Copyright (c) 2025 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           

/// INTENTIONALLY NOT GUARDED                                                 
/// Include this file once in each test cpp file, after all other headers     
// ReSharper disable once CppMissingIncludeGuard
#ifdef TWOBLUECUBES_SINGLE_INCLUDE_CATCH_HPP_INCLUDED
   #error "Catch has already been included prior to this header"
#endif

#include <Langulus/Core.hpp>
#include <Langulus/Except.hpp>


#if LANGULUS(BENCHMARK)
   #define CATCH_CONFIG_ENABLE_BENCHMARKING
#endif

#include <catch2/catch.hpp>

#define BOOL_TYPES            bool
#define CHARACTER_TYPES       char, wchar_t, char8_t, char16_t, char32_t
#define UNSIGNED_TYPES        uint8_t, uint16_t, uint32_t, uint64_t
#define REAL_TYPES            float, double
#define SIGNED_INTEGER_TYPES  int8_t, int16_t, int32_t, int64_t
#define INTEGER_TYPES         UNSIGNED_TYPES, SIGNED_INTEGER_TYPES
#define SIGNED_TYPES          SIGNED_INTEGER_TYPES, REAL_TYPES
#define ALL_TYPES             UNSIGNED_TYPES, SIGNED_TYPES, CHARACTER_TYPES, BOOL_TYPES

CATCH_TRANSLATE_EXCEPTION(::Langulus::Exception& e) {
   #if LANGULUS(DEBUG)
      return e.mMessage;
   #else
      (void)e;
      return ::Langulus::Exception::DefaultMessage;
   #endif
}
