///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include <cstdint>


/// Sorry, Langulus is designed for at least C++23                            
//#if __cplusplus < 202300L// and not defined(_MSC_VER)
//   #error Langulus requires at least a C++23 compliant compiler in order to build
//#endif

/// These macros seem evil, but read this:                                    
/// https://www.foonathan.net/2020/09/move-forward/                           
/// static_cast to rvalue reference                                           
#define MOV(...) static_cast<std::remove_reference_t<decltype(__VA_ARGS__)>&&>(__VA_ARGS__)

/// static_cast to identity                                                   
/// The extra && aren't necessary as discussed above, but make it more robust 
/// in case it's used with a non-reference.                                   
#define FWD(...) static_cast<decltype(__VA_ARGS__)&&>(__VA_ARGS__)

/// Safe mode enables assumption checks all over the code                     
/// High overhead, usually enabled only when testing in debug builds          
#if defined(LANGULUS_SAFE_MODE) or defined(LANGULUS_ASSERTION_LEVEL)
   #ifdef LANGULUS_ASSERTION_LEVEL
      #define LANGULUS_SAFE() LANGULUS_ASSERTION_LEVEL
   #else
      #define LANGULUS_SAFE() 2
   #endif
#else
   #define LANGULUS_SAFE() 0
#endif

#if LANGULUS_SAFE()
   #define IF_SAFE(a)      a
   #define IF_UNSAFE(a)    
#else
   #define IF_SAFE(a)      
   #define IF_UNSAFE(a)    a
#endif

/// Adds the appropriate noexcept specifiers for functions that throw only    
/// in safe builds                                                            
#define has_assumptions IF_UNSAFE(noexcept)

/// Testing mode exposes some otherwise private functions                     
/// Overhead is unlikely                                                      
#ifdef LANGULUS_TESTING
   #undef LANGULUS_TESTING
   #define LANGULUS_TESTING() 1
   #define IF_LANGULUS_TESTING(a)         a
   #define IF_NOT_LANGULUS_TESTING(a)     LANGULUS(NOOP)
#else
   #define LANGULUS_TESTING() 0
   #define IF_LANGULUS_TESTING(a)         LANGULUS(NOOP)
   #define IF_NOT_LANGULUS_TESTING(a)     a
#endif

/// Benchmarking                                                              
/// Tests will become radically slower                                        
#ifdef LANGULUS_BENCHMARK
   #undef LANGULUS_BENCHMARK
   #define LANGULUS_BENCHMARK() 1
   #define IF_LANGULUS_BENCHMARK(a)       a
   #define IF_NOT_LANGULUS_BENCHMARK(a)   LANGULUS(NOOP)
#else
   #define LANGULUS_BENCHMARK() 0
   #define IF_LANGULUS_BENCHMARK(a)       LANGULUS(NOOP)
   #define IF_NOT_LANGULUS_BENCHMARK(a)   a
#endif

/// Paranoid mode introduces overhead, but zeroes any freed memory            
#ifdef LANGULUS_PARANOIA
   #undef LANGULUS_PARANOIA
   #define LANGULUS_PARANOID() 1
   #define IF_LANGULUS_PARANOID(a)        a
   #define IF_NOT_LANGULUS_PARANOID(a)    LANGULUS(NOOP)
#else
   #define LANGULUS_PARANOID() 0
   #define IF_LANGULUS_PARANOID(a)        LANGULUS(NOOP)
   #define IF_NOT_LANGULUS_PARANOID(a)    a
#endif

/// Detect debug builds                                                       
#if defined(LANGULUS_DEBUGGING) or (not defined(NDEBUG) \
                                or defined(DEBUG) \
                                or defined(_DEBUG) \
                                or defined(CB_DEBUG) \
                                or defined(QT_QML_DEBUG))
   #undef LANGULUS_DEBUGGING
   #define LANGULUS_DEBUG()   1
   #define DEBUGGERY(a)       a
#else
   #define LANGULUS_DEBUG()   0
   #define DEBUGGERY(a)       LANGULUS(NOOP)
#endif

/// Reflections will be registered in a centralized location, allowing for    
/// runtime type modification. Meta primitives will always be in the same     
/// place in memory regardless of translation unit, which significantly       
/// speeds up meta definition comparisons.                                    
/// Naming collisions will be detected upon type registration                 
/// Gives a significant overhead on program launch, no dependencies           
#ifdef LANGULUS_FEATURE_MANAGED_REFLECTION
   #undef LANGULUS_FEATURE_MANAGED_REFLECTION
   #define LANGULUS_FEATURE_MANAGED_REFLECTION()   1
   #define IF_LANGULUS_MANAGED_REFLECTION(a)       a
   #define IF_NOT_LANGULUS_MANAGED_REFLECTION(a)   LANGULUS(NOOP)
#else
   #define LANGULUS_FEATURE_MANAGED_REFLECTION()   0
   #define IF_LANGULUS_MANAGED_REFLECTION(a)       LANGULUS(NOOP)
   #define IF_NOT_LANGULUS_MANAGED_REFLECTION(a)   a
#endif

/// Memory allocations will be pooled, authority will be tracked,             
/// memory will be reused whenever possible, and you can also tweak           
/// runtime allocation strategies on per-type basis                           
/// Significantly improves performance, no dependencies                       
#ifdef LANGULUS_FEATURE_MANAGED_MEMORY
   #undef LANGULUS_FEATURE_MANAGED_MEMORY
   #define LANGULUS_FEATURE_MANAGED_MEMORY()    1
   #define IF_LANGULUS_MANAGED_MEMORY(a)        a
   #define IF_NOT_LANGULUS_MANAGED_MEMORY(a)    LANGULUS(NOOP)
#else
   #define LANGULUS_FEATURE_MANAGED_MEMORY()    0
   #define IF_LANGULUS_MANAGED_MEMORY(a)        LANGULUS(NOOP)
   #define IF_NOT_LANGULUS_MANAGED_MEMORY(a)    a
#endif

/// Memory manager shall keep track of statistics                             
/// Some overhead upon allocation/deallocation/reallocation                   
/// Some methods, like string null-termination will pick more memory-         
/// consitent, but less performant approaches (see Text::Terminate())         
#ifdef LANGULUS_FEATURE_MEMORY_STATISTICS
   #undef LANGULUS_FEATURE_MEMORY_STATISTICS
   #define LANGULUS_FEATURE_MEMORY_STATISTICS() 1
   #define IF_LANGULUS_MEMORY_STATISTICS(a)     a
   #define IF_NOT_LANGULUS_MEMORY_STATISTICS(a) LANGULUS(NOOP)
#else
   #define LANGULUS_FEATURE_MEMORY_STATISTICS() 0
   #define IF_LANGULUS_MEMORY_STATISTICS(a)     LANGULUS(NOOP)
   #define IF_NOT_LANGULUS_MEMORY_STATISTICS(a) a
#endif

/// Replace the default new-delete operators with custom ones                 
/// No overhead, no dependencies                                              
#ifdef LANGULUS_FEATURE_NEWDELETE
   #undef LANGULUS_FEATURE_NEWDELETE
   #define LANGULUS_FEATURE_NEWDELETE()   1
   #define IF_LANGULUS_NEWDELETE(a)       a
   #define IF_NOT_LANGULUS_NEWDELETE(a)   LANGULUS(NOOP)
#else
   #define LANGULUS_FEATURE_NEWDELETE()   0
   #define IF_LANGULUS_NEWDELETE(a)       LANGULUS(NOOP)
   #define IF_NOT_LANGULUS_NEWDELETE(a)   a
#endif

/// Enables utf support and utilities for Text container                      
/// No runtime overhead                                                       
#ifdef LANGULUS_FEATURE_UNICODE
   #undef LANGULUS_FEATURE_UNICODE
   #define LANGULUS_FEATURE_UNICODE()     1
   #define IF_LANGULUS_UNICODE(a)         a
   #define IF_NOT_LANGULUS_UNICODE(a)     LANGULUS(NOOP)
#else
   #define LANGULUS_FEATURE_UNICODE()     0
   #define IF_LANGULUS_UNICODE(a)         LANGULUS(NOOP)
   #define IF_NOT_LANGULUS_UNICODE(a)     a
#endif

/// Enable memory compression utilities for containers                        
/// Gives a bit of general runtime overhead, zstd will be linked              
#ifdef LANGULUS_FEATURE_COMPRESSION
   #undef LANGULUS_FEATURE_COMPRESSION
   #define LANGULUS_FEATURE_COMPRESSION() 1
   #define IF_LANGULUS_COMPRESSION(a)     a
   #define IF_NOT_LANGULUS_COMPRESSION(a) LANGULUS(NOOP)
#else
   #define LANGULUS_FEATURE_COMPRESSION() 0
   #define IF_LANGULUS_COMPRESSION(a)     LANGULUS(NOOP)
   #define IF_NOT_LANGULUS_COMPRESSION(a) a
#endif

/// Enable memory encryption and decryption                                   
/// Gives a tiny runtime overhead, no dependencies                            
#ifdef LANGULUS_FEATURE_ENCRYPTION
   #undef LANGULUS_FEATURE_ENCRYPTION
   #define LANGULUS_FEATURE_ENCRYPTION()  1
   #define IF_LANGULUS_ENCRYPTION(a)      a
   #define IF_NOT_LANGULUS_ENCRYPTION(a)  LANGULUS(NOOP)
#else
   #define LANGULUS_FEATURE_ENCRYPTION()  0
   #define IF_LANGULUS_ENCRYPTION(a)      LANGULUS(NOOP)
   #define IF_NOT_LANGULUS_ENCRYPTION(a)  a
#endif

/// Detect compiler                                                           
#if defined(__GNUC__) and not defined(__clang__)
   // We're on a GNUC Compiler!                                         
   #define LANGULUS_COMPILER_GCC() 1
#else
   #define LANGULUS_COMPILER_GCC() 0
#endif

#if defined(__clang__)
   // We're on a clang compiler!                                        
   #define LANGULUS_COMPILER_CLANG() 1
#else
   #define LANGULUS_COMPILER_CLANG() 0
#endif

#if defined(_MSC_VER) and not defined(__clang__)
   // We're on a microsoft visual c++ compiler!                         
   #define LANGULUS_COMPILER_MSVC() 1
#else
   #define LANGULUS_COMPILER_MSVC() 0
#endif

#if defined(__wasm__)
   // We're on a web assembly compiler!                                 
   #define LANGULUS_COMPILER_WASM() 1
#else
   #define LANGULUS_COMPILER_WASM() 0
#endif

#if defined(__MINGW32__) or defined(__MINGW64__) 
   // We're on a mingw compiler!                                        
   #define LANGULUS_COMPILER_MINGW() 1
#else
   #define LANGULUS_COMPILER_MINGW() 0
#endif

/// Checks if a given compiler is enabled                                     
#define LANGULUS_COMPILER(a) LANGULUS_COMPILER_##a()

/// Here's an alternative well-tested solution for now:                       
/// Shamelessly stolen from boost and extended to my liking                   
/// Dumps the current function name                                           
#if defined(__GNUC__) or (defined(__MWERKS__) and (__MWERKS__ >= 0x3000)) or (defined(__ICC) and (__ICC >= 600)) or defined(__ghs__)
   #define LANGULUS_FUNCTION() __PRETTY_FUNCTION__
#elif defined(__clang__) or defined(__wasm__)
   #define LANGULUS_FUNCTION() __PRETTY_FUNCTION__
#elif defined(__DMC__) and (__DMC__ >= 0x810)
   #define LANGULUS_FUNCTION() __PRETTY_FUNCTION__
#elif defined(__FUNCSIG__) or defined(_MSC_VER)
   #define LANGULUS_FUNCTION() __FUNCSIG__
#elif (defined(__INTEL_COMPILER) and (__INTEL_COMPILER >= 600)) or (defined(__IBMCPP__) and (__IBMCPP__ >= 500))
   #define LANGULUS_FUNCTION() __FUNCTION__
#elif defined(__BORLANDC__) and (__BORLANDC__ >= 0x550)
   #define LANGULUS_FUNCTION() __FUNC__
#elif defined(__STDC_VERSION__) and (__STDC_VERSION__ >= 199901)
   #define LANGULUS_FUNCTION() __func__
#elif defined(__cplusplus) and (__cplusplus >= 201103)
   #define LANGULUS_FUNCTION() __func__
#else
   #error LANGULUS_FUNCTION not implemented
#endif

/// Utility macro, that turns its argument to a string literal (inner)        
#define LANGULUS_STRINGIFY_INNER(x) #x

/// Utility macro, that turns its argument to a string literal                
#define LANGULUS_STRINGIFY(x) LANGULUS_STRINGIFY_INNER(x)

/// Macro that generates a literal with the function name, file, and line     
#define LANGULUS_LOCATION() __FILE__ ":" LANGULUS_STRINGIFY(__LINE__)
#define HERE() LANGULUS_LOCATION()

#define LANGULUS_OS(a) LANGULUS_OS_##a()

#if defined(_WIN32) or defined(__CYGWIN__)
   #define LANGULUS_OS_WINDOWS() 1
#else 
   #define LANGULUS_OS_WINDOWS() 0
#endif

#if defined(__linux__)
   #define LANGULUS_OS_LINUX() 1
#else 
   #define LANGULUS_OS_LINUX() 0
#endif

#if defined(__ANDROID__)
   #define LANGULUS_OS_ANDROID() 1
#else 
   #define LANGULUS_OS_ANDROID() 0
#endif

#if defined(__APPLE__)
   #define LANGULUS_OS_MACOS() 1
#else 
   #define LANGULUS_OS_MACOS() 0
#endif

#if defined(__unix__)
   #define LANGULUS_OS_UNIX() 1
#else 
   #define LANGULUS_OS_UNIX() 0
#endif

#if defined(__FreeBSD__)
   #define LANGULUS_OS_FREEBSD() 1
#else 
   #define LANGULUS_OS_FREEBSD() 0
#endif

/// Shared object export/import attributes                                    
#ifdef LANGULUS_SHARED_LIBRARIES
   #if LANGULUS_COMPILER(GCC) or LANGULUS_COMPILER(CLANG) or LANGULUS_COMPILER(WASM)
      #if LANGULUS_OS(WINDOWS)
         #define LANGULUS_EXPORT() __attribute__ ((dllexport))
         #define LANGULUS_IMPORT() __attribute__ ((dllimport))
      #else
         #define LANGULUS_EXPORT() __attribute__ ((visibility("default")))
         #define LANGULUS_IMPORT() // requires -fvisibility=hidden      
      #endif
   #elif LANGULUS_COMPILER(MSVC) or LANGULUS_COMPILER(MINGW)
      #define LANGULUS_EXPORT() __declspec(dllexport)
      #define LANGULUS_IMPORT() __declspec(dllimport)
   #else 
      #error Compiler not implemented
   #endif
#else
   /// Shared library exports are disabled                                    
   #define LANGULUS_EXPORT()
   #define LANGULUS_IMPORT()
#endif

/// Useful for globally exporting everything, when building the framework     
#ifdef LANGULUS_EXPORT_ALL
   #define LANGULUS_API_ALL() LANGULUS_EXPORT()
#else
   #define LANGULUS_API_ALL() LANGULUS_IMPORT()
#endif

/// Used to define imports/exports per module                                 
#define LANGULUS_API(a) LANGULUS_API_##a()

/// Make the rest of the code aware, that Langulus::Core has been included    
#define LANGULUS_LIBRARY_CORE() 1

/// All non-argument macros should use this facility                          
/// https://www.fluentcpp.com/2019/05/28/better-macros-better-flags/          
#define LANGULUS(a) LANGULUS_##a()

/// Checks if a library is included                                           
#define LANGULUS_LIBRARY(a) LANGULUS_LIBRARY_##a()

/// Checks if a feature is enabled                                            
#define LANGULUS_FEATURE(a) LANGULUS_FEATURE_##a()

/// Checks if code is executed at compile-time                                
///   @attention must be followed by {...}                                    
/// TODO when we transition to C++23, we should replace                       
/// if (std::is_constant_evaluated()) statements with `if consteval` ones     
/// unfortunately MSVC is lagging behind a lot, so this macro is here to      
/// eventually replace and test it out when they catch up                     
#define IF_CONSTEXPR()     if (    ::std::is_constant_evaluated())
#define IF_NOT_CONSTEXPR() if (not ::std::is_constant_evaluated())

/// No-op for empty macros, forces coder to add a semicolon to avoid          
/// obscure errors                                                            
#define LANGULUS_NOOP() ((void)0)

#if LANGULUS_COMPILER(MSVC)
   /// Force no inlining                                                      
   #define LANGULUS_NOINLINE() __declspec(noinline)

   /// Force inlining, even on debug builds                                   
   #define LANGULUS_ALWAYS_INLINED() __forceinline

   #if LANGULUS(DEBUG)
      #define LANGULUS_INLINED() inline
   #else
      /// Force always inlining - significantly increases build time!         
      #define LANGULUS_INLINED() __forceinline
   #endif
#else
   /// Force no inlining                                                      
   #define LANGULUS_NOINLINE() __attribute__((noinline))

   /// Force inlining, even on debug builds                                   
   #define LANGULUS_ALWAYS_INLINED() __attribute__((always_inline)) inline

   #if LANGULUS(DEBUG)
      #define LANGULUS_INLINED() inline
   #else
      /// Force always inlining - significantly increases build time!         
      #define LANGULUS_INLINED() __attribute__((always_inline)) inline
   #endif
#endif

#ifndef LANGULUS_ALIGNMENT
   #define LANGULUS_ALIGNMENT 16
#endif


///                                                                           
///   The all-encompassing Langulus namespace                                 
///                                                                           
namespace Langulus
{
   
   /// The default floating point type, depends on configuration              
   #if not defined(LANGULUS_FPU_DOUBLE)
      using Real = float;
   #elif defined(LANGULUS_FPU_DOUBLE) and not defined(LANGULUS_FPU_FLOAT)
      using Real = double;
   #else
      #error Conflicting real type definitions
   #endif

   /// The size of a void* in bytes, depends on architecture                  
   constexpr int Byteness = sizeof(void*);

   /// The size of a void* in bits, depends on architecture                   
   constexpr int Bitness = Byteness * 8;

   /// The default alignment, depends on configuration and enabled SIMD       
   constexpr int Alignment = LANGULUS_ALIGNMENT;
   
   /// Equivalent to ::std::true_type, but without the silly nomenclature     
   struct Yes {
      static constexpr bool Enabled = true;
   };

   /// Equivalent to Yes, but also carries a constant of any type             
   template<auto VALUE>
   struct YesValue {
      static constexpr auto Constant = VALUE;
      static constexpr bool Enabled = true;
   };

   /// Equivalent to ::std::false_type, but without the silly nomenclature    
   struct No {
      static constexpr bool Enabled = false;
   };
   
   /// Equivalent to ::std::false_type or ::std::true_type, depending on arg  
   template<bool VALUE>
   struct Maybe {
      static constexpr bool Enabled = VALUE;
   };
   
   /// Same as ::std::declval, but more conveniently named                    
   template<class T>
   T&& Fake() noexcept {
      static_assert(false, "Calling Fake is ill-formed");
   }

} // namespace Langulus