///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Except.hpp"
#include "Logger.hpp"
#include "NameOf.hpp"

#if LANGULUS(STACKTRACE)
   #include <stacktrace>

   #ifndef LANGULUS_DEFAULT_STACK_SKIP
      #define LANGULUS_DEFAULT_STACK_SKIP 2
   #endif

   #ifndef LANGULUS_DEFAULT_STACK_DEPTH
      #define LANGULUS_DEFAULT_STACK_DEPTH 3
   #endif
#endif
namespace Langulus
{
   #if LANGULUS(STACKTRACE)
      /// Dump the stack                                                      
      ///   @param depth - the number of stack entries to log                 
      ///   @param skip - the number of stack entries to skip. These are      
      ///      usually the Stacktrace() function itself, as well as the       
      ///      ErrorInner/AssertInner/AssumeInner function that called it.    
      inline void Stacktrace(
         const size_t depth = LANGULUS_DEFAULT_STACK_DEPTH,
         const size_t skip  = LANGULUS_DEFAULT_STACK_SKIP
      ) {
         auto stack = std::stacktrace::current();
         if (depth > 1) {
            auto group = Logger::Section("Current stack:");
            auto skipped = skip;
            auto dumped = depth;
            for (auto const& frame : stack) {
               if (skipped) {
                  --skipped;
                  continue;
               }

               try {
                  Logger::Line(std::to_string(frame));
               }
               catch (...) {
                  Logger::Line("<error while logging stack frame>");
               }

               --dumped;
               if (not dumped)
                  break;
            }

            if (stack.size() > depth + skip) {
               Logger::Line("(", stack.size() - (depth + skip),
                  " additional hidden entries, "
                  "define LANGULUS_DEFAULT_STACK_DEPTH to show more)"
               );
            }
         }
         else Logger::Line("At: ", std::to_string(stack[skip]));
      }
   #endif

   /// Will throw an exception                                                
   ///   @param m1 optional main error message                                
   ///   @param location optional location of the error                       
   ///   @param mn additional information to log                              
   template<class E = Exception, class...MORE>
   void ErrorInner(
      [[maybe_unused]] const char* location,
      ::std::string_view const& m1 = "<unknown error>",
      MORE&&...mn
   ) {
      // Log error message                                              
      auto s = Logger::ErrorScoped("Assertion failure: ");
      Logger::Append(m1);
      (Logger::Append(LglsFwd(mn)), ...);
      #if LANGULUS(STACKTRACE)
         Stacktrace();
      #else
         if (location)
            Logger::Line("At: ", location);
      #endif

      // Throw                                                          
      if constexpr (CT::Exception<E>)
         throw E {m1.data(), location};
      else
         throw E {m1.data()};
   }

   #define LglsError(...) ::Langulus::ErrorInner(HERE() __VA_OPT__(,) __VA_ARGS__)
   
   /// Assertion that works both at runtime and at compile-time.              
   /// Will throw an exception if condition isn't met at runtime.             
   ///   @param condition the condition that must hold true                   
   ///   @param m1 optional main error message if condition doesn't hold      
   ///   @param location optional location of the error                       
   ///   @param mn additional information to log                              
   template<class E = Exception, class...MORE>
   constexpr void AssertInner(
      [[maybe_unused]] const char* location, bool condition,
      ::std::string_view const& m1 = "<unknown assertion failure>",
      MORE&&...mn
   ) {
      if not consteval {
         if (not condition) {
            // Log error message                                        
            auto s = Logger::ErrorScoped("Assertion failure: ");
            Logger::Append(m1);
            (Logger::Append(LglsFwd(mn)), ...);
            #if LANGULUS(STACKTRACE)
               Stacktrace();
            #else
               if (location)
                  Logger::Line("At: ", location);
            #endif

            // Throw                                                    
            if constexpr (CT::Exception<E>)
               throw E {m1.data(), location};
            else
               throw E {m1.data()};
         }
      }
   }
   
   #define LglsAssert(...) ::Langulus::AssertInner(HERE(), __VA_ARGS__)

   /// Assertion that works at runtime.                                       
   /// Doesn't throw or ruin compilation.                                     
   ///   @param condition the condition that must hold true                   
   ///   @param m1 optional main warning message if condition doesn't hold    
   ///   @param location optional location of the error                       
   ///   @param mn additional information to log                              
   template<class...MORE>
   constexpr void AssertWarnInner(
      [[maybe_unused]] const char* location, bool condition,
      ::std::string_view const& m1 = "<unknown assertion failure>",
      MORE&&...mn
   ) noexcept {
      if not consteval {
         if (not condition) {
            // Log error message                                        
            auto s = Logger::WarningScoped("Assertion failure: ");
            Logger::Append(m1);
            (Logger::Append(LglsFwd(mn)), ...);
            #if LANGULUS(STACKTRACE)
               Stacktrace(1);
            #else
               if (location)
                  Logger::Line("At: ", location);
            #endif
         }
      }
   }
   
   #define LglsAssertWarn(...) ::Langulus::AssertWarnInner(HERE(), __VA_ARGS__)

   #if LANGULUS(SAFE) > 0
   /// User assumption that works both at runtime and at compile-time.        
   /// Tested only if LANGULUS(SAFE) >= 1.                                    
   /// Will throw an exception if condition isn't met at runtime.             
   ///   @param condition the condition that must hold true                   
   ///   @param m1 optional main error message if condition doesn't hold      
   ///   @param location optional location of the error                       
   ///   @param mn additional information to log                              
   template<class E = Exception, class...MORE>
   constexpr void AssumeUserInner(
      [[maybe_unused]] const char* location, bool condition,
      ::std::string_view const& m1 = "<unknown user assumption failure>",
      MORE&&...mn
   ) {
      if not consteval {
         if (not condition) {
            // Log error message                                        
            auto s = Logger::ErrorScoped("User assumption failure: ");
            Logger::Append(m1);
            (Logger::Append(LglsFwd(mn)), ...);
            #if LANGULUS(STACKTRACE)
               Stacktrace();
            #else
               if (location)
                  Logger::Line("At: ", location);
            #endif

            // Throw                                                    
            if constexpr (CT::Exception<E>)
               throw E {m1.data(), location};
            else
               throw E {m1.data()};
         }
      }
   }
   
   /// User assumption at runtime.                                            
   /// Tested only if LANGULUS(SAFE) >= 1.                                    
   /// Doesn't throw or ruin compilation.                                     
   ///   @param condition the condition that must hold true                   
   ///   @param m1 optional main warning message if condition doesn't hold    
   ///   @param location optional location of the error                       
   ///   @param mn additional information to log                              
   template<class...MORE>
   constexpr void AssumeUserWarnInner(
      [[maybe_unused]] const char* location, bool condition,
      ::std::string_view const& m1 = "<unknown assertion failure>",
      MORE&&...mn
   ) noexcept {
      if not consteval {
         if (not condition) {
            // Log error message                                        
            auto s = Logger::WarningScoped("User assumption failure: ");
            Logger::Append(m1);
            (Logger::Append(LglsFwd(mn)), ...);
            #if LANGULUS(STACKTRACE)
               Stacktrace(1);
            #else
               if (location)
                  Logger::Line("At: ", location);
            #endif
         }
      }
   }

      #define LglsAssumeUser(...)     ::Langulus::AssumeUserInner(HERE(), __VA_ARGS__)
      #define LglsAssumeUserWarn(...) ::Langulus::AssumeUserWarnInner(HERE(), __VA_ARGS__)
   
      /// Leverages C++23's [[assume(condition)]] attribute, in order to both 
      /// test the assumption when safety is enabled, and instruct the        
      /// compiler to generate more performant code                           
      #define LglsAssumeUserAndOptimize(...) \
         ::Langulus::AssumeUserInner(HERE(), __VA_ARGS__); \
         LglsCompilerSpecificAssume(__VA_ARGS__)
   #else
      #define LglsAssumeUser(...)            LANGULUS(NOOP)
      #define LglsAssumeUserWarn(...)        LANGULUS(NOOP)
      #define LglsAssumeUserAndOptimize(...) LglsCompilerSpecificAssume(__VA_ARGS__)
   #endif

   #if LANGULUS(SAFE) > 1
   /// Developer assumption that works both at runtime and at compile-time.   
   /// Tested only if LANGULUS(SAFE) >= 2.                                    
   /// Will throw an exception if condition isn't met at runtime.             
   ///   @param condition the condition that must hold true                   
   ///   @param m1 optional main error message if condition doesn't hold      
   ///   @param location optional location of the error                       
   ///   @param mn additional information to log                              
   template<class E = Exception, class...MORE>
   constexpr void AssumeDevInner(
      [[maybe_unused]] const char* location, bool condition,
      ::std::string_view const& m1 = "<unknown dev assumption failure>",
      MORE&&...mn
   ) {
      if not consteval {
         if (not condition) {
            // Log error message                                        
            auto s = Logger::ErrorScoped("Dev assumption failure: ");
            Logger::Append(m1);
            (Logger::Append(LglsFwd(mn)), ...);
            #if LANGULUS(STACKTRACE)
               Stacktrace();
            #else
               if (location)
                  Logger::Line("At: ", location);
            #endif

            // Throw                                                    
            if constexpr (CT::Exception<E>)
               throw E {m1.data(), location};
            else
               throw E {m1.data()};
         }
      }
   }
   
   /// Developer assumption at runtime.  Tested only if LANGULUS(SAFE) >= 2.  
   /// Doesn't throw or ruin compilation.                                     
   ///   @param condition the condition that must hold true                   
   ///   @param m1 optional main warning message if condition doesn't hold    
   ///   @param location optional location of the error                       
   ///   @param mn additional information to log                              
   template<class...MORE>
   constexpr void AssumeDevWarnInner(
      [[maybe_unused]] const char* location, bool condition,
      ::std::string_view const& m1 = "<unknown assertion failure>",
      MORE&&...mn
   ) noexcept {
      if not consteval {
         if (not condition) {
            // Log error message                                        
            auto s = Logger::WarningScoped("Dev assumption failure: ");
            Logger::Append(m1);
            (Logger::Append(LglsFwd(mn)), ...);
            #if LANGULUS(STACKTRACE)
               Stacktrace(1);
            #else
               if (location)
                  Logger::Line("At: ", location);
            #endif
         }
      }
   }

      #define LglsAssumeDev(...)     ::Langulus::AssumeDevInner(HERE(), __VA_ARGS__)
      #define LglsAssumeDevWarn(...) ::Langulus::AssumeDevWarnInner(HERE(), __VA_ARGS__)
   
      /// Leverages C++23's [[assume(condition)]] attribute, in order to both 
      /// test the assumption when safety is enabled, and instruct the        
      /// compiler to generate more performant code                           
      #define LglsAssumeDevAndOptimize(...) \
         ::Langulus::AssumeDevInner(HERE(), __VA_ARGS__); \
         LglsCompilerSpecificAssume(__VA_ARGS__)
   #else
      #define LglsAssumeDev(...)             LANGULUS(NOOP)
      #define LglsAssumeDevWarn(...)         LANGULUS(NOOP)
      #define LglsAssumeDevAndOptimize(...)  LglsCompilerSpecificAssume(__VA_ARGS__)
   #endif

   /// Custom assumption that works both at runtime and at compile-time.      
   /// Tested only if LANGULUS(SAFE) >= LEVEL.                                
   /// Will throw an exception if condition isn't met at runtime.             
   ///   @param condition the condition that must hold true                   
   ///   @param m1 optional main error message if condition doesn't hold      
   ///   @param location optional location of the error                       
   ///   @param mn additional information to log                              
   template<uint LEVEL, class E = Exception, class...MORE>
   constexpr void AssumeInner(
      [[maybe_unused]] const char* location, bool condition,
      ::std::string_view const& m1 = "<unknown assumption failure>",
      MORE&&...mn
   ) {
      if constexpr (LANGULUS(SAFE) >= LEVEL) {
         if not consteval {
            if (not condition) {
               // Log error message                                     
               auto s = Logger::ErrorScoped("Assumption level ", LEVEL, " failure: ");
               Logger::Append(m1);
               (Logger::Append(LglsFwd(mn)), ...);
               #if LANGULUS(STACKTRACE)
                  Stacktrace();
               #else
                  if (location)
                     Logger::Line("At: ", location);
               #endif

               // Throw                                                 
               if constexpr (CT::Exception<E>)
                  throw E {m1.data(), location};
               else
                  throw E {m1.data()};
            }
         }
      }
   }

   /// Leverages C++23's [[assume(condition)]] attribute, in order to both    
   /// test the assumption when safety is enabled, and instruct the compiler  
   /// to generate more performant code                                       
   #define LglsAssume(LEVEL, ...) ::Langulus::AssumeInner<LEVEL>(HERE(), __VA_ARGS__)

   #define LglsAssumeAndOptimize(LEVEL, ...) \
      ::Langulus::AssumeInner<LEVEL>(HERE(), __VA_ARGS__) \
      LglsCompilerSpecificAssume(__VA_ARGS__)
   
   /// Custom assumption at runtime. Tested only if LANGULUS(SAFE) >= LEVEL.  
   /// Doesn't throw or ruin compilation.                                     
   ///   @param condition the condition that must hold true                   
   ///   @param m1 optional main warning message if condition doesn't hold    
   ///   @param location optional location of the error                       
   ///   @param mn additional information to log                              
   template<uint LEVEL, class...MORE>
   constexpr void AssumeWarnInner(
      [[maybe_unused]] const char* location, bool condition,
      ::std::string_view const& m1 = "<unknown assertion failure>",
      MORE&&...mn
   ) noexcept {
      if constexpr (LANGULUS(SAFE) >= LEVEL) {
         if not consteval {
            if (not condition) {
               // Log error message                                     
               auto s = Logger::WarningScoped("Assumption level ", LEVEL, " failure: ");
               Logger::Append(m1);
               (Logger::Append(LglsFwd(mn)), ...);
               #if LANGULUS(STACKTRACE)
                  Stacktrace(1);
               #else
                  if (location)
                     Logger::Line("At: ", location);
               #endif
            }
         }
      }
   }

   #define LglsAssumeWarn(LEVEL, ...) ::Langulus::AssumeWarnInner<LEVEL>(HERE(), __VA_ARGS__)
}

/// Convenience macro for specifying temporary lazyness                       
#define TODO() ::Langulus::AssertInner(HERE(), false, "Unfinished code")


namespace fmt
{
   /// @note global qualifier specializations don't work on GCC :(            
   /// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=66892                     

   ///                                                                        
   /// Extend FMT to be capable of logging any exception                      
   template<::Langulus::CT::Exception T>
   struct formatter<T> {
      template<class CONTEXT>
      constexpr auto parse(CONTEXT& ctx) {
         return ctx.begin();
      }

      template<class CONTEXT>
      auto format([[maybe_unused]] T const& e, CONTEXT& ctx) const {
         constexpr auto name = ::Langulus::NameOf<T>();
         #if LANGULUS(DEBUG)
            return format_to(ctx.out(), "{}({} at {})",
               static_cast<::Langulus::Token>(name), e.mMessage, e.mLocation);
         #else
            return format_to(ctx.out(), "{}", static_cast<::Langulus::Token>(name));
         #endif
      }
   };
}