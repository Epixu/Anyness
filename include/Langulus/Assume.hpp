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


namespace Langulus
{
   /// Will throw an exception                                                
   ///   @param m1 optional main error message                                
   ///   @param location optional location of the error                       
   ///   @param mn additional information to log                              
   template<class E = Exception, class...MORE>
   void ErrorInner(
      [[maybe_unused]] const char* location = nullptr,
      const char* m1 = "<unknown error>",
      MORE&&...mn
   ) {
      //if not consteval {
         // Log location first, because message might cause             
         // additional errors                                           
         if (location) {
            Logger::Error("At ");
            Logger::Append(location);
         }

         // Log error message                                           
         Logger::Error("Assertion failure: ");
         Logger::Append(m1);
         (Logger::Append(FWD(mn)), ...);

         // Throw                                                       
         if constexpr (CT::Exception<E>)
            throw E {m1, location};
         else
            throw E {m1};
      //}
   }

   #define LglsError(...) ErrorInner(HERE() __VA_OPT__(,) __VA_ARGS__)
   
   /// Assertion that works both at runtime and at compile-time.              
   /// Will throw an exception if condition isn't met at runtime.             
   ///   @param condition the condition that must hold true                   
   ///   @param m1 optional main error message if condition doesn't hold      
   ///   @param location optional location of the error                       
   ///   @param mn additional information to log                              
   template<class E = Exception, class...MORE>
   constexpr void AssertInner(
      bool condition,
      [[maybe_unused]] const char* location = nullptr,
      const char* m1 = "<unknown assertion failure>",
      MORE&&...mn
   ) {
      if not consteval {
         if (not condition) {
            // Log location first, because message might cause          
            // additional errors                                        
            if (location) {
               Logger::Error("At ");
               Logger::Append(location);
            }

            // Log error message                                        
            Logger::Error("Assertion failure: ");
            Logger::Append(m1);
            (Logger::Append(FWD(mn)), ...);

            // Throw                                                    
            if constexpr (CT::Exception<E>)
               throw E {m1, location};
            else
               throw E {m1};
         }
      }
   }
   
   #define LglsAssert(CONDITION, ...) \
      AssertInner(static_cast<bool>(CONDITION), HERE() __VA_OPT__(,) __VA_ARGS__)

   /// Assertion that works at runtime.                                       
   /// Doesn't throw or ruin compilation.                                     
   ///   @param condition the condition that must hold true                   
   ///   @param m1 optional main warning message if condition doesn't hold    
   ///   @param location optional location of the error                       
   ///   @param mn additional information to log                              
   template<class...MORE>
   constexpr void AssertWarnInner(
      bool condition,
      [[maybe_unused]] const char* location = nullptr,
      const char* m1 = "<unknown assertion failure>",
      MORE&&...mn
   ) noexcept {
      if not consteval {
         if (not condition) {
            // Log location first, because message might cause          
            // additional errors                                        
            if (location) {
               Logger::Warning("At ");
               Logger::Append(location);
            }

            // Log error message                                        
            Logger::Warning("Assertion failure: ");
            Logger::Append(m1);
            (Logger::Append(FWD(mn)), ...);
         }
      }
   }
   
   #define LglsAssertWarn(CONDITION, ...) \
      AssertWarnInner(static_cast<bool>(CONDITION), HERE() __VA_OPT__(,) __VA_ARGS__)

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
      bool condition,
      [[maybe_unused]] const char* location = nullptr,
      const char* m1 = "<unknown user assumption failure>",
      MORE&&...mn
   ) {
      if not consteval {
         if (not condition) {
            // Log location first, because message might cause          
            // additional errors                                        
            if (location) {
               Logger::Error("At ");
               Logger::Append(location);
            }

            // Log error message                                        
            Logger::Error("User assumption failure: ");
            Logger::Append(m1);
            (Logger::Append(FWD(mn)), ...);

            // Throw                                                    
            if constexpr (CT::Exception<E>)
               throw E {m1, location};
            else
               throw E {m1};
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
      bool condition,
      [[maybe_unused]] const char* location = nullptr,
      const char* m1 = "<unknown assertion failure>",
      MORE&&...mn
   ) noexcept {
      if not consteval {
         if (not condition) {
            // Log location first, because message might cause          
            // additional errors                                        
            if (location) {
               Logger::Warning("At ");
               Logger::Append(location);
            }

            // Log error message                                        
            Logger::Warning("User assumption failure: ");
            Logger::Append(m1);
            (Logger::Append(FWD(mn)), ...);
         }
      }
   }

      #define LglsAssumeUser(CONDITION, ...) \
         AssumeUserInner(static_cast<bool>(CONDITION), HERE() __VA_OPT__(,) __VA_ARGS__)
      #define LglsAssumeUserWarn(CONDITION, ...) \
         AssumeUserWarnInner(static_cast<bool>(CONDITION), HERE() __VA_OPT__(,) __VA_ARGS__)
   
      /// Leverages C++23's [[assume(condition)]] attribute, in order to both 
      /// test the assumption when safety is enabled, and instruct the        
      /// compiler to generate more performant code                           
      #define LglsAssumeUserAndOptimize(CONDITION, ...) \
         AssumeUserInner(static_cast<bool>(CONDITION), HERE() __VA_OPT__(,) __VA_ARGS__); \
         LglsCompilerSpecificAssume(CONDITION)
   #else
      #define LglsAssumeUser(CONDITION, ...) LANGULUS(NOOP)
      #define LglsAssumeUserWarn(CONDITION, ...) LANGULUS(NOOP)
      #define LglsAssumeUserAndOptimize(CONDITION, ...) [[assume(CONDITION)]]
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
      bool condition,
      [[maybe_unused]] const char* location = nullptr,
      const char* m1 = "<unknown dev assumption failure>",
      MORE&&...mn
   ) {
      if not consteval {
         if (not condition) {
            // Log location first, because message might cause          
            // additional errors                                        
            if (location) {
               Logger::Error("At ");
               Logger::Append(location);
            }

            // Log error message                                        
            Logger::Error("Dev assumption failure: ");
            Logger::Append(m1);
            (Logger::Append(FWD(mn)), ...);

            // Throw                                                    
            if constexpr (CT::Exception<E>)
               throw E {m1, location};
            else
               throw E {m1};
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
      bool condition,
      [[maybe_unused]] const char* location = nullptr,
      const char* m1 = "<unknown assertion failure>",
      MORE&&...mn
   ) noexcept {
      if not consteval {
         if (not condition) {
            // Log location first, because message might cause          
            // additional errors                                        
            if (location) {
               Logger::Warning("At ");
               Logger::Append(location);
            }

            // Log error message                                        
            Logger::Warning("Dev assumption failure: ");
            Logger::Append(m1);
            (Logger::Append(FWD(mn)), ...);
         }
      }
   }

      #define LglsAssumeDev(CONDITION, ...) \
         AssumeDevInner(static_cast<bool>(CONDITION), HERE() __VA_OPT__(,) __VA_ARGS__)
      #define LglsAssumeDevWarn(CONDITION, ...) \
         AssumeDevWarnInner(static_cast<bool>(CONDITION), HERE() __VA_OPT__(,) __VA_ARGS__)
   
      /// Leverages C++23's [[assume(condition)]] attribute, in order to both 
      /// test the assumption when safety is enabled, and instruct the        
      /// compiler to generate more performant code                           
      #define LglsAssumeDevAndOptimize(CONDITION, ...) \
         AssumeDevInner(static_cast<bool>(CONDITION), HERE() __VA_OPT__(,) __VA_ARGS__); \
         LglsCompilerSpecificAssume(CONDITION)
   #else
      #define LglsAssumeDev(CONDITION, ...) LANGULUS(NOOP)
      #define LglsAssumeDevWarn(CONDITION, ...) LANGULUS(NOOP)
      #define LglsAssumeDevAndOptimize(CONDITION, ...) [[assume(CONDITION)]]
   #endif

   /// Custom assumption that works both at runtime and at compile-time.      
   /// Tested only if LANGULUS(SAFE) >= LEVEL.                                
   /// Will throw an exception if condition isn't met at runtime.             
   ///   @param condition the condition that must hold true                   
   ///   @param m1 optional main error message if condition doesn't hold      
   ///   @param location optional location of the error                       
   ///   @param mn additional information to log                              
   template<unsigned LEVEL, class E = Exception, class...MORE>
   constexpr void AssumeInner(
      bool condition,
      [[maybe_unused]] const char* location = nullptr,
      const char* m1 = "<unknown assumption failure>",
      MORE&&...mn
   ) {
      if constexpr (LANGULUS(SAFE) >= LEVEL) {
         if not consteval {
            if (not condition) {
               // Log location first, because message might cause       
               // additional errors                                     
               if (location) {
                  Logger::Error("At ");
                  Logger::Append(location);
               }

               // Log error message                                     
               Logger::Error("Assumption level ", LEVEL, " failure: ");
               Logger::Append(m1);
               (Logger::Append(FWD(mn)), ...);

               // Throw                                                 
               if constexpr (CT::Exception<E>)
                  throw E {m1, location};
               else
                  throw E {m1};
            }
         }
      }
   }

   /// Leverages C++23's [[assume(condition)]] attribute, in order to both    
   /// test the assumption when safety is enabled, and instruct the compiler  
   /// to generate more performant code                                       
   #define LglsAssume(LEVEL, CONDITION, ...) \
      AssumeInner<LEVEL>(static_cast<bool>(CONDITION), HERE() __VA_OPT__(,) __VA_ARGS__)
   #define LglsAssumeAndOptimize(LEVEL, CONDITION, ...) \
      AssumeInner<LEVEL>(static_cast<bool>(CONDITION), HERE() __VA_OPT__(,) __VA_ARGS__) \
      LglsCompilerSpecificAssume(CONDITION)
   
   /// Custom assumption at runtime. Tested only if LANGULUS(SAFE) >= LEVEL.  
   /// Doesn't throw or ruin compilation.                                     
   ///   @param condition the condition that must hold true                   
   ///   @param m1 optional main warning message if condition doesn't hold    
   ///   @param location optional location of the error                       
   ///   @param mn additional information to log                              
   template<unsigned LEVEL, class...MORE>
   constexpr void AssumeWarnInner(
      bool condition,
      [[maybe_unused]] const char* location = nullptr,
      const char* m1 = "<unknown assertion failure>",
      MORE&&...mn
   ) noexcept {
      if constexpr (LANGULUS(SAFE) >= LEVEL) {
         if not consteval {
            if (not condition) {
               // Log location first, because message might cause       
               // additional errors                                     
               if (location) {
                  Logger::Error("At ");
                  Logger::Append(location);
               }

               // Log error message                                     
               Logger::Warning("Assumption level ", LEVEL, " failure: ");
               Logger::Append(m1);
               (Logger::Append(FWD(mn)), ...);
            }
         }
      }
   }

   #define LglsAssumeWarn(LEVEL, CONDITION, ...) \
      AssumeWarnInner<LEVEL>(static_cast<bool>(CONDITION), HERE() __VA_OPT__(,) __VA_ARGS__)
}

/// Convenience macro for specifying temporary lazyness                       
#define TODO() ::Langulus::AssertInner(false, HERE(), "Unfinished code")


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
