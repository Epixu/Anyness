///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "CT/Except.hpp"
#include "Logger.hpp"
#include "NameOf.hpp"


namespace Langulus
{
   
   /// Assertion that works both at runtime and at compile-time               
   /// Will throw an exception if condition isn't met at runtime              
   ///   @param condition - the condition that must hold true                 
   ///   @param m1 - optional main error message if condition doesn't hold    
   ///   @param location - optional location of the error                     
   ///   @param mn - additional information to log                            
   template<class E = Exception, class...MORE> LANGULUS(INLINED)
   constexpr void Assert(
      bool condition,
      const char* location = nullptr,
      const char* m1 = "<unknown assertion failure>",
      MORE&&...mn
   ) {
      if not consteval {
         if (not condition) {
            // Log location first, because message might cause          
            // additional errors                                        
            DEBUGGERY(if (location) Logger::ErrorRaw("At ", location));

            // Log error message                                        
            Logger::ErrorRaw("Assertion failure: ", m1, FWD(mn)...);

            // Throw                                                    
            if constexpr (CT::Exception<E>)
               throw E {m1, location};
            else
               throw E {m1};
         }
      }
   }
   
   /// Assertion that works at runtime                                        
   /// Doesn't throw or ruin compilation                                      
   ///   @param condition - the condition that must hold true                 
   ///   @param m1 - optional main warning message if condition doesn't hold  
   ///   @param location - optional location of the error                     
   ///   @param mn - additional information to log                            
   template<class...MORE> LANGULUS(INLINED)
   constexpr void AssertWarn(
      bool condition,
      const char* location = nullptr,
      const char* m1 = "<unknown assertion failure>",
      MORE&&...mn
   ) noexcept {
      if not consteval {
         if (not condition) {
            // Log location first, because message might cause          
            // additional errors                                        
            DEBUGGERY(if (location) Logger::WarningRaw("At ", location));

            // Log error message                                        
            Logger::WarningRaw("Assertion failure: ", m1, FWD(mn)...);
         }
      }
   }
   
   /// User assumption that works both at runtime and at compile-time         
   /// Tested only if LANGULUS(SAFE) >= 1                                     
   /// Will throw an exception if condition isn't met at runtime              
   ///   @param condition - the condition that must hold true                 
   ///   @param m1 - optional main error message if condition doesn't hold    
   ///   @param location - optional location of the error                     
   ///   @param mn - additional information to log                            
   template<class E = Exception, class...MORE> LANGULUS(INLINED)
   constexpr void AssumeUser(
      bool condition,
      const char* location = nullptr,
      const char* m1 = "<unknown user assumption failure>",
      MORE&&...mn
   ) {
      if constexpr (LANGULUS(SAFE) > 0) {
         if not consteval {
            if (not condition) {
               // Log location first, because message might cause       
               // additional errors                                     
               DEBUGGERY(if (location) Logger::ErrorRaw("At ", location));

               // Log error message                                     
               Logger::ErrorRaw("User assumption failure: ", m1, FWD(mn)...);

               // Throw                                                 
               if constexpr (CT::Exception<E>)
                  throw E {m1, location};
               else
                  throw E {m1};
            }
         }
      }
      else LANGULUS(NOOP);
   }

   /// Leverages C++23's [[assume(condition)]] attribute, in order to both    
   /// test the assumption when safety is enabled, and instruct the compiler  
   /// to generate more performant code                                       
   #if LANGULUS(SAFE) > 0
      #define AssumeUserAndOptimize(CONDITION, ...) \
         AssumeUser(static_cast<bool>(CONDITION), HERE(), __VA_ARGS__); \
         [[assume(CONDITION)]]
   #else
      #define AssumeUserAndOptimize(CONDITION, ...) [[assume(CONDITION)]]
   #endif

   /// User assumption at runtime                                             
   /// Tested only if LANGULUS(SAFE) >= 1                                     
   /// Doesn't throw or ruin compilation                                      
   ///   @param condition - the condition that must hold true                 
   ///   @param m1 - optional main warning message if condition doesn't hold  
   ///   @param location - optional location of the error                     
   ///   @param mn - additional information to log                            
   template<class...MORE> LANGULUS(INLINED)
   constexpr void AssumeUserWarn(
      bool condition,
      const char* location = nullptr,
      const char* m1 = "<unknown assertion failure>",
      MORE&&...mn
   ) noexcept {
      if constexpr (LANGULUS(SAFE) > 0) {
         if not consteval {
            if (not condition) {
               // Log location first, because message might cause       
               // additional errors                                     
               DEBUGGERY(if (location) Logger::WarningRaw("At ", location));

               // Log error message                                     
               Logger::WarningRaw("User assumption failure: ", m1, FWD(mn)...);
            }
         }
      }
      else LANGULUS(NOOP);
   }

   /// Developer assumption that works both at runtime and at compile-time    
   /// Tested only if LANGULUS(SAFE) >= 2                                     
   /// Will throw an exception if condition isn't met at runtime              
   ///   @param condition - the condition that must hold true                 
   ///   @param m1 - optional main error message if condition doesn't hold    
   ///   @param location - optional location of the error                     
   ///   @param mn - additional information to log                            
   template<class E = Exception, class...MORE> LANGULUS(INLINED)
   constexpr void AssumeDev(
      bool condition,
      const char* location = nullptr,
      const char* m1 = "<unknown dev assumption failure>",
      MORE&&...mn
   ) {
      if constexpr (LANGULUS(SAFE) > 1) {
         if not consteval {
            if (not condition) {
               // Log location first, because message might cause       
               // additional errors                                     
               DEBUGGERY(if (location) Logger::ErrorRaw("At ", location));

               // Log error message                                     
               Logger::ErrorRaw("Dev assumption failure: ", m1, FWD(mn)...);

               // Throw                                                 
               if constexpr (CT::Exception<E>)
                  throw E {m1, location};
               else
                  throw E {m1};
            }
         }
      }
      else LANGULUS(NOOP);
   }

   /// Leverages C++23's [[assume(condition)]] attribute, in order to both    
   /// test the assumption when safety is enabled, and instruct the compiler  
   /// to generate more performant code                                       
   #if LANGULUS(SAFE) > 1
      #define AssumeDevAndOptimize(CONDITION, ...) \
         AssumeDev(static_cast<bool>(CONDITION), HERE(), __VA_ARGS__); \
         [[assume(CONDITION)]]
   #else
      #define AssumeDevAndOptimize(CONDITION, ...) [[assume(CONDITION)]]
   #endif

   /// Developer assumption at runtime                                        
   /// Tested only if LANGULUS(SAFE) >= 2                                     
   /// Doesn't throw or ruin compilation                                      
   ///   @param condition - the condition that must hold true                 
   ///   @param m1 - optional main warning message if condition doesn't hold  
   ///   @param location - optional location of the error                     
   ///   @param mn - additional information to log                            
   template<class...MORE> LANGULUS(INLINED)
   constexpr void AssumeDevWarn(
      bool condition,
      const char* location = nullptr,
      const char* m1 = "<unknown assertion failure>",
      MORE&&...mn
   ) noexcept {
      if constexpr (LANGULUS(SAFE) > 1) {
         if not consteval {
            if (not condition) {
               // Log location first, because message might cause       
               // additional errors                                     
               DEBUGGERY(if (location) Logger::WarningRaw("At ", location));

               // Log error message                                     
               Logger::WarningRaw("Dev assumption failure: ", m1, FWD(mn)...);
            }
         }
      }
      else LANGULUS(NOOP);
   }

   /// Custom assumption that works both at runtime and at compile-time       
   /// Tested only if LANGULUS(SAFE) >= LEVEL                                 
   /// Will throw an exception if condition isn't met at runtime              
   ///   @param condition - the condition that must hold true                 
   ///   @param m1 - optional main error message if condition doesn't hold    
   ///   @param location - optional location of the error                     
   ///   @param mn - additional information to log                            
   template<unsigned LEVEL, class E = Exception, class...MORE> LANGULUS(INLINED)
   constexpr void Assume(
      bool condition,
      const char* location = nullptr,
      const char* m1 = "<unknown assumption failure>",
      MORE&&...mn
   ) {
      if constexpr (LANGULUS(SAFE) >= LEVEL) {
         if not consteval {
            if (not condition) {
               // Log location first, because message might cause       
               // additional errors                                     
               DEBUGGERY(if (location) Logger::ErrorRaw("At ", location));

               // Log error message                                     
               Logger::ErrorRaw("Assumption level ", LEVEL, " failure: ", m1, FWD(mn)...);

               // Throw                                                 
               if constexpr (CT::Exception<E>)
                  throw E {m1, location};
               else
                  throw E {m1};
            }
         }
      }
      else LANGULUS(NOOP);
   }

   /// Leverages C++23's [[assume(condition)]] attribute, in order to both    
   /// test the assumption when safety is enabled, and instruct the compiler  
   /// to generate more performant code                                       
   #define AssumeAndOptimize(LEVEL, CONDITION, ...) \
      Assume<LEVEL>(static_cast<bool>(CONDITION), HERE(), __VA_ARGS__); \
      [[assume(CONDITION)]];

   
   /// Custom assumption at runtime                                           
   /// Tested only if LANGULUS(SAFE) >= LEVEL                                 
   /// Doesn't throw or ruin compilation                                      
   ///   @param condition - the condition that must hold true                 
   ///   @param m1 - optional main warning message if condition doesn't hold  
   ///   @param location - optional location of the error                     
   ///   @param mn - additional information to log                            
   template<unsigned LEVEL, class...MORE> LANGULUS(INLINED)
   constexpr void AssumeWarn(
      bool condition,
      const char* location = nullptr,
      const char* m1 = "<unknown assertion failure>",
      MORE&&...mn
   ) noexcept {
      if constexpr (LANGULUS(SAFE) >= LEVEL) {
         if not consteval {
            if (not condition) {
               // Log location first, because message might cause       
               // additional errors                                     
               DEBUGGERY(if (location) Logger::WarningRaw("At ", location));

               // Log error message                                     
               Logger::WarningRaw("Assumption level ", LEVEL, " failure: ", m1, FWD(mn)...);
            }
         }
      }
      else LANGULUS(NOOP);
   }

} // namespace Langulus

/// Convenience macro for specifying temporary lazyness                       
#define TODO() ::Langulus::Assert(false, HERE(), "Unfinished code")


///                                                                           
/// Extend FMT to be capable of logging any exception                         
///                                                                           
template<::Langulus::CT::Exception T>
struct ::fmt::formatter<T> {
   template<class CONTEXT>
   constexpr auto parse(CONTEXT& ctx) {
      return ctx.begin();
   }

   template<class CONTEXT> LANGULUS(INLINED)
   auto format(T const& e, CONTEXT& ctx) const {
      constexpr auto name = ::Langulus::NameOf<T>();
      #if LANGULUS(DEBUG)
         return ::fmt::format_to(ctx.out(), "{}({} at {})",
            static_cast<::Langulus::Token>(name), e.mMessage, e.mLocation);
      #else
         return ::fmt::format_to(ctx.out(), "{}", static_cast<::Langulus::Token>(name));
      #endif
   }
};
