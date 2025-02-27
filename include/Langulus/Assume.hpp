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
      IF_NOT_CONSTEXPR() {
         if (not condition) {
            // Log location first, because message might cause          
            // additional errors                                        
            DEBUGGERY(if (location) Logger::Error("At ", location));

            // Log error message                                        
            Logger::Error("Assertion failure: ", m1, Forward<MORE>(mn)...);

            // Throw                                                    
            throw E {m1, location};
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
      IF_NOT_CONSTEXPR() {
         if (not condition) {
            // Log location first, because message might cause          
            // additional errors                                        
            DEBUGGERY(if (location) Logger::Warning("At ", location));

            // Log error message                                        
            Logger::Warning("Assertion failure: ", m1, Forward<MORE>(mn)...);
         }
      }
   }
   
   /// User assumption that works both at runtime and at compile-time         
   /// Enabled only if LANGULUS(SAFE) >= 1                                    
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
         IF_NOT_CONSTEXPR() {
            if (not condition) {
               // Log location first, because message might cause       
               // additional errors                                     
               DEBUGGERY(if (location) Logger::Error("At ", location));

               // Log error message                                     
               Logger::Error("User assumption failure: ", m1, Forward<MORE>(mn)...);

               // Throw                                                 
               throw E {m1, location};
            }
         }
      }
      else LANGULUS(NOOP);
   }
   
   /// User assumption at runtime                                             
   /// Enabled only if LANGULUS(SAFE) >= 1                                    
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
         IF_NOT_CONSTEXPR() {
            if (not condition) {
               // Log location first, because message might cause       
               // additional errors                                     
               DEBUGGERY(if (location) Logger::Warning("At ", location));

               // Log error message                                     
               Logger::Warning("User assumption failure: ", m1, Forward<MORE>(mn)...);
            }
         }
      }
      else LANGULUS(NOOP);
   }

   /// Developer assumption that works both at runtime and at compile-time    
   /// Enabled only if LANGULUS(SAFE) >= 2                                    
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
         IF_NOT_CONSTEXPR() {
            if (not condition) {
               // Log location first, because message might cause       
               // additional errors                                     
               DEBUGGERY(if (location) Logger::Error("At ", location));

               // Log error message                                     
               Logger::Error("Dev assumption failure: ", m1, Forward<MORE>(mn)...);

               // Throw                                                 
               throw E {m1, location};
            }
         }
      }
      else LANGULUS(NOOP);
   }
   
   /// Developer assumption at runtime                                        
   /// Enabled only if LANGULUS(SAFE) >= 2                                    
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
         IF_NOT_CONSTEXPR() {
            if (not condition) {
               // Log location first, because message might cause       
               // additional errors                                     
               DEBUGGERY(if (location) Logger::Warning("At ", location));

               // Log error message                                     
               Logger::Warning("Dev assumption failure: ", m1, Forward<MORE>(mn)...);
            }
         }
      }
      else LANGULUS(NOOP);
   }

   /// Custom assumption that works both at runtime and at compile-time       
   /// Enabled only if LANGULUS(SAFE) >= LEVEL                                
   /// Will throw an exception if condition isn't met                         
   /// Will throw an exception if condition isn't met at runtime              
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
         IF_NOT_CONSTEXPR() {
            if (not condition) {
               // Log location first, because message might cause       
               // additional errors                                     
               DEBUGGERY(if (location) Logger::Error("At ", location));

               // Log error message                                     
               Logger::Error("Assumption level ", LEVEL, " failure: ", m1, Forward<MORE>(mn)...);

               // Throw                                                 
               throw E {m1, location};
            }
         }
      }
      else LANGULUS(NOOP);
   }
   
   /// Custom assumption at runtime                                           
   /// Enabled only if LANGULUS(SAFE) >= LEVEL                                
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
         IF_NOT_CONSTEXPR() {
            if (not condition) {
               // Log location first, because message might cause       
               // additional errors                                     
               DEBUGGERY(if (location) Logger::Warning("At ", location));

               // Log error message                                     
               Logger::Warning("Assumption level ", LEVEL, " failure: ", m1, Forward<MORE>(mn)...);
            }
         }
      }
      else LANGULUS(NOOP);
   }

} // namespace Langulus

/// Convenience macro for specifying temporary lazyness                       
#define TODO() ::Langulus::Assert(false, HERE(), "Unfinished code")

/// Adds the appropriate noexcept specifiers for functions that throw only    
/// in safe builds                                                            
#define has_assumptions IF_UNSAFE(noexcept)