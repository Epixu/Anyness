///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include <Langulus/HashOf.hpp>
#include <Langulus/NameOf.hpp>
#include <string_view>
#include <algorithm>
#include <iterator>
#include <cctype>


#if LANGULUS_FEATURE(MANAGED_REFLECTION)
   /// The Langulus::RTTI::Boundary symbol is intentionally left undefined,   
   /// so that it is mandatory for you to define it inside your executables   
   /// or mods. It's a simple compile-time string, that is attached upon data 
   /// reflection, so that RTTI can track from which library a type was       
   /// reflected, and thus unregister it when shared object is unloaded.      
   /// The boundary also affects pooling tactics, because if boundary is not  
   /// equal exactly to RTTI::MainBoundary, pooling will be PoolTactic::Type  
   /// by default, so that allocation that happen from external libraries can 
   /// be easily tracked                                                      
   #define LANGULUS_RTTI_BOUNDARY(a) \
      namespace Langulus::RTTI { Token Boundary = a; }

   namespace Langulus::RTTI
   {
      /// The main boundary indentifier token                                 
      constexpr Token MainBoundary = "MAIN";
   }
#else
   #define LANGULUS_RTTI_BOUNDARY(a)
#endif


namespace Langulus::RTTI::Inner
{

   /// Convert a token to a lowercase string                                  
   ///   @param token - the token to lowercase                                
   ///   @return the lowercase string                                         
   LANGULUS(INLINED)
   Lowercase ToLowercase(const Token& token) noexcept {
      Lowercase lc;
      lc.reserve(token.size());
      ::std::transform(token.begin(), token.end(), std::back_inserter(lc),
         [](char c) { return static_cast<char>(::std::tolower(c)); }
      );
      return lc;
   }


   ///                                                                        
   ///   Abstract definition                                                  
   ///                                                                        
   class Definition {
   protected:
      // Each reflected type has an unique hash based on C++ name       
      // First for immediate access                                     
      const Hash mHash;

      // Original name of the type as it appears in C++                 
      const Token mCppName;

      // The original reflected token used in scripting                 
      Token mToken;
      // Sanitized mToken, with proper capitalization                   
      std::string mTokenSanitized;

      // Each reflection may or may not have some info                  
      Token mInfo = "<no info provided>";

      // Major version                                                  
      unsigned mVersionMajor = 1;

      // Minor version                                                  
      unsigned mVersionMinor = 0;

   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      // Populated to be LANGULUS_RTTI_BOUNDARY on reflection-time      
      Token mBoundary;
   #endif

      Definition() = delete;
      Definition(const Token&);

      template<class>
      void ReflectCommon();
   };
   
} // namespace Langulus::RTTI::Inner
