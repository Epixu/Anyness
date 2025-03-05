///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../HashOf.hpp"
#include "../NameOf.hpp"
#include <string_view>
#include <algorithm>
#include <iterator>


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
      // The shared library that defined the module, used to unload     
      // definitions when module is unloaded                            
      Token mLibraryName;

      LANGULUS_API(RTTI)
      Token GetShortestUnambiguousToken() const;
   #endif

      Definition() = delete;
      Definition(const Token&);

      template<CT::Decayed>
      void ReflectCommon();
   };
   
} // namespace Langulus::RTTI::Inner
