///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../HashOf.hpp"
#include <string_view>
#include <algorithm>
#include <iterator>


namespace Langulus::RTTI
{
   
   using Lowercase = ::std::string;
   using Token     = ::std::string_view;


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
      // Each reflected type has an unique hash                         
      // First for immediate access                                     
      const Hash mHash;

      // Each reflection primitive has a unique token, but that         
      // uniqueness is checked only if MANAGED_REFLECTION feature is    
      // enabled                                                        
      const Token mToken;

      // Each reflection may or may not have some info                  
      Token mInfo = "<no info provided>";

      // Original name of the type                                      
      Token mCppName;

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

   public:
      Definition() = delete;

      LANGULUS(INLINED)
      Definition(const Token& name)
         : mHash  {HashOf(name)}
         , mToken {name} {}
   };
   
} // namespace Langulus::RTTI
