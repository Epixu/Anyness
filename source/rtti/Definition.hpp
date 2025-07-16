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
#include <Langulus/CT/Info.hpp>
#include <Langulus/CT/Versioned.hpp>

#include "Langulus/InfoOf.hpp"

#if LANGULUS_FEATURE(MANAGED_REFLECTION)
   #include <unordered_set>
#endif


namespace Langulus::RTTI
{
   class DefinitionConst;
   class DefinitionData;
   class DefinitionTag;
   class DefinitionVerb;

   class Registry;
      
   namespace Inner
   {
      struct MetaDataNaked;
      template<unsigned, unsigned>
      struct MetaDataStructured_XY;

      struct MetaTagNaked;
      struct MetaTagPacked_16;

      struct MetaConstNaked;
      struct MetaConstPacked_16;

      struct MetaVerbNaked;
      template<unsigned>
      struct MetaVerbStructured_X8;
   }
}

namespace Langulus::RTTI::Inner
{

   /// Convert a token to a lowercase string                                  
   ///   @param token - the token to lowercase                                
   ///   @return the lowercase string                                         
   LANGULUS(INLINED)
   Lowercase ToLowercase(const Token& token) noexcept {
      Lowercase lc;
      lc.reserve(token.size());
      ::std::ranges::transform(token.begin(), token.end(), std::back_inserter(lc),
         [](char c) { return static_cast<char>(::std::tolower(c)); }
      );
      return lc;
   }

   /// Isolate and lowercase an operator token                                
   ///   @param token - the operator                                          
   ///   @return the lowercased and isolated operator token                   
   LANGULUS(INLINED)
   Lowercase IsolateOperator(const Token& token) noexcept {
      // Skip skippable at the front and the back of token              
      auto l = token.data();
      auto r = token.data() + token.size();
      while (l < r and     *l <= 32)   ++l;
      while (r > l and *(r-1) <= 32)   --r;

      // Lowercase the isolated token                                   
      return ToLowercase(token.substr(l - token.data(), r - l));
   }
      
   /// Get the last, most relevant part of a token that may or may not have   
   /// namespaces in it. Essentially finds last "::" that isn't enclosed in   
   /// a template <>, and skip forward to that                                
   ///   @param token - the token to scan                                     
   ///   @return the last token                                               
   constexpr Token ToLastToken(const Token& token) noexcept {
      size_t depth = 0;
      for (size_t i = token.size() - 1; i < token.size(); --i) {
         switch (token[i]) {
         case ':':
            // If no depth, then we found it                            
            if (not depth)
               return token.substr(i + 1, token.size() - i - 1);
            break;
         case '>':
            // Open template scope                                      
            ++depth;
            break;
         case '<':
            // Close template scope                                     
            if (depth)
               --depth;
            break;
         default:
            break;
         }
      }

      return token;
   }

   ///                                                                        
   ///   Abstract definition                                                  
   ///                                                                        
   class Definition {
   protected:
      friend class RTTI::Registry;

      // Each reflected type has an unique hash based on C++ name       
      const Hash mHash;

      // Original name of the type as it appears in C++                 
      // We can't afford these to be pointers to avoid data behind them 
      // getting unloaded on a shared object unload                     
      const ::std::string mCppNameOf;
      // Sanitized mToken with proper capitalization, used in scripting 
      ::std::string mNameOf;
      // Precomputed lowercase nameof                                   
      Lowercase mNameOfLowercased;
      // Each reflection may or may not have some info                  
      ::std::string mInfoOf;

      // Major version                                                  
      unsigned mVersionMajor IF_SAFE(= 1);
      // Minor version                                                  
      unsigned mVersionMinor IF_SAFE(= 0);

      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
         // A sequential identifier provided by the registry            
         // Used for packing type ids                                   
         size_t mID = 0;

         // Populated from LANGULUS_BOUNDARY on reflection-time         
         // Types can be reflected from the point of view of different  
         // shared libraries. Each new reflection will be applied on the
         // top of the old one, but overwriting properties only if the  
         // changes come from the MainBoundary. Once mBoundary becomes  
         // the MainBoundary, the definition shall never be unregistered
         public: using BoundarySet = ::std::unordered_set<Token>;
         protected: BoundarySet mBoundaries;
      #endif

      /// Construct an abstract definition                                    
      ///   @param cppname - the C++ name of the definition                   
      ///   @param boundary - the library from which we're defining           
      Definition(const Token& cppname)
         : mHash      {HashOf(cppname)}
         , mCppNameOf {cppname} {}

      /// Reflect some common type properties, like info and version          
      ///   @attention must always be inline, so that boundary is relative    
      ///   @attention call this first, so that version is checked before any 
      ///      other changes are made to the type                             
      ///   @tparam T - the type to reflect                                   
      template<class T> LANGULUS(ALWAYS_INLINED)
      void ReflectCommon() {
         // Reflected version                                           
         mVersionMajor = VersionOf<T>().Major;
         mVersionMinor = VersionOf<T>().Minor;
         
         // Save the boundary at time of reflection, but don't even     
         // bother if it is the main one                                
         #if LANGULUS_FEATURE(MANAGED_REFLECTION)
            if (Boundary != MainBoundary)
               mBoundaries.insert(Boundary);
         #endif

         // Reflected info                                              
         if constexpr (CT::Info<T>)
            mInfoOf = InfoOf<T>();
      }

      /// Check whether the definition is in the current boundary, or has     
      /// been reflected from the main one                                    
      ///   @attention must always be inline, so that boundary is relative    
      LANGULUS(ALWAYS_INLINED)
      constexpr bool IsInRelevantBoundary() const noexcept {
         #if LANGULUS_FEATURE(MANAGED_REFLECTION)
            return mBoundaries.empty() or mBoundaries.contains(Boundary);
         #else
            return true;
         #endif
      }
      
   public:
      using CTTI_ReflectAs = void;

      Definition() = delete;
      virtual ~Definition() = default;
   };
   
} // namespace Langulus::RTTI::Inner
