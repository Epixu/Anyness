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


namespace Langulus::RTTI
{
   struct MetaData;
   struct MetaTag;
   struct MetaVerb;
   struct MetaConst;

   class DefinitionConst;
   class DefinitionData;
   class DefinitionTag;
   class DefinitionVerb;

   class Registry;
      
   namespace Inner
   {

      struct MetaDataNaked;
      struct MetaDataStructured_8_8;
      struct MetaDataStructured_16_16;
      struct MetaDataStructured_24_8;

      struct MetaTagNaked;
      struct MetaTagPacked_16;

      struct MetaConstNaked;
      struct MetaConstPacked_16;

      struct MetaVerbNaked;
      template<unsigned>
      struct MetaVerbStructured_X8;

   } // namespace Langulus::RTTI::Inner

} // namespace Langulus::RTTI

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
      while (l < r and *l <= 32)
         ++l;

      while (r > l and *(r-1) <= 32)
         --r;

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

      #if LANGULUS_FEATURE(MANAGED_MEMORY)
         // A sequential identifier provided by the registry            
         // Used for packing type ids                                   
         size_t mID = 0;
      #endif

      // Each reflected type has an unique hash based on C++ name       
      const Hash mHash;

      // Original name of the type as it appears in C++                 
      const ::std::string mCppNameOf;
      // Sanitized mToken with proper capitalization, used in scripting 
      ::std::string mNameOf;
      // Precomputed lowercase nameof                                   
      Lowercase mNameOfLowercased;

      // Each reflection may or may not have some info                  
      ::std::string mInfo = "<no info provided>";

      // Major version                                                  
      unsigned mVersionMajor = 1;

      // Minor version                                                  
      unsigned mVersionMinor = 0;

      // Populated to be LANGULUS_BOUNDARY on reflection-time           
      // Types can be reflected from the point of view of different     
      // shared libraries. Each new reflection will be applied on the   
      // top of the old one, but overwriting properties only if the     
      // changes come from the MainBoundary. Once mBoundary becomes     
      // the MainBoundary, the definition shall never be unregistered.  
      IF_LANGULUS_MANAGED_REFLECTION(Token mBoundary);

      /// Construct an abstract definition                                    
      ///   @param cppname - the name of the definition, as it appears in C++ 
      explicit Definition(const Token& cppname)
         : mHash     {HashOf(cppname)}
         , mCppNameOf{cppname} {}

      /// Reflect some common type properties, like info and version          
      ///   @tparam T - the type to reflect                                   
      template<class T>
      void ReflectCommon() {
         // Save the boundary at time of reflection                     
         IF_LANGULUS_MANAGED_REFLECTION(mBoundary = Langulus::Boundary);

         if constexpr (CT::Info<T>) {
            // Reflected info                                           
            if constexpr (CTTI::Info<T>::Enabled)
               mInfo = CTTI::Info<T>::Text;
            else if constexpr (requires { T::CTTI_Info::Enabled; })
               mInfo = T::CTTI_Info::Constant;
         }

         if constexpr (CT::Versioned<T>) {
            // Reflected version                                        
            if constexpr (CTTI::Versioned<T>::Enabled) {
               mVersionMajor = CTTI::Versioned<T>::Major;
               mVersionMinor = CTTI::Versioned<T>::Minor;
            }
            else if constexpr (requires { T::CTTI_Versioned::Enabled; }) {
               mVersionMajor = T::CTTI_Versioned::Major;
               mVersionMinor = T::CTTI_Versioned::Minor;
            }
         }
      }

   public:
      Definition() = delete;
      virtual ~Definition() = default;
   };
   
} // namespace Langulus::RTTI::Inner
