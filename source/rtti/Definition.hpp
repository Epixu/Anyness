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
#include <Langulus/CT/Version.hpp>


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
   #define LANGULUS_RTTI_BOUNDARY(a) namespace Langulus::RTTI { Token Boundary = a; }

   namespace Langulus::RTTI
   {
      /// The main boundary indentifier token                                 
      constexpr Token MainBoundary = "MAIN";
   }

   #if defined(LANGULUS_EXPORT_ALL) or defined(LANGULUS_EXPORT_RTTI)
      #define LANGULUS_API_RTTI() LANGULUS_EXPORT()
   #else
      #define LANGULUS_API_RTTI() LANGULUS_IMPORT()
   #endif
#else
   #define LANGULUS_RTTI_BOUNDARY(a)
#endif

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
      // First for immediate access                                     
      const Hash mHash;

      // Original name of the type as it appears in C++                 
      const Token mCppName;

      // The original reflected token used in scripting                 
      Token mToken;
      // Sanitized mToken, with proper capitalization                   
      ::std::string mTokenSanitized;

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

      /// Construct an abstract definition                                    
      ///   @param cppname - the name of the definition, as it appears in C++ 
      explicit Definition(const Token& cppname)
         : mHash    {HashOf(cppname)}
         , mCppName {cppname} {}

      /// Reflect some common type properties, like info and version          
      ///   @tparam T - the type to reflect                                   
      template<class T>
      void ReflectCommon() {
         if constexpr (CT::Info<T>) {
            // Reflected info                                           
            if constexpr (CTTI::Info<T>::Enabled)
               mInfo = CTTI::Info<T>::Text;
            else if constexpr (requires { T::CTTI_Info::Enabled; })
               mInfo = T::CTTI_Info::Constant;
         }

         if constexpr (CT::Version<T>) {
            // Reflected version                                        
            if constexpr (CTTI::Version<T>::Enabled) {
               mVersionMajor = CTTI::Version<T>::Major;
               mVersionMinor = CTTI::Version<T>::Minor;
            }
            else if constexpr (requires { T::CTTI_Version::Enabled; }) {
               mVersionMajor = T::CTTI_Version::Constant::Major;
               mVersionMinor = T::CTTI_Version::Constant::Minor;
            }
         }
      }

   public:
      Definition() = delete;
      virtual ~Definition() = default;

      IF_LANGULUS_MANAGED_REFLECTION(
         LANGULUS_API(RTTI) Token GetShortestUnambiguousToken() const
      );
   };
   
} // namespace Langulus::RTTI::Inner
