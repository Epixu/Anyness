///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Definition.hpp"
#include <unordered_map>
#include <unordered_set>

#if not LANGULUS_FEATURE(MANAGED_REFLECTION)
   #error "This file shouldn't be included if MANAGED_REFLECTION is disabled"
#endif

#if defined(LANGULUS_EXPORT_ALL) or defined(LANGULUS_EXPORT_RTTI)
   #define LANGULUS_API_RTTI() LANGULUS_EXPORT()
#else
   #define LANGULUS_API_RTTI() LANGULUS_IMPORT()
#endif

/// Make the rest of the code aware, that Langulus::RTTI has been included    
#define LANGULUS_LIBRARY_RTTI() 1


namespace Langulus::RTTI
{

   using MetaSet = ::std::unordered_set<Inner::Definition const*>;

   struct MetaException : Exception {
      using Exception::Exception;
   };


   ///                                                                        
   ///   The RTTI registry                                                    
   ///                                                                        
   /// Available only if managed reflection feature is enabled                
   ///                                                                        
   class Registry {
      // Definitions indexed by lowercase reflected name                
      template<class T>
      using MetaMap = ::std::unordered_map<Token, T>;
      
      // Definitions indexed by ID                                      
      template<class T>
      using Indexed = ::std::vector<T>;

      // Database for meta data definitions                             
      MetaMap<DefinitionData const*>  mMetaDataByName;
      Indexed<DefinitionData const*>  mMetaDataByID;

      // Database for named values                                      
      MetaMap<DefinitionConst const*> mMetaConstantsByName;
      Indexed<DefinitionConst const*> mMetaConstantsByID;

      // Database for meta trait definitions                            
      MetaMap<DefinitionTag const*>   mMetaTagsByName;
      Indexed<DefinitionTag const*>   mMetaTagsByID;

      // Database for meta verb definitions                             
      MetaMap<DefinitionVerb const*>  mMetaVerbsByCppName;
      MetaMap<DefinitionVerb const*>  mMetaVerbsByTokens;
      Indexed<DefinitionVerb const*>  mMetaVerbsByID;

      // Database for ambiguous tokens                                  
      MetaMap<MetaSet> mMetaAmbiguous;
      // Meta data definitions, indexed by file extensions              
      MetaMap<MetaSet> mFileDatabase;
      
      template<bool BY_CPPNAME>
      auto GetMetaByName(const auto& where, const Token& name) const noexcept
         -> decltype(where.begin()->second);
      auto GetMetaByID  (const auto& where, size_t id) const noexcept;

   protected:
      friend class DefinitionVerb;
      friend class DefinitionData;
      friend class DefinitionTag;
      friend class DefinitionConst;
      friend struct Inner::MetaTagPacked_16;
      friend struct Inner::MetaConstPacked_16;
      template<unsigned, unsigned>
      friend struct Inner::MetaDataStructured_XY;

      void RegisterVerbOperator       (Token const&) has_assumptions;
      void RegisterVerbOperatorReverse(Token const&) has_assumptions;
      void RegisterVerbToken          (Token const&) has_assumptions;
      void RegisterVerbTokenReverse   (Token const&) has_assumptions;

      LANGULUS_API(RTTI)
      auto RegisterData(Token const&) -> DefinitionData&;
      LANGULUS_API(RTTI)
      auto ReserveDataID(DefinitionData const*) -> size_t;

      LANGULUS_API(RTTI)
      auto RegisterConst(Token const&) -> DefinitionConst&;

      LANGULUS_API(RTTI)
      auto RegisterTag(Token const&) -> DefinitionTag&;

      LANGULUS_API(RTTI)
      auto RegisterVerb(Token const&) -> DefinitionVerb&;
      
      LANGULUS_API(RTTI)
      void RegisterFileExtension(Token const&, DefinitionData*) has_assumptions;

      LANGULUS_API(RTTI)
      auto GetMetaDataByCppName(Token const&) const noexcept -> DefinitionData const*;
      LANGULUS_API(RTTI)
      auto GetMetaTagByCppName(Token const&) const noexcept -> DefinitionTag const*;
      LANGULUS_API(RTTI)
      auto GetMetaVerbByCppName(Token const&) const noexcept -> DefinitionVerb const*;
      LANGULUS_API(RTTI)
      auto GetMetaConstByCppName(Token const&) const noexcept -> DefinitionConst const*;

      LANGULUS_API(RTTI)
      auto GetMetaDataByID(Inner::MetaDataStructured_XY<2, 2> const&) const noexcept -> DefinitionData const*;
      LANGULUS_API(RTTI)
      auto GetMetaDataByID(Inner::MetaDataStructured_XY<3, 1> const&) const noexcept -> DefinitionData const*;
      LANGULUS_API(RTTI)
      auto GetMetaTagByID(Inner::MetaTagPacked_16 const&) const noexcept -> DefinitionTag const*;
      LANGULUS_API(RTTI)
      auto GetMetaVerbByID(Inner::MetaVerbStructured_X8<1> const&) const noexcept -> DefinitionVerb const*;
      LANGULUS_API(RTTI)
      auto GetMetaVerbByID(Inner::MetaVerbStructured_X8<3> const&) const noexcept -> DefinitionVerb const*;
      LANGULUS_API(RTTI)
      auto GetMetaConstByID(Inner::MetaConstPacked_16 const&) const noexcept -> DefinitionConst const*;

   public:
      ~Registry();

      LANGULUS_API(RTTI)
      auto GetMetaDataByToken (Token const&) const noexcept -> DefinitionData const*;
      LANGULUS_API(RTTI)
      auto GetMetaTagByToken  (Token const&) const noexcept -> DefinitionTag const*;
      LANGULUS_API(RTTI)
      auto GetMetaVerbByToken (Token const&) const noexcept -> DefinitionVerb const*;
      LANGULUS_API(RTTI)
      auto GetMetaConstByToken(Token const&) const noexcept -> DefinitionConst const*;

      LANGULUS_API(RTTI)
      auto GetOperator(Token const&) const noexcept -> DefinitionVerb const*;

      LANGULUS_API(RTTI)
      auto GetAmbiguousMeta(Token const&) const noexcept -> const MetaSet&;

      LANGULUS_API(RTTI)
      auto DisambiguateMeta(Token const&) const -> Inner::Definition const*;

      LANGULUS_API(RTTI)
      auto ResolveFileExtension(Token const&) const -> const MetaSet&;

      LANGULUS_API(RTTI)
      void UnloadBoundary(Token const&);
   };


   ///                                                                        
   ///   The global RTTI registry                                             
   ///                                                                        
   LANGULUS_API(RTTI) extern Registry Instance;

      
   LANGULUS(ALWAYS_INLINED)
   auto GetAmbiguousMeta(Token const& token) noexcept -> const MetaSet& {
      return Instance.GetAmbiguousMeta(token);
   }

   LANGULUS(ALWAYS_INLINED)
   auto DisambiguateMeta(Token const& token) -> Inner::Definition const* {
      return Instance.DisambiguateMeta(token);
   }

   LANGULUS(ALWAYS_INLINED)
   auto& ResolveFileExtension(Token const& token) {
      return Instance.ResolveFileExtension(token);
   }

   LANGULUS(ALWAYS_INLINED)
   void UnloadBoundary(Token const& boundary) {
      Instance.UnloadBoundary(boundary);
   }

} // namespace Langulus::RTTI
