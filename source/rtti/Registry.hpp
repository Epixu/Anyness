///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Definition.hpp"
//#include <unordered_map>
#include <unordered_set>

#if not LANGULUS_FEATURE(MANAGED_REFLECTION)
   #error "This file shouldn't be included if MANAGED_REFLECTION is disabled"
#endif

namespace Langulus::RTTI
{
   struct MetaException : Exception {
      using Exception::Exception;
   };

   using MetaSet = ::std::unordered_set<Inner::Definition const*>;

   ///                                                                        
   ///   The RTTI registry                                                    
   ///                                                                        
   /// Available only if managed reflection feature is enabled                
   class Registry {
   public:
      // Definitions indexed by lowercase reflected name                
      /*template<class T>
      using MetaMap  = ::std::unordered_map<Token, T>;
      using MetaSet  = ::std::unordered_set<Inner::Definition const*>;

      // Definitions indexed by ID                                      
      template<class T>
      using Indexed = ::std::vector<T>;*/

   private:
      // @attention order of these containers matters!                  
      // Database for meta data definitions                             
      /*MetaMap<::std::unique_ptr<DefinitionData>> mMetaDataByCppName;
      MetaMap<DefinitionData const*>  mMetaDataByToken;
      Indexed<DefinitionData const*>  mMetaDataByID;

      // Database for named values                                      
      MetaMap<::std::unique_ptr<DefinitionConst>> mMetaConstantsByCppName;
      MetaMap<DefinitionConst const*> mMetaConstantsByToken;
      Indexed<DefinitionConst const*> mMetaConstantsByID;

      // Database for meta trait definitions                            
      MetaMap<::std::unique_ptr<DefinitionTag>> mMetaTagsByCppName;
      MetaMap<DefinitionTag const*>   mMetaTagsByToken;
      Indexed<DefinitionTag const*>   mMetaTagsByID;

      // Database for meta verb definitions                             
      MetaMap<::std::unique_ptr<DefinitionVerb>> mMetaVerbsByCppName;
      MetaMap<DefinitionVerb const*>  mMetaVerbsByToken;
      Indexed<DefinitionVerb const*>  mMetaVerbsByID;

      // Database for ambiguous tokens                                  
      // All definitions indexed by their last lowercased token         
      MetaMap<MetaSet> mMetaAmbiguous;
      // Meta data definitions, indexed by file extensions              
      MetaMap<MetaSet> mFileDatabase;*/
      
      //static auto GetMetaByID(const auto& where, size_t id) assumptious;

   protected:
                           friend class DefinitionVerb;
                           friend class DefinitionData;
                           friend class DefinitionTag;
                           friend class DefinitionConst;
                           friend struct Inner::MetaTagPacked_16;
                           friend struct Inner::MetaConstPacked_16;
      template<uint, uint> friend struct Inner::MetaDataStructured_XY;
      template<uint>       friend struct Inner::MetaVerbStructured_X8;

      LANGULUS_API(RTTI)
      static auto RegisterData(Token const& cppname, Token const& token) -> DefinitionData&;
      LANGULUS_API(RTTI)
      static auto ReserveDataID(DefinitionData const*) -> size_t;
      LANGULUS_API(RTTI)
      static auto RegisterConst(Token const& cppname, Token const& token) -> DefinitionConst&;
      LANGULUS_API(RTTI)
      static auto RegisterTag(Token const& cppname, Token const& token) -> DefinitionTag&;
      LANGULUS_API(RTTI)
      static auto RegisterVerb(
         Token const& cppname,
         Token const& token,
         Token const& tokenRev,
         Token const& op,
         Token const& opRev
      ) -> DefinitionVerb&;
      
      LANGULUS_API(RTTI)
      static void RegisterFileExtension(Token const&, DefinitionData*) assumptious;

      LANGULUS_API(RTTI)
      static auto GetMetaDataByCppName(Token const&) noexcept -> DefinitionData const*;
      LANGULUS_API(RTTI)
      static auto GetMetaTagByCppName(Token const&) noexcept -> DefinitionTag const*;
      LANGULUS_API(RTTI)
      static auto GetMetaVerbByCppName(Token const&) noexcept -> DefinitionVerb const*;
      LANGULUS_API(RTTI)
      static auto GetMetaConstByCppName(Token const&) noexcept -> DefinitionConst const*;

      LANGULUS_API(RTTI)
      static auto GetMetaDataByID(size_t, bool sparse, bool constant) assumptious-> DefinitionData const*;
      LANGULUS_API(RTTI)
      static auto GetMetaTagByID(size_t) assumptious-> DefinitionTag const*;
      LANGULUS_API(RTTI)
      static auto GetMetaVerbByID(size_t) assumptious-> DefinitionVerb const*;
      LANGULUS_API(RTTI)
      static auto GetMetaConstByID(size_t) assumptious-> DefinitionConst const*;

   public:
      /*LANGULUS_API(RTTI)
      ~Registry();*/

      LANGULUS_API(RTTI)
      static auto GetMetaDataByToken (Token const&) assumptious -> DefinitionData const*;
      LANGULUS_API(RTTI)
      static auto GetMetaTagByToken  (Token const&) assumptious -> DefinitionTag const*;
      LANGULUS_API(RTTI)
      static auto GetMetaVerbByToken (Token const&) assumptious -> DefinitionVerb const*;
      LANGULUS_API(RTTI)
      static auto GetMetaConstByToken(Token const&) assumptious -> DefinitionConst const*;

      LANGULUS_API(RTTI)
      static auto GetAmbiguousMeta(Token const&) assumptious -> const MetaSet&;
      LANGULUS_API(RTTI)
      static auto DisambiguateMeta(Token const&) -> Inner::Definition const*;
      LANGULUS_API(RTTI)
      static auto ResolveFileExtension(Token const&) assumptious -> const MetaSet&;
      LANGULUS_API(RTTI)
      static void UnloadBoundary(Token const&);
   };


   ///                                                                        
   ///   The global RTTI registry                                             
   /*LANGULUS_API(RTTI) extern Registry Instance;

      
   LANGULUS(ALWAYS_INLINED)
   auto GetAmbiguousMeta(Token const& token) noexcept -> const Registry::MetaSet& {
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
   }*/
}
