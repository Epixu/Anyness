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
      using MetaMap = ::std::unordered_map<Lowercase, T>;
      
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
      MetaMap<DefinitionVerb const*>  mMetaVerbsByName;
      Indexed<DefinitionVerb const*>  mMetaVerbsByID;
      // Verbs, mapped to their original C++ class name                 
      MetaMap<DefinitionVerb const*>  mUniqueVerbs;
      // Database for verb definitions indexed by operator token        
      MetaMap<DefinitionVerb const*>  mOperators;

      // Database for ambiguous tokens                                  
      MetaMap<MetaSet> mMetaAmbiguous;
      // Meta data definitions, indexed by file extensions              
      MetaMap<MetaSet> mFileDatabase;

      void UnregisterAmbiguous(const Token&, const Lowercase&, Inner::Definition const*) noexcept;
      template<bool BY_CPPNAME>
      auto GetMetaByName(const auto& where, const Token& name, const Token& library = "") const noexcept -> decltype(where.begin()->second);
      auto GetMetaList  (const auto& where, const Token& name, const Token& library) const noexcept -> const MetaSet&;
      auto GetMetaByID  (const auto& where, size_t id) const noexcept;

   protected:
      friend class DefinitionVerb;
      friend class DefinitionData;
      friend class DefinitionTag;
      friend class DefinitionConst;

      void RegisterAmbiguous          (const Token&, const Lowercase&, Inner::Definition const*) noexcept;
      void RegisterVerbOperator       (const Token&, const Token& library) has_assumptions;
      void RegisterVerbOperatorReverse(const Token&, const Token& library) has_assumptions;
      void RegisterVerbToken          (const Token&, const Token& library) has_assumptions;
      void RegisterVerbTokenReverse   (const Token&, const Token& library) has_assumptions;

      LANGULUS_API(RTTI)
      auto RegisterData(const Token& name, const Token& library) -> DefinitionData&;
      LANGULUS_API(RTTI)
      auto ReserveDataID(DefinitionData const*) -> size_t;

      LANGULUS_API(RTTI)
      auto RegisterConst(const Token& name, const Token& library) -> DefinitionConst&;

      LANGULUS_API(RTTI)
      auto RegisterTag(const Token& name, const Token& library) -> DefinitionTag&;

      LANGULUS_API(RTTI)
      auto RegisterVerb(const Token&name, const Token& library) -> DefinitionVerb&;
      
      LANGULUS_API(RTTI)
      void RegisterFileExtension(const Token&, DefinitionData*, const Token& library) has_assumptions;

   public:
      ~Registry();

      LANGULUS_API(RTTI)
      auto GetMetaDataByCppName(const Token&, const Token& library = "") const noexcept -> DefinitionData const*;
      LANGULUS_API(RTTI)
      auto GetMetaDataByToken(const Token&, const Token& library = "") const noexcept -> DefinitionData const*;
      LANGULUS_API(RTTI)
      auto GetMetaDataByID(const Inner::MetaDataStructured_8_8&) const noexcept -> DefinitionData const*;
      LANGULUS_API(RTTI)
      auto GetMetaDataByID(const Inner::MetaDataStructured_16_16&) const noexcept -> DefinitionData const*;
      LANGULUS_API(RTTI)
      auto GetMetaDataByID(const Inner::MetaDataStructured_24_8&) const noexcept -> DefinitionData const*;

      LANGULUS_API(RTTI)
      auto GetMetaTagByCppName(const Token&, const Token& library = "") const noexcept -> DefinitionTag const*;
      LANGULUS_API(RTTI)
      auto GetMetaTagByToken(const Token&, const Token& library = "") const noexcept -> DefinitionTag const*;
      LANGULUS_API(RTTI)
      auto GetMetaTagByID(const Inner::MetaTagPacked_16&) const noexcept -> DefinitionTag const*;

      LANGULUS_API(RTTI)
      auto GetMetaVerbByCppName(const Token&, const Token& library = "") const noexcept -> DefinitionVerb const*;
      LANGULUS_API(RTTI)
      auto GetMetaVerbByToken(const Token&, const Token& library = "") const noexcept -> DefinitionVerb const*;
      LANGULUS_API(RTTI)
      auto GetMetaVerbByID(const Inner::MetaVerbStructured_X8<1>&) const noexcept -> DefinitionVerb const*;
      LANGULUS_API(RTTI)
      auto GetMetaVerbByID(const Inner::MetaVerbStructured_X8<3>&) const noexcept -> DefinitionVerb const*;

      LANGULUS_API(RTTI)
      auto GetMetaConstByCppName(const Token&, const Token& library = "") const noexcept -> DefinitionConst const*;
      LANGULUS_API(RTTI)
      auto GetMetaConstByToken(const Token&, const Token& library = "") const noexcept -> DefinitionConst const*;
      LANGULUS_API(RTTI)
      auto GetMetaConstByID(const Inner::MetaConstPacked_16&) const noexcept -> DefinitionConst const*;

      LANGULUS_API(RTTI)
      auto GetOperator(const Token&, const Token& library = "") const noexcept -> DefinitionVerb const*;

      LANGULUS_API(RTTI)
      auto GetAmbiguousMeta(const Token&, const Token& library = "") const noexcept -> const MetaSet&;

      LANGULUS_API(RTTI)
      auto DisambiguateMeta(const Token&, const Token& library = "") const -> Inner::Definition const*;

      LANGULUS_API(RTTI)
      auto ResolveFileExtension(const Token&, const Token& library = "") const -> const MetaSet&;

      LANGULUS_API(RTTI)
      void UnloadBoundary(const Token&);
   };


   ///                                                                        
   ///   The global RTTI registry                                             
   ///                                                                        
   LANGULUS_API(RTTI) extern Registry Instance;

      
   LANGULUS(INLINED)
   auto& GetAmbiguousMeta(const Token& token, const Token& boundary = "") noexcept {
      return Instance.GetAmbiguousMeta(token, boundary);
   }

   LANGULUS(INLINED)
   auto DisambiguateMeta(const Token& token, const Token& boundary = "") -> Inner::Definition const* {
      return Instance.DisambiguateMeta(token, boundary);
   }

   LANGULUS(INLINED)
   auto& ResolveFileExtension(const Token& token, const Token& boundary = "") {
      return Instance.ResolveFileExtension(token, boundary);
   }

   LANGULUS(INLINED)
   void UnloadBoundary(const Token& boundary) {
      Instance.UnloadBoundary(boundary);
   }

} // namespace Langulus::RTTI
