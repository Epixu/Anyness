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

   using MetaList = ::std::unordered_set<Inner::Definition*>;


   ///                                                                        
   ///   The RTTI registry                                                    
   ///                                                                        
   /// Available only if managed reflection feature is enabled                
   ///                                                                        
   class Registry {
   private:
      template<class T>
      using BoundedMeta = ::std::unordered_map<Token, T>;
      template<class T>
      using MetaMap = ::std::unordered_map<Lowercase, BoundedMeta<T>>;

      // Database for meta data definitions                             
      MetaMap<DefinitionData*>  mMetaData;
      // Database for named values                                      
      MetaMap<DefinitionConst*> mMetaConstants;
      // Database for meta trait definitions                            
      MetaMap<DefinitionTag*>   mMetaTags;
      // Database for meta verb definitions                             
      MetaMap<DefinitionVerb*>  mMetaVerbs;

      // Verbs, mapped to their original C++ class name                 
      MetaMap<DefinitionVerb*> mUniqueVerbs;
      // Database for verb definitions indexed by operator token        
      MetaMap<DefinitionVerb*> mOperators;
      // Database for ambiguous tokens                                  
      MetaMap<MetaList> mMetaAmbiguous;
      // Meta data definitions, indexed by file extensions              
      MetaMap<MetaList> mFileDatabase;

      void RegisterAmbiguous(const Token&, const Lowercase&, Inner::Definition*) noexcept;
      void UnregisterAmbiguous(const Token&, const Lowercase&, Inner::Definition*) noexcept;
      auto GetMeta(const auto&, const Token&, const Token&) const noexcept;
      auto GetMetaList(const auto&, const Token&, const Token&) const noexcept -> const MetaList&;

   protected:
      friend class DefinitionVerb;

      void RegisterVerbOperator(const Token&, const Token& library) has_assumptions;
      void RegisterVerbOperatorReverse(const Token&, const Token& library) has_assumptions;
      void RegisterVerbToken(const Token&, const Token& library) has_assumptions;
      void RegisterVerbTokenReverse(const Token&, const Token& library) has_assumptions;

   public:
      LANGULUS_API(RTTI)
      auto RegisterData(const Token& name, const Token& library) -> DefinitionData&;

      LANGULUS_API(RTTI)
      auto RegisterConst(const Token& name, const Token& library) -> DefinitionConst&;

      LANGULUS_API(RTTI)
      auto RegisterTag(const Token& name, const Token& library) -> DefinitionTag&;

      LANGULUS_API(RTTI)
      auto RegisterVerb(const Token&, const Token&, const Token&, const Token&, const Token&, const Token&) -> DefinitionVerb&;
      
      LANGULUS_API(RTTI)
      void RegisterFileExtension(const Token&, DefinitionData*, const Token& library) has_assumptions;

   public:
      ~Registry();

      LANGULUS_API(RTTI)
      auto GetMetaData(const Token&, const Token& library = "") const noexcept -> DefinitionData const*;
      LANGULUS_API(RTTI)
      auto GetMetaData(const Inner::MetaDataStructured_8_8&)    const noexcept -> DefinitionData const*;
      LANGULUS_API(RTTI)
      auto GetMetaData(const Inner::MetaDataStructured_16_16&)  const noexcept -> DefinitionData const*;
      LANGULUS_API(RTTI)
      auto GetMetaData(const Inner::MetaDataStructured_24_8&)   const noexcept -> DefinitionData const*;

      LANGULUS_API(RTTI)
      auto GetMetaTag(const Token&, const Token& library = "") const noexcept -> DefinitionTag const*;
      LANGULUS_API(RTTI)
      auto GetMetaTag(const MetaTag&) const noexcept -> DefinitionTag const*;

      LANGULUS_API(RTTI)
      auto GetMetaVerb(const Token&, const Token& library = "") const noexcept -> DefinitionVerb const*;
      LANGULUS_API(RTTI)
      auto GetMetaVerb(const MetaVerb&) const noexcept -> DefinitionVerb const*;

      LANGULUS_API(RTTI)
      auto GetMetaConst(const Token&, const Token& library = "") const noexcept -> DefinitionConst const*;
      LANGULUS_API(RTTI)
      auto GetMetaConst(const MetaConst&) const noexcept -> DefinitionConst const*;

      LANGULUS_API(RTTI)
      auto GetOperator(const Token&, const Token& library = "") const noexcept -> DefinitionVerb const*;

      LANGULUS_API(RTTI)
      auto GetAmbiguousMeta(const Token&, const Token& library = "") const noexcept -> const MetaList&;

      LANGULUS_API(RTTI)
      auto DisambiguateMeta(const Token&, const Token& library = "") const -> Inner::Definition const*;

      LANGULUS_API(RTTI)
      auto ResolveFileExtension(const Token&, const Token& library = "") const -> const MetaList&;

      LANGULUS_API(RTTI)
      void UnloadBoundary(const Token&);
   };


   ///                                                                        
   ///   The global RTTI registry                                             
   ///                                                                        
   LANGULUS_API(RTTI) extern Registry Instance;


   ///                                                                        
   ///   Boundary identifier, local to every shared library/executable        
   ///   It's a simple compile-time string, that is attached upon data        
   /// reflection, so that RTTI can track from which library a type was       
   /// reflected, and thus unregister it when shared object is unloaded.      
   /// The boundary also affects pooling tactics, because if boundary is not  
   /// equal exactly to RTTI::MainBoundary, pooling will be PoolTactic::Type  
   /// by default, so that allocation that happens from external libraries    
   /// can be easily tracked and not pollute other pools                      
   ///                                                                        
   extern Token Boundary;

   
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
