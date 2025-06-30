///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "Registry.hpp"
#include "DefinitionData.hpp"
#include "DefinitionTag.hpp"
#include "DefinitionConst.hpp"
#include "DefinitionVerb.hpp"
#include <ranges>

#include "MetaData.hpp"
#include "MetaTag.hpp"

#if not LANGULUS_FEATURE(MANAGED_REFLECTION)
   #error "This file shouldn't be compiled if MANAGED_REFLECTION is disabled"
#endif

#define VERBOSE 0


namespace Langulus::RTTI
{

   Registry Instance {};

   /// Database destruction                                                   
   Registry::~Registry() {
      // If an exception happens here on a delete, then a meta likely   
      // wasn't unregistered upon mod unload. Thank me later.           
      for (auto& definitions : ::std::ranges::views::values(mMetaDataByName))
         for(auto& meta : ::std::ranges::views::values(definitions))
            delete meta;

      for (auto& definitions : ::std::ranges::views::values(mMetaTagsByName))
         for (auto& meta : ::std::ranges::views::values(definitions))
            delete meta;

      for (auto& definitions : ::std::ranges::views::values(mUniqueVerbs))
         for (auto& meta : ::std::ranges::views::values(definitions))
            delete meta;
   }

   /// Common way to extract something from the registry by reflected C++ name
   ///   @param where - where to search in                                    
   ///   @param cppname - the C++ name to search for                          
   ///   @param boundary - the boundary to search in (optional)               
   ///   @return the found element, or nullptr if not found                   
   auto Registry::GetMetaByCppName(
      const auto& where, const Token& cppname, const Token& boundary
   ) const noexcept {
      using R = decltype(where.begin()->second.begin()->second);
      const auto foundToken = where.find(::std::string(cppname));
      if (foundToken == where.end())
         return static_cast<R>(nullptr);

      if (not boundary.empty()) {
         // Search in a specific boundary                               
         const auto foundBoundary = foundToken->second.find(boundary);
         if (foundBoundary == foundToken->second.end())
            return static_cast<R>(nullptr);
         return foundBoundary->second;
      }
   
      // Always prefer the main boundary if available, because it's     
      // more persistent                                                
      const auto foundBoundary = foundToken->second.find(MainBoundary);
      if (foundBoundary != foundToken->second.end())
         return foundBoundary->second;
      if (not foundToken->second.empty())
         return foundToken->second.begin()->second;
      return static_cast<R>(nullptr);
   }

   /// Common way to extract something from the registry by reflected name    
   ///   @param where - where to search in                                    
   ///   @param token - the token to search for                               
   ///   @param boundary - the boundary to search in (optional)               
   ///   @return the found element, or nullptr if not found                   
   auto Registry::GetMetaByToken(
      const auto& where, const Token& token, const Token& boundary
   ) const noexcept {
      using R = decltype(where.begin()->second.begin()->second);
      const auto lc = Inner::ToLowercase(token);
      const auto foundToken = where.find(lc);
      if (foundToken == where.end())
         return static_cast<R>(nullptr);

      if (not boundary.empty()) {
         // Search in a specific boundary                               
         const auto foundBoundary = foundToken->second.find(boundary);
         if (foundBoundary == foundToken->second.end())
            return static_cast<R>(nullptr);
         return foundBoundary->second;
      }
   
      // Always prefer the main boundary if available, because it's     
      // more persistent                                                
      const auto foundBoundary = foundToken->second.find(MainBoundary);
      if (foundBoundary != foundToken->second.end())
         return foundBoundary->second;
      if (not foundToken->second.empty())
         return foundToken->second.begin()->second;
      return static_cast<R>(nullptr);
   }
   
   /// Common way to extract something from the registry by ID                
   ///   @param where - where to search in                                    
   ///   @param id - the id to search for                                     
   ///   @return the found element, or nullptr if not found                   
   auto Registry::GetMetaByID(const auto& where, size_t id) const noexcept {
      if (id == 0)
         return static_cast<TypeOf<decltype(where)>>(nullptr);
      return where[id-1];
   }

   /// Get a list of all the interpretations for an ambiguous token           
   /// These can be data types, verbs, traits, or constants                   
   ///   @param where - the map to search for the token in                    
   ///   @param token - the token to search for                               
   ///   @param boundary - the boundary to search in (optional)               
   ///   @return the list of associated meta definitions                      
   auto Registry::GetMetaList(
      const auto& where, const Token& token, const Token& boundary
   ) const noexcept -> const MetaList& {
      static const MetaList fallback {};
      const auto lc = Inner::ToLowercase(Inner::ToLastToken(token));
      const auto foundToken = where.find(lc);
      if (foundToken == where.end())
         return fallback;

      if (not boundary.empty()) {
         // Search in a specific boundary                               
         const auto foundBoundary = foundToken->second.find(boundary);
         if (foundBoundary == foundToken->second.end())
            return fallback;
         return foundBoundary->second;
      }

      // Always prefer the MAIN boundary, because it's persistent       
      const auto foundBoundary = foundToken->second.find(MainBoundary);
      if (foundBoundary != foundToken->second.end())
         return foundBoundary->second;
      if (not foundToken->second.empty())
         return foundToken->second.begin()->second;
      return fallback;
   }

   /// Get an existing meta data definition by its token and boundary         
   ///   @param token - the token of the data definition                      
   ///   @param library - the boundary to search in (optional)                
   ///   @return the definition, or nullptr if not found                      
   auto Registry::GetMetaData(const Token& token, const Token& library)
   const noexcept -> DefinitionData const* {
      return GetMetaByToken(mMetaDataByName, token, library);
   }

   auto Registry::GetMetaData(const Inner::MetaDataStructured_8_8& id)
   const noexcept -> DefinitionData const* {
      return GetMetaByID(mMetaDataByID, id.mHandle[0]);
   }

   auto Registry::GetMetaData(const Inner::MetaDataStructured_16_16& id)
   const noexcept -> DefinitionData const* {
      size_t id_processed;
      memcpy(&id_processed, id.mHandle, sizeof(id.mHandle));
      return GetMetaByID(mMetaDataByID, id_processed);
   }

   auto Registry::GetMetaData(const Inner::MetaDataStructured_24_8& id)
   const noexcept -> DefinitionData const* {
      size_t id_processed;
      memcpy(&id_processed, id.mHandle, sizeof(id.mHandle));
      return GetMetaByID(mMetaDataByID, id_processed);
   }

   /// Get an existing meta constant definition by its token and boundary     
   ///   @param token - the token of the constant definition                  
   ///   @param boundary - the boundary to search in (optional)               
   ///   @return the definition, or nullptr if not found                      
   auto Registry::GetMetaConst(const Token& token, const Token& boundary)
   const noexcept -> DefinitionConst const* {
      return GetMetaByToken(mMetaConstantsByName, token, boundary);
   }

   /// Get an existing meta tag definition by its token and boundary          
   ///   @param token - the token of the tag definition                       
   ///   @param boundary - the boundary to search in (optional)               
   ///   @return the definition, or nullptr if not found                      
   auto Registry::GetMetaTag(const Token& token, const Token& boundary)
   const noexcept -> DefinitionTag const* {
      return GetMetaByToken(mMetaTagsByName, token, boundary);
   }

   /// Get an existing meta verb definition by its token and boundary         
   ///   @param token - the token of the verb definition                      
   ///                  you can search by positive, as well as negative token 
   ///   @param boundary - the boundary to search in (optional)               
   ///   @return the definition, or nullptr if not found                      
   auto Registry::GetMetaVerb(const Token& token, const Token& boundary)
   const noexcept -> DefinitionVerb const* {
      return GetMetaByToken(mMetaVerbsByName, token, boundary);
   }

   /// Get an existing meta verb definition by its operator token and boundary
   ///   @param token - the operator of the verb definition                   
   ///                  you can search by positive, as well as negative       
   ///   @param boundary - the boundary to search in (optional)               
   ///   @return the definition, or nullptr if not found                      
   auto Registry::GetOperator(const Token& token, const Token& boundary)
   const noexcept -> DefinitionVerb const* {
      const auto lc = Inner::IsolateOperator(token);
      return GetMetaByToken(mOperators, lc, boundary);
   }

   /// Get a list of all the interpretations for an ambiguous token           
   /// These can be data types, verbs, tags, or constants                     
   ///   @param token - the token to search for                               
   ///   @param boundary - the boundary to search in (optional)               
   ///   @return the list of associated meta definitions                      
   auto Registry::GetAmbiguousMeta(
      const Token& token, const Token& boundary
   ) const noexcept -> const MetaList& {
      return GetMetaList(mMetaAmbiguous, token, boundary);
   }
   
   /// Disambiguate a token                                                   
   /// Works in the following way:                                            
   ///   1. Checks keyword for an exact match (not case-sensitive)            
   ///      If such is found, the meta is returned directly                   
   ///   2. If multiple keywords match partially:                             
   ///      a. Meta-data and meta-tags are always with higher priority than   
   ///         meta-verbs and meta-constants.                                 
   ///      b. A keyword starting with a capital letter is always hinted as   
   ///         meta-data, instead of meta-tag.                                
   ///   3. If after all these disambiguation attempts there's still ambiguity
   ///      throw an exception - the ambiguity has to be manually fixed       
   ///   @param keyword - the token to search for                             
   ///   @param boundary - the boundary to search in (optional)               
   ///   @return the disambiguated token; throws if not found/ambiguous       
   auto Registry::DisambiguateMeta(const Token& keyword, const Token& boundary)
   const -> Inner::Definition const* {
      auto& symbols = GetAmbiguousMeta(keyword, boundary);
      Assert(not symbols.empty(), HERE(),
         "Keyword not found", ": `", keyword, '`');
      
      if (symbols.size() == 1) {
         // No ambiguity, just return the single result (1)             
         return *symbols.begin();
      }

      // Collect all origin types, and work with those                  
      // Also, GetAmbiguousMeta works only with the last part of the    
      // keyword, but the keyword might contain hints as to which       
      // ambiguous meta to pick. Discard symbols that do not            
      // contain the provided keyword (not case sensitive)              
      const auto lowercased = Inner::ToLowercase(keyword);
      MetaList origins;
      for (auto& meta : symbols) {
         AssumeDevAndOptimize(meta, "Bad meta");
         const auto meta_lc_token = meta->mTokenSanitized;
         if (meta_lc_token.find(lowercased) == ::std::string::npos)
            continue;
         
         if (auto dmeta = dynamic_cast<DefinitionData const*>(meta)) {
            if (dmeta->mOrigin)
               origins.insert(dmeta->mOrigin);
            else
               origins.insert(meta);
         }
         else origins.insert(meta);
      }

      Assert(not origins.empty(), HERE(),
         "No relevant origins for keyword", ": `", keyword, '`');

      DefinitionData const* meta_data             IF_SAFE(= nullptr);
      DefinitionData const* meta_data_exact_match IF_SAFE(= nullptr);
      DefinitionTag  const* meta_tag              IF_SAFE(= nullptr);
      size_t meta_data_encountered = 0;
      size_t meta_tag_encountered  = 0;

      if (origins.size() == 1) {
         // Candidate types reduced to a single relevant origin (1)     
         return *origins.begin();
      }

      for (auto& candidate : origins) {
         // There's a chance, that one of the symbols matches the       
         // lowercased keyword exactly (1)                              
         auto dmeta = dynamic_cast<DefinitionData const*>(candidate);
         if (candidate->mTokenSanitized == lowercased and dmeta)
            meta_data_exact_match = dmeta;

         if (dmeta) {
            meta_data = dmeta;
            ++meta_data_encountered;
         }
         else if (auto tmeta = dynamic_cast<DefinitionTag const*>(candidate)) {
            meta_tag = tmeta;
            ++meta_tag_encountered;
         }
      }

      // If there are data/traits available, discard verbs/consts (2.a) 
      if (meta_data_encountered and meta_tag_encountered) {
         // Both data and traits encountered, check first letter (2.b)  
         if (::std::islower(keyword[0])) {
            if (meta_tag_encountered == 1)
               return meta_tag;
         }
         else {
            if (meta_data_encountered == 1)
               return meta_data;

            if (meta_data_exact_match)
               return meta_data_exact_match;
         }
      }
      else if (meta_data_encountered == 1) {
         // No traits, just meta data                                   
         // If it's just one, directly return it (2.a)                  
         return meta_data;
      }
      else if (meta_data_exact_match) {
         // If there was an exact match - now's the time to return it   
         return meta_data_exact_match;
      }
      else if (meta_tag_encountered == 1) {
         // No data, just meta traits                                   
         // If it's just one, directly return it (2.a)                  
         return meta_tag;
      }

      // Unfixable ambiguity reached, report error and throw (3)        
      const auto tab = Logger::ErrorScoped(
         "Ambiguous symbol: `", keyword, "`; Could be one of: "
      );
      
      for (auto& meta : origins) {
         Logger::Line('`', Logger::PushDarkYellow,
            meta->mToken, Logger::Pop, '`');
         
         if (dynamic_cast<DefinitionData const*>(meta))
            Logger::Append(" (data)");
         else if (dynamic_cast<DefinitionTag const*>(meta))
            Logger::Append(" (tag)");
         else if (dynamic_cast<DefinitionVerb const*>(meta))
            Logger::Append(" (verb)");
         else if (dynamic_cast<DefinitionConst const*>(meta))
            Logger::Append(" (constant)");
      }
      
      throw MetaException {"Ambiguous symbol", HERE()};
   }

   /// Resolve a file extension                                               
   ///   @param token - the file extension to search for                      
   ///   @param boundary - the boundary to search in (optional)               
   ///   @return all meta definitions associated with the file extension      
   auto Registry::ResolveFileExtension(
      const Token& token, const Token& boundary
   ) const -> const MetaList& {
      return GetMetaList(mFileDatabase, token, boundary);
   }
   
   /// Register most relevant token to the ambiguous token map                
   ///   @param boundary - the boundary to register in                        
   ///   @param token - the token to register                                 
   ///   @param meta - the definition to add                                  
   void Registry::RegisterAmbiguous(
      const Token& boundary, const Lowercase& token, Inner::Definition const* meta
   ) noexcept {
      Lowercase ambiguous {Inner::ToLastToken(token)};
      const auto foundAmbiguous = mMetaAmbiguous.find(ambiguous);
      if (foundAmbiguous == mMetaAmbiguous.end()) {
         mMetaAmbiguous.insert({MOV(ambiguous), {{boundary, {meta}}}});
         return;
      }

      const auto foundBoundary = foundAmbiguous->second.find(boundary);
      if (foundBoundary == foundAmbiguous->second.end())
         foundAmbiguous->second.insert({boundary, {meta}});
      else
         foundBoundary->second.insert(meta);
   }

   /// Unregister most relevant token from the ambiguous token map            
   ///   @attention only definitions in current boundary are affected         
   ///   @param boundary - the boundary to unregister from                    
   ///   @param token - the token to unregister                               
   ///   @param meta - the definition to remove                               
   void Registry::UnregisterAmbiguous(
      const Token& boundary, const Lowercase& token, Inner::Definition const* meta
   ) noexcept {
      Lowercase ambiguous {Inner::ToLastToken(token)};
      const auto foundAmbiguous = mMetaAmbiguous.find(ambiguous);
      if (foundAmbiguous == mMetaAmbiguous.end())
         return;
      
      const auto foundBoundary = foundAmbiguous->second.find(boundary);
      if (foundBoundary == foundAmbiguous->second.end())
         return;

      foundBoundary->second.erase(meta);
      
      if (foundBoundary->second.empty())
         foundAmbiguous->second.erase(foundBoundary);
      if (foundAmbiguous->second.empty())
         mMetaAmbiguous.erase(foundAmbiguous);
   }

   /// A common function to register a specific type in a specific place      
   ///   @param meta - the new meta definition to register                    
   ///   @param where - place to insert the definition                        
   ///   @param boundary - the boundary to register in                        
   ///   @return the newly defined meta for that token                        
   auto& Registry::Register(auto meta, auto& where, const Token& boundary) has_assumptions {
      AssumeDev(not boundary.empty(), HERE(), "Bad boundary");

      // If reached, then not found, so insert a new definition
      const ::std::string cppname {meta->mCppName};
      const auto foundToken = where.find(cppname);
      if (foundToken == where.end())
         where.insert({cppname, {{boundary, meta}}});
      else
         foundToken->second.insert({boundary, meta});
      return *meta;
   }

   /// Register a data definition                                             
   ///   @attention assumes type is not yet registered in the given boundary  
   ///   @param cppname - the C++ type name to register                       
   ///   @param boundary - the boundary to register in                        
   ///   @return the newly defined meta data for that name                    
   auto Registry::RegisterData(const Token& cppname, const Token& boundary) -> DefinitionData& {
      AssumeDev(not GetMetaByCppName(mMetaDataByName, cppname, boundary), HERE(),
         "Data with this name is already registered: ", cppname);
      
      Assert(not GetMetaByCppName(mMetaTagsByName, cppname), HERE(),
         "Data name conflicts with tag: ", cppname);
      Assert(not GetMetaByCppName(mMetaVerbsByName, cppname), HERE(),
         "Data name conflicts with verb: ", cppname);
      Assert(not GetMetaByCppName(mMetaConstantsByName, cppname), HERE(),
         "Data name conflicts with constant: ", cppname);

      return Register(new DefinitionData {cppname}, mMetaDataByName, boundary);
   }

   /// Register a constant definition                                         
   ///   @attention assumes token is not yet registered in the given boundary 
   ///   @param cppname - the C++ type name to register                       
   ///   @param boundary - the boundary to register in                        
   ///   @return the newly defined meta constant for that token               
   auto Registry::RegisterConst(const Token& cppname, const Token& boundary) -> DefinitionConst& {
      AssumeDev(not GetMetaByCppName(mMetaConstantsByName, cppname, boundary), HERE(),
         "Constant with this name is already registered: ", cppname);

      Assert(not GetMetaByCppName(mMetaTagsByName, cppname), HERE(),
         "Constant name conflicts with tag: ", cppname);
      Assert(not GetMetaByCppName(mMetaVerbsByName, cppname), HERE(),
         "Constant name conflicts with verb: ", cppname);
      Assert(not GetMetaByCppName(mMetaConstantsByName, cppname), HERE(),
         "Constant name conflicts with data: ", cppname);

      return Register(new DefinitionConst {cppname}, mMetaConstantsByName, boundary);
   }

   /// Register a trait definition                                            
   ///   @attention assumes token is not yet registered in the given boundary 
   ///   @param cppname - the C++ type name to register                       
   ///   @param boundary - the boundary to register in                        
   ///   @return the newly defined meta trait for that token                  
   auto Registry::RegisterTag(const Token& cppname, const Token& boundary) -> DefinitionTag& {
      AssumeDev(not GetMetaByCppName(mMetaTagsByName, cppname, boundary), HERE(),
         "Tag with this name is already registered: ", cppname);

      Assert(not GetMetaByCppName(mMetaConstantsByName, cppname), HERE(),
         "Tag name conflicts with constant: ", cppname);
      Assert(not GetMetaByCppName(mMetaVerbsByName, cppname), HERE(),
         "Tag name conflicts with verb: ", cppname);
      Assert(not GetMetaByCppName(mMetaDataByName, cppname), HERE(),
         "Tag name conflicts with data: ", cppname);

      return Register(new DefinitionTag {cppname}, mMetaTagsByName, boundary);
   }

   /// Register a verb definition                                             
   ///   @attention assumes tokens are not yet registered                     
   ///   @param cppname - the C++ type name to register                       
   ///   @param boundary - the boundary to register in                        
   ///   @return the newly defined meta verb for that token configuration     
   auto Registry::RegisterVerb(const Token& cppname, const Token& boundary) -> DefinitionVerb& {
      AssumeDev(not GetMetaByCppName(mUniqueVerbs, cppname, boundary), HERE(),
         "Verb with this name is already registered: ", cppname);

      Assert(not GetMetaByCppName(mMetaConstantsByName, cppname), HERE(),
         "Verb name conflicts with constant: ", cppname);
      Assert(not GetMetaByCppName(mMetaTagsByName, cppname), HERE(),
         "Verb name conflicts with tag: ", cppname);
      Assert(not GetMetaByCppName(mMetaDataByName, cppname), HERE(),
         "Verb name conflicts with data: ", cppname);

      return Register(new DefinitionVerb {cppname}, mUniqueVerbs, boundary);
      
      /*AssumeDev(not boundary.empty(), HERE(),
         "Bad boundary provided");
      const auto cppnamelc = Inner::ToLowercase(cppname);

      IF_SAFE(const auto uniqueFound = mUniqueVerbs.find(cppnamelc));
      AssumeDev(uniqueFound == mUniqueVerbs.end()
         or not uniqueFound->second.contains(boundary), HERE(),
         "Verb already registered for that boundary");

      auto lc1 = Inner::ToLowercase(token);
      AssumeDev(not GetMetaVerb(lc1, boundary), HERE(),
         "Verb already registered with token: ",token);

      Lowercase lc2;
      if (not tokenReverse.empty()) {
         lc2 = Inner::ToLowercase(tokenReverse);
         AssumeDev(not GetMetaVerb(lc2, boundary), HERE(),
            "Verb already registered with token: ", tokenReverse);
      }

      Assert(not GetMetaConst(token), HERE(),
         "Verb positive token conflicts with constant: ", token);
      Assert(not GetMetaTag(token), HERE(),
         "Verb positive token conflicts with trait: ", token);
      Assert(not GetMetaData(token), HERE(),
         "Verb positive token conflicts with data: ", token);

      Assert(not GetMetaConst(tokenReverse), HERE(),
         "Verb negative token conflicts with constant: ", tokenReverse);
      Assert(not GetMetaTag(tokenReverse), HERE(),
         "Verb negative token conflicts with trait: ", tokenReverse);
      Assert(not GetMetaData(tokenReverse), HERE(),
         "Verb negative token conflicts with data: ", tokenReverse);

      Lowercase op1;
      if (not op.empty()) {
         op1 = Inner::IsolateOperator(op);
         AssumeDev(not GetOperator(op1, boundary), HERE(),
            "Positive operator already registered");

         Assert(not GetMetaConst(op1), HERE(),
            "Verb positive operator conflicts with constant: ", op1);
         Assert(not GetMetaTag(op1), HERE(),
            "Verb positive operator conflicts with trait: ", op1);
         Assert(not GetMetaData(op1), HERE(),
            "Verb positive operator conflicts with data: ", op1);
      }

      Lowercase op2;
      if (not opReverse.empty()) {
         op2 = Inner::IsolateOperator(opReverse);
         AssumeDev(not GetOperator(op2, boundary), HERE(),
            "Negative operator already registered");

         Assert(not GetMetaConst(op2), HERE(),
            "Verb positive operator conflicts with constant: ", op2);
         Assert(not GetMetaTag(op2), HERE(),
            "Verb positive operator conflicts with trait: ", op2);
         Assert(not GetMetaData(op2), HERE(),
            "Verb positive operator conflicts with data: ", op2);
      }

      const auto meta = Register<false>(
         new DefinitionVerb {token, tokenReverse, op, opReverse},
         mUniqueVerbs, cppnamelc, boundary
      );

      if (tokenReverse.empty())
         Logger::Verbose<VERBOSE>("Verb ", token, " registered");
      else
         Logger::Verbose<VERBOSE>("Verb ", token, '/', tokenReverse, " registered");

      Register(meta, mMetaVerbsByName, lc1, boundary);

      if (not lc2.empty())
         Register(meta, mMetaVerbsByName, lc2, boundary);

      if (not op1.empty()) {
         Register<false>(meta, mOperators, op1, boundary);
         Logger::Verbose<VERBOSE>("Operator ", op1, " registered");
      }

      if (not op2.empty()) {
         Register<false>(meta, mOperators, op2, boundary);
         Logger::Verbose<VERBOSE>("Operator ", op2, " registered");
      }

      return meta;*/
   }

   /// Register file extension                                                
   ///   @param token - the file extension token to reserve                   
   ///   @param type - the data to associate file with                        
   ///   @param boundary - the boundary to register in                        
   void Registry::RegisterFileExtension(
      const Token& token, DefinitionData* type, const Token& boundary
   ) has_assumptions {
      AssumeDev(not token.empty(), HERE(),
         "Bad file extension");
      AssumeDevAndOptimize(type,
         "Bad meta data for file extension ", token);
      AssumeDev(not boundary.empty(), HERE(),
         "Bad boundary provided");

      const auto lc = Inner::ToLowercase(token);
      const auto foundToken = mFileDatabase.find(lc);
      if (foundToken == mFileDatabase.end()) {
         mFileDatabase[lc].insert({boundary, {type}});
         return;
      }

      const auto foundBoundary = foundToken->second.find(boundary);
      if (foundBoundary == foundToken->second.end())
         foundToken->second.insert({boundary, {type}});
      else
         foundBoundary->second.insert(type);
   }

   /// Runs through all definitions, and destroys all of those, that were     
   /// defined within the given boundary token                                
   ///   @param boundary - the boundary token to search for                   
   void Registry::UnloadBoundary(const Token& boundary) {
      Logger::Verbose<VERBOSE>(Logger::PushRed, Logger::Underline, 
         "Unloading library ", boundary, Logger::Pop);

      // Unload constants                                               
      for (auto pair = mMetaConstantsByName.begin(); pair != mMetaConstantsByName.end();) {
         auto found = pair->second.find(boundary);
         if (found == pair->second.end()) {
            ++pair;
            continue;
         }

         Logger::Verbose<VERBOSE>(
            "Constant ", Logger::PushYellow, found->second->mToken,
            Logger::PopRed, " unregistered (", boundary, ")"
         );

         UnregisterAmbiguous(boundary, pair->first, found->second);
         delete found->second;
         pair->second.erase(found);
         if (pair->second.empty())
            pair = mMetaConstantsByName.erase(pair);
         else
            ++pair;
      }

      // Unload file types                                              
      for (auto pair = mFileDatabase.begin(); pair != mFileDatabase.end();) {
         auto found = pair->second.find(boundary);
         if (found == pair->second.end()) {
            ++pair;
            continue;
         }

         Logger::Verbose<VERBOSE>(
            "File ", Logger::PushCyan, pair->first,
            Logger::PopRed, " unregistered (", boundary, ")"
         );

         pair->second.erase(found);
         if (pair->second.empty())
            pair = mFileDatabase.erase(pair);
         else
            ++pair;
      }

      // Unload data types                                              
      for (auto pair = mMetaDataByName.begin(); pair != mMetaDataByName.end();) {
         auto found = pair->second.find(boundary);
         if (found == pair->second.end()) {
            ++pair;
            continue;
         }

         Logger::Verbose<VERBOSE>(
            "Data ", Logger::PushCyan, found->second->mToken,
            Logger::PopRed, " unregistered (", boundary, ")"
         );

         UnregisterAmbiguous(boundary, pair->first, found->second);
         delete found->second;
         pair->second.erase(found);
         if (pair->second.empty())
            pair = mMetaDataByName.erase(pair);
         else
            ++pair;
      }

      // Unload traits                                                  
      for (auto pair = mMetaTagsByName.begin(); pair != mMetaTagsByName.end();) {
         auto found = pair->second.find(boundary);
         if (found == pair->second.end()) {
            ++pair;
            continue;
         }

         Logger::Verbose<VERBOSE>(
            "Trait ", Logger::PushPurple, found->second->mToken,
            Logger::PopRed, " unregistered (", boundary, ")"
         );

         UnregisterAmbiguous(boundary, pair->first, found->second);
         delete found->second;
         pair->second.erase(found);
         if (pair->second.empty())
            pair = mMetaTagsByName.erase(pair);
         else
            ++pair;
      }

      // Unload verbs                                                   
      for (auto pair = mUniqueVerbs.begin(); pair != mUniqueVerbs.end();) {
         auto found = pair->second.find(boundary);
         if (found == pair->second.end()) {
            ++pair;
            continue;
         }

         DefinitionVerb const* definition = found->second;
         auto& lc1 = definition->mTokenSanitized;
         AssumeDev(definition == GetMetaVerb(lc1, boundary), HERE(),
            "Bad VMeta definition"
         );

         auto foundlc1 = mMetaVerbsByName.find(lc1);
         if (foundlc1 != mMetaVerbsByName.end())
            foundlc1->second.erase(boundary);

         auto& lc2 = definition->mTokenReverseSanitized;
         if (not lc2.empty()) {
            AssumeDev(definition == GetMetaVerb(lc2, boundary),
               "Bad VMeta definition"
            );
            
            auto foundlc2 = mMetaVerbsByName.find(lc2);
            if (foundlc2 != mMetaVerbsByName.end())
               foundlc2->second.erase(boundary);
         }

         if (not definition->mOperator.empty()) {
            const auto op1 = Inner::IsolateOperator(definition->mOperator);
            Logger::Verbose<VERBOSE>("Operator ", Logger::PushDarkGreen, op1,
               Logger::PopRed, " unregistered (", boundary, ")");
            
            auto foundop1 = mOperators.find(op1);
            if (foundop1 != mOperators.end())
               foundop1->second.erase(boundary);
         }

         if (not definition->mOperatorReverse.empty()) {
            const auto op2 = Inner::IsolateOperator(definition->mOperatorReverse);
            Logger::Verbose<VERBOSE>("Operator ", Logger::PushDarkGreen, op2,
               Logger::PopRed, " unregistered (", boundary, ")");
            
            auto foundop2 = mOperators.find(op2);
            if (foundop2 != mOperators.end())
               foundop2->second.erase(boundary);
         }

         if (not definition->mTokenReverse.empty()) {
            Logger::Verbose<VERBOSE>("Verb ", Logger::PushDarkGreen,
               definition->mToken, "/", definition->mTokenReverse,
               Logger::PopRed, " unregistered (", boundary, ")"
            );
         }
         else {
            Logger::Verbose<VERBOSE>("Verb ", Logger::PushDarkGreen,
               definition->mToken, Logger::PopRed,
               " unregistered (", boundary, ")"
            );
         }

         UnregisterAmbiguous(boundary, lc1, definition);
         if (not lc2.empty())
            UnregisterAmbiguous(boundary, lc2, definition);
         
         pair->second.erase(found);
         
         if (pair->second.empty())
            pair = mUniqueVerbs.erase(pair);
         else
            ++pair;

         delete definition;
      }
   }

   /// Get the shortest possible unambiguous token                            
   ///   @return the token                                                    
   Token Inner::Definition::GetShortestUnambiguousToken() const {
      auto& ambiguous = Instance.GetAmbiguousMeta(mToken);
      if (ambiguous.size() == 1)
         return Inner::ToLastToken(mToken);

      // Collect all origin types, and work with those                  
      int datas = 0;
      int traits = 0;
      MetaList origins;
      for (auto& meta : ambiguous) {
         const auto dmeta = meta.As<DMeta>();
         if (dmeta and dmeta->mOrigin) {
            origins.insert(dmeta->mOrigin);
            ++datas;
         }
         else {
            origins.insert(meta);

            if (dmeta)
               ++datas;
            else if (meta.Kind() == Meta::Trait)
               ++traits;
         }
      }

      // Some easy to do disambiguations                                
      // Meta datas/tags always win over verbs/constants                
      if (origins.size() == 1)
         return ToLastToken(mToken);
      
      if ((datas  == 1 and traits == 0 and Kind() == Meta::Data)
      or  (traits == 1 and datas  == 0 and Kind() == Meta::Trait))
         return ToLastToken(mToken);
      
      if (datas == 1 and traits == 1) {
         if (Kind() == Meta::Data) {
            // Token should be starting with a capital letter           
            return static_cast<const MetaData*>(this)->mTokenSanitized;
         }

         if (Kind() == Meta::Trait) {
            // Token should be starting with a lower letter             
            return static_cast<const MetaTrait*>(this)->mTokenSanitized;
         }
      }

      // Start including namespaces, until the resulting token has      
      // exactly one match inside the ambiguous list                    
      auto start = ToLastToken(mToken).data() - 3;
      while (start >= mToken.data()) {
         if (*start == ':') {
            const auto candidate = mToken.substr(start - mToken.data() + 1);
            Count matches = 0;
            for (auto& meta : origins) {
               if (meta->mToken.ends_with(candidate)) {
                  if (++matches > 1)
                     break;
               }
            }

            if (matches == 1) {
               // Match found                                           
               return candidate;
            }

            start -= 2;
         }

         --start;
      }

      // Full token returned as fallback                                
      return mToken;
   }

} // namespace Langulus::RTTI
