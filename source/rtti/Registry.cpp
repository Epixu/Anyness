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
      for (auto& definition : ::std::ranges::views::values(mMetaDataByName))
            delete definition;

      for (auto& definition : ::std::ranges::views::values(mMetaTagsByName))
            delete definition;

      for (auto& definition : ::std::ranges::views::values(mMetaVerbsByCppName))
            delete definition;
   }

   /// Common way to extract something from the registry by NameOf or by      
   /// CppNameOf. The latter is faster, as no token normalization is applied  
   ///   @tparam BY_CPPNAME - true if token is provided from CppNameOf, and   
   ///      no normalization is required. Used mostly internally.             
   ///   @param where - where to search in                                    
   ///   @param token - the token to search for                               
   ///   @param boundary - the boundary to search in (optional)               
   ///   @return the found element, or nullptr if not found                   
   template<bool BY_CPPNAME>
   auto Registry::GetMetaByName(const auto& where, const Token& token)
   const noexcept -> decltype(where.begin()->second) {
      const ::std::string lc {BY_CPPNAME ? token : Inner::ToLowercase(token)};
      const auto foundToken = where.find(lc);
      if (foundToken == where.end())
         return nullptr;
      return foundToken->second;
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

   /// Get an existing data definition by its CppNameOf                       
   ///   @param token - the C++ name of the data definition                   
   ///   @return the definition, or nullptr if not found                      
   auto Registry::GetMetaDataByCppName(const Token& token)
   const noexcept -> DefinitionData const* {
      return GetMetaByName<true>(mMetaDataByName, token);
   }

   /// Get an existing data definition by its NameOf                          
   ///   @param token - the reflected token of the data definition            
   ///   @return the definition, or nullptr if not found                      
   auto Registry::GetMetaDataByToken(const Token& token)
   const noexcept -> DefinitionData const* {
      return GetMetaByName<false>(mMetaDataByName, token);
   }

   /// Get an existing constant definition by its CppNameOf                   
   ///   @param token - the C++ name of the constant definition               
   ///   @return the definition, or nullptr if not found                      
   auto Registry::GetMetaConstByCppName(const Token& token)
   const noexcept -> DefinitionConst const* {
      return GetMetaByName<true>(mMetaConstantsByName, token);
   }

   /// Get an existing constant definition by its NameOf                      
   ///   @param token - the reflected token of the constant definition        
   ///   @return the definition, or nullptr if not found                      
   auto Registry::GetMetaConstByToken(const Token& token)
   const noexcept -> DefinitionConst const* {
      return GetMetaByName<false>(mMetaConstantsByName, token);
   }

   /// Get an existing tag definition by its CppNameOf                        
   ///   @param token - the C++ name of the tag definition                    
   ///   @return the definition, or nullptr if not found                      
   auto Registry::GetMetaTagByCppName(const Token& token)
   const noexcept -> DefinitionTag const* {
      return GetMetaByName<true>(mMetaTagsByName, token);
   }

   /// Get an existing tag definition by its NameOfTag                        
   ///   @param token - the reflected token of the tag definition             
   ///   @return the definition, or nullptr if not found                      
   auto Registry::GetMetaTagByToken(const Token& token)
   const noexcept -> DefinitionTag const* {
      return GetMetaByName<false>(mMetaTagsByName, token);
   }
   
   /// Get an existing verb definition by its CppNameOf                       
   ///   @param token - the C++ name of the verb definition                   
   ///   @return the definition, or nullptr if not found                      
   auto Registry::GetMetaVerbByCppName(const Token& token)
   const noexcept -> DefinitionVerb const* {
      return GetMetaByName<true>(mMetaVerbsByCppName, token);
   }

   /// Get an existing verb definition by NameOfVerb/NameOfVerbReverse        
   ///   @param token - the reflected token of the verb definition            
   ///                  you can search by positive, as well as negative token 
   ///   @return the definition, or nullptr if not found                      
   auto Registry::GetMetaVerbByToken(const Token& token)
   const noexcept -> DefinitionVerb const* {
      return GetMetaByName<false>(mMetaVerbsByTokens, token);
   }
   
   /// Get an existing data definition by unpacking an ID                     
   ///   @param id - the ID                                                   
   ///   @param sparse - is the data type sparse?                             
   ///   @param constant - is the data type constant?                         
   ///   @return the definition, or nullptr if not found                      
   auto Registry::GetMetaDataByID(size_t id, bool sparse, bool constant)
   const noexcept -> DefinitionData const* {
      DefinitionData const* found = GetMetaByID(mMetaDataByID, id);
      if (not found)
         return nullptr;

      if (sparse) {
         AssumeDevAndOptimize(found->mAddPtr,
            "An indirection ID for a type exists, "
            "but no such type has been reflected yet: ", found->mNameOf
         );
         found = found->mAddPtr;
      }

      if (constant) {
         AssumeDevAndOptimize(found->mAddConst,
            "A constant ID for a type exists, "
            "but no such type has been reflected yet: ", found->mNameOf
         );
         found = found->mAddConst;
      }
      
      return found;
   }

   /// Get an existing verb definition by OperatorOfVerb/OperatorOfVerbReverse
   ///   @param token - the reflected operator of the verb definition         
   ///                  you can search by positive, as well as negative       
   ///   @return the definition, or nullptr if not found                      
   auto Registry::GetOperator(const Token& token)
   const noexcept -> DefinitionVerb const* {
      const auto lc = Inner::IsolateOperator(token);
      return GetMetaByName<false>(mMetaVerbsByTokens, lc);
   }

   /// Get a list of all the interpretations for an ambiguous token           
   ///   @param token - the token to search for                               
   ///   @return the set of associated meta definitions                       
   auto Registry::GetAmbiguousMeta(const Token& token)
   const noexcept -> const MetaSet& {
      static const MetaSet fallback {};
      const auto lc = Inner::ToLowercase(Inner::ToLastToken(token));
      const auto foundToken = mMetaAmbiguous.find(lc);
      if (foundToken == mMetaAmbiguous.end())
         return fallback;
      return foundToken->second;
   }
   
   /// Disambiguate a token. Works in the following way:                      
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
   ///   @return the disambiguated token; throws if not found/ambiguous       
   auto Registry::DisambiguateMeta(const Token& keyword)
   const -> Inner::Definition const* {
      auto& symbols = GetAmbiguousMeta(keyword);
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
      MetaSet origins;
      for (auto& meta : symbols) {
         AssumeDevAndOptimize(meta, "Bad meta");
         if (not meta->mNameOfLowercased.contains(lowercased))
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
         if (candidate->mNameOfLowercased == lowercased and dmeta)
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
            meta->mNameOf, Logger::Pop, '`');
         
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
   ///   @return all meta definitions associated with the file extension      
   auto Registry::ResolveFileExtension(const Token& token) const -> const MetaSet& {
      static const MetaSet fallback {};
      const auto lc = Inner::ToLowercase(Inner::ToLastToken(token));
      const auto foundToken = mFileDatabase.find(lc);
      if (foundToken == mFileDatabase.end())
         return fallback;
      return foundToken->second;
   }

   /// Register a data definition                                             
   ///   @attention assumes token is not yet registered                       
   ///   @param cppname - the C++ type name to register                       
   ///   @return the newly defined meta data for that name                    
   auto Registry::RegisterData(const Token& cppname) -> DefinitionData& {
      AssumeDev(not mMetaDataByName.contains(cppname), HERE(),
         "Data with this name is already registered: ", cppname);
      
      Assert(not mMetaTagsByName.contains(cppname), HERE(),
         "Data name conflicts with tag: ", cppname);
      Assert(not mMetaVerbsByCppName.contains(cppname), HERE(),
         "Data name conflicts with verb: ", cppname);
      Assert(not mMetaConstantsByName.contains(cppname), HERE(),
         "Data name conflicts with constant: ", cppname);

      // If reached, then not found, so insert a new definition         
      auto meta = new DefinitionData {cppname};

      // Index by C++ name                                              
      mMetaDataByName[meta->mCppNameOf] = meta;
      return *meta;
   }

   /// Reserves a data ID for more compact representation of metadata         
   /// Used in packed pointers to definitions                                 
   ///   @param meta - the definition to reserve ID for                       
   ///   @attention assumes meta definition is stripped from a single level   
   ///      of indirection, constness and volatileness                        
   ///   @return the new ID                                                   
   auto Registry::ReserveDataID(DefinitionData const* meta) -> size_t {
      mMetaDataByID.push_back(meta);
      return mMetaDataByID.size();
   }

   /// Register a constant definition                                         
   ///   @attention assumes token is not yet registered                       
   ///   @param cppname - the C++ type name to register                       
   ///   @return the newly defined meta constant for that token               
   auto Registry::RegisterConst(const Token& cppname) -> DefinitionConst& {
      AssumeDev(not mMetaConstantsByName.contains(cppname), HERE(),
         "Constant with this name is already registered: ", cppname);

      Assert(not mMetaDataByName.contains(cppname), HERE(),
         "Constant name conflicts with data: ", cppname);
      Assert(not mMetaTagsByName.contains(cppname), HERE(),
         "Constant name conflicts with tag: ", cppname);
      Assert(not mMetaVerbsByCppName.contains(cppname), HERE(),
         "Constant name conflicts with verb: ", cppname);

      // If reached, then not found, so insert a new definition         
      auto meta = new DefinitionConst {cppname};

      // Index by C++ name                                              
      mMetaConstantsByName[meta->mCppNameOf] = meta;

      // Index by ID                                                    
      mMetaConstantsByID.push_back(meta);
      meta->mID = mMetaConstantsByID.size();
      return *meta;
   }

   /// Register a tag definition                                              
   ///   @attention assumes token is not yet registered                       
   ///   @param cppname - the C++ type name to register                       
   ///   @return the newly defined meta trait for that token                  
   auto Registry::RegisterTag(const Token& cppname) -> DefinitionTag& {
      AssumeDev(not mMetaTagsByName.contains(cppname), HERE(),
         "Tag with this name is already registered: ", cppname);

      Assert(not mMetaDataByName.contains(cppname), HERE(),
         "Tag name conflicts with data: ", cppname);
      Assert(not mMetaConstantsByName.contains(cppname), HERE(),
         "Tag name conflicts with constant: ", cppname);
      Assert(not mMetaVerbsByCppName.contains(cppname), HERE(),
         "Tag name conflicts with verb: ", cppname);

      // If reached, then not found, so insert a new definition         
      auto meta = new DefinitionTag {cppname};

      // Index by C++ name                                              
      mMetaTagsByName[meta->mCppNameOf] = meta;

      // Index by ID                                                    
      mMetaTagsByID.push_back(meta);
      meta->mID = mMetaTagsByID.size();
      return *meta;
   }

   /// Register a verb definition                                             
   ///   @attention assumes tokens are not yet registered                     
   ///   @param cppname - the C++ type name to register                       
   ///   @return the newly defined meta verb for that token configuration     
   auto Registry::RegisterVerb(const Token& cppname) -> DefinitionVerb& {
      AssumeDev(not mMetaVerbsByCppName.contains(cppname), HERE(),
         "Verb with this name is already registered: ", cppname);

      Assert(not mMetaDataByName.contains(cppname), HERE(),
         "Verb name conflicts with data: ", cppname);
      Assert(not mMetaConstantsByName.contains(cppname), HERE(),
         "Verb name conflicts with constant: ", cppname);
      Assert(not mMetaTagsByName.contains(cppname), HERE(),
         "Verb name conflicts with tag: ", cppname);

      // If reached, then not found, so insert a new definition         
      auto meta = new DefinitionVerb {cppname};

      // Index by C++ name                                              
      mMetaVerbsByCppName[meta->mCppNameOf] = meta;

      // Index by ID                                                    
      mMetaVerbsByID.push_back(meta);
      meta->mID = mMetaVerbsByID.size();
      return *meta;
      
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
   void Registry::RegisterFileExtension(const Token& token, DefinitionData* type)
   has_assumptions {
      AssumeDev(not token.empty(), HERE(), "Bad file extension");
      AssumeDevAndOptimize(type, "Bad meta data for file extension: ", token);

      const auto lc = Inner::ToLowercase(token);
      const auto foundToken = mFileDatabase.find(lc);
      if (foundToken == mFileDatabase.end())
         mFileDatabase[lc].insert({type});
      else
         foundToken->second.insert(type);
   }

   /// Runs through all definitions and destroys all of those, that were      
   /// defined only within the given boundary token                           
   ///   @param boundary - the boundary token to search for                   
   void Registry::UnloadBoundary(const Token& boundary) {
      AssumeDev(boundary != MainBoundary, HERE(),
         "Can't unload main boundary");
      auto scope = Logger::VerboseScoped<VERBOSE>(Logger::Red, Logger::Underline, 
         "Unloading boundary ", boundary);

      //                                                                
      // Unload constants                                               
      for (auto pair = mMetaConstantsByName.begin(); pair != mMetaConstantsByName.end();) {
         auto definition = const_cast<DefinitionConst*>(pair->second);
         if (not definition->mBoundaries.erase(boundary)) {
            // Boundary is irrelevant for this definition               
            ++pair;
            continue;
         }

         if (not definition->mBoundaries.empty()) {
            // Definition is still used in other boundaries             
            ++pair;
            continue;
         }

         // If this is reached, then it is time to destroy the          
         // definition - it is no longer in use                         
         Logger::Verbose<VERBOSE>(
            "Constant ", Logger::Yellow, definition->mNameOf,
            Logger::Red, " unregistered"
         );

         // Remove from indexing by ID                                  
         if (mMetaConstantsByID[definition->mID] == definition)
            mMetaConstantsByID[definition->mID] = nullptr;

         // Remove from the ambiguity map                               
         const auto ambiguous = mMetaAmbiguous.find(definition->mNameOfLowercased);
         ambiguous->second.erase(definition);
         if (ambiguous->second.empty())
            mMetaAmbiguous.erase(ambiguous);

         // Finally, delete the definition and remove it from registry  
         delete definition;
         pair = mMetaConstantsByName.erase(pair);
      }

      //                                                                
      // Unload file types (must be done before deleting meta data)     
      for (auto pair = mFileDatabase.begin(); pair != mFileDatabase.end();) {
         for (auto def = pair->second.begin(); def != pair->second.end();) {
            if ((*def)->mBoundaries.size() == 1
            and (*def)->mBoundaries.contains(boundary))
               def = pair->second.erase(def);
            else
               ++def;
         }

         Logger::Verbose<VERBOSE>(
            "File ", Logger::Push, pair->first,
            Logger::Red, " unregistered"
         );
         
         if (pair->second.empty())
            pair = mFileDatabase.erase(pair);
         else
            ++pair;
      }

      //                                                                
      // Unload data types                                              
      for (auto pair = mMetaDataByName.begin(); pair != mMetaDataByName.end();) {
         auto definition = const_cast<DefinitionData*>(pair->second);
         if (not definition->mBoundaries.erase(boundary)) {
            // Boundary is irrelevant for this definition               
            ++pair;
            continue;
         }

         if (not definition->mBoundaries.empty()) {
            // Definition is still used in other boundaries, make sure  
            // we pick new function pointers                            
            definition->mOtherBoundaries.erase(boundary);
            definition->mCurrentBoundary = definition->mOtherBoundaries.begin()->second;
            ++pair;
            continue;
         }

         Logger::Verbose<VERBOSE>(
            "Data ", Logger::Cyan, definition->mNameOf,
            Logger::Red, " unregistered"
         );

         // Remove from indexing by ID                                  
         if (mMetaDataByID[definition->mID] == definition)
            mMetaDataByID[definition->mID] = nullptr;

         // Remove from the ambiguity map                               
         const auto ambiguous = mMetaAmbiguous.find(definition->mNameOfLowercased);
         ambiguous->second.erase(definition);
         if (ambiguous->second.empty())
            mMetaAmbiguous.erase(ambiguous);

         // Finally, delete the definition and remove it from registry  
         delete definition;
         pair = mMetaDataByName.erase(pair);
      }

      //                                                                
      // Unload tags                                                    
      for (auto pair = mMetaTagsByName.begin(); pair != mMetaTagsByName.end();) {
         auto definition = const_cast<DefinitionTag*>(pair->second);
         if (not definition->mBoundaries.erase(boundary)) {
            // Boundary is irrelevant for this definition               
            ++pair;
            continue;
         }

         if (not definition->mBoundaries.empty()) {
            // Definition is still used in other boundaries             
            ++pair;
            continue;
         }

         Logger::Verbose<VERBOSE>(
            "Tag ", Logger::Purple, definition->mNameOf,
            Logger::Red, " unregistered"
         );

         // Remove from indexing by ID                                  
         if (mMetaTagsByID[definition->mID] == definition)
            mMetaTagsByID[definition->mID] = nullptr;

         // Remove from the ambiguity map                               
         const auto ambiguous = mMetaAmbiguous.find(definition->mNameOfLowercased);
         ambiguous->second.erase(definition);
         if (ambiguous->second.empty())
            mMetaAmbiguous.erase(ambiguous);

         // Finally, delete the definition and remove it from registry  
         delete definition;
         pair = mMetaTagsByName.erase(pair);
      }

      //                                                                
      // Unload verbs                                                   
      for (auto pair = mMetaVerbsByCppName.begin(); pair != mMetaVerbsByCppName.end();) {
         auto definition = const_cast<DefinitionVerb*>(pair->second);
         if (not definition->mBoundaries.erase(boundary)) {
            // Boundary is irrelevant for this definition               
            ++pair;
            continue;
         }

         if (not definition->mBoundaries.empty()) {
            // Definition is still used in other boundaries             
            ++pair;
            continue;
         }

         if (not definition->mNameOfReverse.empty()) {
            Logger::Verbose<VERBOSE>("Verb ", Logger::DarkGreen,
               definition->mNameOf, "/", definition->mNameOfReverse,
               Logger::Red, " unregistered"
            );
         }
         else {
            Logger::Verbose<VERBOSE>("Verb ", Logger::DarkGreen,
               definition->mNameOf, Logger::Red,
               " unregistered"
            );
         }

         // Remove from indexing by ID                                  
         if (mMetaVerbsByID[definition->mID] == definition)
            mMetaVerbsByID[definition->mID] = nullptr;

         // Remove from the ambiguity map                               
         const auto ambiguous = mMetaAmbiguous.find(definition->mNameOf);
         ambiguous->second.erase(definition);
         if (ambiguous->second.empty())
            mMetaAmbiguous.erase(ambiguous);

         if (not definition->mNameOfReverse.empty()) {
            const auto ambiguous_rev = mMetaAmbiguous.find(definition->mNameOfReverse);
            ambiguous_rev->second.erase(definition);
            if (ambiguous_rev->second.empty())
               mMetaAmbiguous.erase(ambiguous_rev);
         }

         // Remove from the token map                                   
         mMetaVerbsByTokens.erase(definition->mNameOf);
         if (not definition->mNameOfReverse.empty())
            mMetaVerbsByTokens.erase(definition->mNameOfReverse);
         
         if (not definition->mOperator.empty()) {
            Logger::Verbose<VERBOSE>("Operator ", Logger::DarkGreen, definition->mOperator,
               Logger::Red, " unregistered");
            mMetaVerbsByTokens.erase(definition->mOperator);
         }
         
         if (not definition->mOperatorReverse.empty()) {
            Logger::Verbose<VERBOSE>("Operator ", Logger::DarkGreen, definition->mOperatorReverse,
               Logger::Red, " unregistered");
            mMetaVerbsByTokens.erase(definition->mOperatorReverse);
         }

         // Finally, delete the definition and remove it from registry  
         delete definition;
         pair = mMetaVerbsByCppName.erase(pair);
      }
   }

   /// Get the shortest possible unambiguous token                            
   ///   @return the token                                                    
   /*Token Inner::Definition::GetShortestUnambiguousToken() const {
      auto& ambiguous = Instance.GetAmbiguousMeta(mToken);
      if (ambiguous.size() == 1)
         return Inner::ToLastToken(mToken);

      // Collect all origin types, and work with those                  
      int datas = 0;
      int traits = 0;
      MetaList origins;
      for (auto meta : ambiguous) {
         auto dmeta = dynamic_cast<DefinitionData const*>(meta);
         if (dmeta and dmeta->mOrigin) {
            origins.insert(dmeta->mOrigin);
            ++datas;
         }
         else {
            origins.insert(meta);

            if (dmeta)
               ++datas;
            else if (dynamic_cast<DefinitionTag const*>(meta))
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
   }*/

} // namespace Langulus::RTTI
