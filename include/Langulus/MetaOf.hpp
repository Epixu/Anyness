///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../source/rtti/DefinitionData.hpp"
#include "../source/rtti/DefinitionTag.hpp"
#include "../source/rtti/DefinitionConst.hpp"
#include "../source/rtti/DefinitionVerb.hpp"


namespace Langulus
{

   /// Get the meta definition of a type, deducing whether it's a data, verb, 
   /// or tag. Note: anything can be data, so meta-data is given only if      
   /// not evaluated to be a tag or verb, which might not be desired. Use one 
   /// of the alternatives below to explicitly state your intent.             
   ///   @tparam T - type to get meta definition of                           
   ///   @return the meta definition of the provided type                     
   template<class T>
   auto MetaOf() {
      if constexpr (CT::DefineTag<Decay<T>>)
         return RTTI::DefinitionTag::Reflect<T>();
      else if constexpr (CT::DefineVerb<Decay<T>>)
         return RTTI::DefinitionVerb::Reflect<T>();
      else
         return RTTI::DefinitionData::Reflect<T>();
   }

   /// Get the meta definition of a constant, like an enum                    
   ///   @tparam E - constant to get meta definition of                       
   ///   @return the meta definition of the provided constant                 
   template<auto E>
   auto MetaOf() {
      return RTTI::DefinitionConst::Reflect<E>();
   }

   /// Data definition retrieval                                              
   /// Some types, like tags/verbs for example, can be represented both as    
   /// DMeta and TMeta/VMeta, and this is useful to state a clear intent      
   ///   @tparam T - type to get data definition from                         
   ///   @return the definition                                               
   template<class T>
   auto MetaDataOf() {
      return RTTI::DefinitionData::Reflect<T>();
   }

   /// Tag definition retrieval                                               
   /// Some types, like tags for example, can be represented both as DMeta    
   /// and TMeta, and this is useful to state a clear intent                  
   ///   @tparam T - type to get tag definition from                          
   ///   @return the definition                                               
   template<class T>
   auto MetaTagOf() {
      return RTTI::DefinitionTag::Reflect<T>();
   }

   /// Verb definition retrieval                                              
   /// Some types, like verbs for example, can be represented both as DMeta   
   /// and VMeta, and this is useful to state a clear intent                  
   ///   @tparam T - type to get verb definition from                         
   ///   @return the definition                                               
   template<class T>
   auto MetaVerbOf() {
      return RTTI::DefinitionVerb::Reflect<T>();
   }

   /// Constant definition retrieval                                          
   ///   @tparam E - constant to get definition from                          
   ///   @return the definition                                               
   template<auto E>
   auto MetaConstOf() {
      return RTTI::DefinitionConst::Reflect<E>();
   }

#if LANGULUS_FEATURE(MANAGED_REFLECTION)
   /// Convenience operators for getting meta definitions from token          
   LANGULUS(INLINED)
   RTTI::DMeta operator ""_dmeta(const char* token, ::std::size_t size) noexcept {
      return RTTI::GetMetaData(Token {token, size});
   }

   LANGULUS(INLINED)
   RTTI::TMeta operator ""_tmeta(const char* token, ::std::size_t size) noexcept {
      return RTTI::GetMetaTag(Token {token, size});
   }

   LANGULUS(INLINED)
   RTTI::CMeta operator ""_cmeta(const char* token, ::std::size_t size) noexcept {
      return RTTI::GetMetaConstant(Token {token, size});
   }

   LANGULUS(INLINED)
   RTTI::VMeta operator ""_vmeta(const char* token, ::std::size_t size) noexcept {
      return RTTI::GetMetaVerb(Token {token, size});
   }

   LANGULUS(INLINED)
   RTTI::AMeta operator ""_meta(const char* token, ::std::size_t size) {
      auto& found = RTTI::GetAmbiguousMeta(Token {token, size});
      if (found.size() == 1)
         return *found.begin();
      else
         LANGULUS_THROW(Meta, "Ambiguous meta literal "
            "- use RTTI::GetAmbiguousMeta and process the result yourself");
   }
#endif

} // namespace Langulus