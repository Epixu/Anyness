///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Definition.hpp"


namespace Langulus::RTTI
{  

   ///                                                                        
   /// A verb definition                                                      
   ///                                                                        
   class DefinitionVerb : public Inner::Definition {
   protected:
      friend struct Inner::MetaVerbNaked;
      template<unsigned>
      friend struct Inner::MetaVerbStructured_X8;

      // Verbs have antonyms, denoted via this 'negative' token         
      // For example, 'Destroy' is the reverse of 'Create'              
      // This is just syntax sugar - reverse token just does mass *= -1 
      const Token mTokenReverse;

      // Verbs can be tokenized as operators - just syntax sugar        
      const Token mOperator;
      const Token mOperatorReverse;

      // Verb's reflected precedence                                    
      Real mPrecedence {};

      // Reflected default verb for mutable context, if available       
      using FDefaultVerbMutable = bool (*)(Anyness::Many&, Flow::Verb&);
      FDefaultVerbMutable mDefaultInvocationMutable {};

      // Reflected default verb for immutable context, if available     
      using FDefaultVerbConstant = bool (*)(const Anyness::Many&, Flow::Verb&);
      FDefaultVerbConstant mDefaultInvocationConstant {};

      // Reflected stateless verb, if available                         
      using FStatelessVerb = bool (*)(Flow::Verb&);
      FStatelessVerb mStatelessInvocation {};

      // A set of data types that are capable of doing the verb         
      using AbleList = ::std::unordered_set<DefinitionData const*>;
      AbleList mAble;

      DefinitionVerb(const Token& cppname) : Definition {cppname} {}

   public:
      template<CT::Decayed>
      static auto Reflect() -> DefinitionVerb const*;
   };

} // namespace Langulus::RTTI

#include "DefinitionVerb.inl"