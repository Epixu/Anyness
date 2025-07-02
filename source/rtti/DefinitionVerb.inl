///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "DefinitionVerb.hpp"
#include "MetaVerb.hpp"
#include <Langulus/CT/ReflectAs.hpp>
#include <Langulus/CT/DefineTag.hpp>
#include <Langulus/CT/DefineVerb.hpp>
#include <Langulus/Logger.hpp>
#include <optional>


namespace Langulus::RTTI
{

   /// Reflect or return an already reflected verb                            
   ///   @attention when making a shared library and reflecting your verbs    
   ///      at library initialization, it is recommended you mark all other   
   ///      relevant instantiations of this function as extern template, to   
   ///      save on a lot of compiler resources:                              
   ///      https://stackoverflow.com/questions/8130602                       
   ///   @tparam T - the decayed verb to reflect                              
   template<CT::Decayed T>
   auto DefinitionVerb::Reflect() -> DefinitionVerb const* {
      static_assert(CT::Complete<T>,
         "Can't reflect incomplete verb - "
         "make sure you have included the corresponding headers "
         "before the point of reflection. "
         "This could also be triggered due to an incomplete member in T");
      static_assert(CT::Reflectable<T>,
         "Can't reflect verb that was explicitly marked unreflectable");
      static_assert(CT::DefineVerb<T>,
         "Type is not reflected as a verb definition");
      static_assert(not CT::DefineTag<T>,
         "Can't reflect a tag as a verb");
      static_assert(not ::std::is_function_v<T>,
         "Can't reflect this function signature as a verb");

      constexpr auto cppname = CppNameOf<T>();

      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
         // Try to get an already existing definition - the verb might  
         // have been reflected previously in another shared library    
         DefinitionVerb const* meta = Instance.GetMetaVerbByCppName(cppname);
         if (meta and meta->IsInRelevantBoundary())
            return meta;

         DefinitionVerb& definition = meta
            ? const_cast<DefinitionVerb&>(*meta)
            : Instance.RegisterVerb(cppname);
      #else
         // There's no centralized registry when MANAGED_REFLECTION is  
         // disabled, so all we can do is keep a definition on the stack
         // for each translation unit, and rely on runtime checks to    
         // make sure that definitions match between those.             
         static constinit std::optional<DefinitionVerb> s_definition;
         if (s_definition.has_value())
            return &s_definition.value();

         DefinitionVerb& definition = s_definition.emplace(cppname, "");
      #endif

      //                                                                
      // If this is reached, then verb is not defined yet               
      definition.template ReflectCommon<T>();
      
      constexpr auto verbPos = NameOfVerb<T>();
      constexpr auto verbNeg = NameOfVerbReverse<T>();
      static_assert(not verbPos.empty(),
         "Invalid positive verb token is not allowed");
      static_assert(verbPos != verbNeg,
         "Verb can't have the same positive and negative tokens");
      definition.mNameOf        = Inner::ToLowercase(verbPos);
      definition.mNameOfReverse = Inner::ToLowercase(verbNeg);

      constexpr auto opPos = OperatorOfVerb<T>();
      constexpr auto opNeg = OperatorOfVerbReverse<T>();
      static_assert(opPos != opNeg or opPos.empty(),
         "Verb can't have the same positive and negative operators");
      definition.mOperator        = Inner::ToLowercase(opPos);
      definition.mOperatorReverse = Inner::ToLowercase(opNeg);

      if constexpr (CTTI::DefineVerb<T>::Enabled)
         definition.mPrecedence = CTTI::DefineVerb<T>::Precedence;
      else
         definition.mPrecedence = T::CTTI_DefineVerb::Precedence;

      definition.mCurrentBoundary.mDefaultMut = VerbDefaultMutable<T>();
      definition.mCurrentBoundary.mDefault    = VerbDefaultConstant<T>();
      definition.mCurrentBoundary.mStateless  = VerbStateless<T>();

   /*#if LANGULUS_FEATURE(MANAGED_REFLECTION)
      if (definition.mOperator.size()) {
         Instance.RegisterVerbOperator(definition.mOperator);
         const auto op = IsolateOperator(definition.mOperator);
         Logger::VerboseRaw(
            "Operator ", Logger::DarkGreen, op,
            " (ID: ", definition.mID, ") ", Logger::Green,
            " registered from ", Boundary
         );
      }

      if (not definition.mOperatorReverse.empty()) {
         Instance.RegisterVerbOperatorReverse(definition.mOperatorReverse);
         const auto op = IsolateOperator(definition.mOperatorReverse);
         Logger::VerboseRaw(
            "Operator ", Logger::DarkGreen, op,
            " (ID: ", definition.mID, ") ", Logger::Green,
            " registered from ", Boundary
         );
      }

      Instance.RegisterVerbToken(definition.mNameOf);
      if (definition.mNameOfReverse.empty()) {
         Logger::VerboseRaw(
            "Verb ", Logger::DarkGreen, definition.mNameOf,
            " (ID: ", definition.mID, ") ", Logger::Green,
            " registered from ", Boundary
         );
      }
      else {
         Instance.RegisterVerbTokenReverse(definition.mNameOfReverse);
         Logger::VerboseRaw(
            "Verb ", Logger::DarkGreen, definition.mNameOf, "/", definition.mNameOfReverse,
            " (ID: ", definition.mID, ") ", Logger::Green,
            " registered from ", Boundary
         );
      }
   #else
      if (definition.mOperator.size()) {
         const auto op1 = IsolateOperator(definition.mOperator);
         Logger::VerboseRaw(
            "Operator ", Logger::DarkGreen, op1, Logger::Green,
            " registered from ", Boundary
         );
      }

      if (not definition.mOperatorReverse.empty()) {
         const auto op2 = IsolateOperator(definition.mOperatorReverse);
         Logger::VerboseRaw(
            "Operator ", Logger::DarkGreen, op2, Logger::Green,
            " registered from ", Boundary
         );
      }

      if (definition.mNameOfReverse.empty()) {
         Logger::VerboseRaw(
            "Verb ", Logger::DarkGreen, definition.mNameOf, Logger::Green,
            " registered from ", Boundary
         );
      }
      else {
         Logger::VerboseRaw(
            "Verb ", Logger::DarkGreen, definition.mNameOf, "/", definition.mNameOfReverse,
            Logger::Green, " registered from ", Boundary
         );
      }
   #endif*/
      
      return &definition;
   }

} // namespace Langulus::RTTI
