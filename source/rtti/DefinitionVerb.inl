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
   /// Definition is generated only on decayed types to avoid static variable 
   /// duplication                                                            
   ///   @attention when making a shared library and reflecting your verbs    
   ///      at library initialization, it is recommended you mark all other   
   ///      relevant instantiations of this function as extern template, to   
   ///      save on a lot of compiler resources:                              
   ///      https://stackoverflow.com/questions/8130602                       
   ///   @tparam T - the decayed verb to reflect                              
   template<CT::Decayed T> LANGULUS(NOINLINE)
   VMeta DefinitionVerb::Reflect() {
      static_assert(not CT::Function<T>,
         "Can't reflect this function signature as a verb");
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

      constexpr auto cppname = CppNameOf<T>();

      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
         // Try to get an already existing definition - the verb might  
         // have been reflected previously in another shared library.   
         // We can't keep a static pointer to the meta, because shared  
         // libraries might get unloaded, resulting in different memory 
         // spaces when reloaded. An individual definition is kept for  
         // each shared library boundary, because definitions will      
         // contain pointers to functions that reside in the library    
         // memory itself, and it is a bad idea to mix those with the   
         // main library itself.                                        
         VMeta meta = Registry.GetMetaVerb(cppname, RTTI::Boundary);
         if (meta)
            return meta;

         auto& definition = Registry.RegisterVerb(cppname, RTTI::Boundary);

      #else
         // There's no centralized registry when MANAGED_REFLECTION is  
         // disabled, so all we can do is keep a definition on the stack
         // for each translation unit, and rely on runtime checks to    
         // make sure that definitions match between those.             
         static constinit std::optional<DefinitionVerb> s_definition;
         if (s_definition.has_value())
            return VMeta {&s_definition.value()};

         auto& definition = s_definition.emplace(cppname);
      #endif

      //                                                                
      // If this is reached, then trait is not defined yet              
      constexpr auto verbPos = NameOfVerb<T>();
      constexpr auto verbNeg = NameOfVerbReverse<T>();
      static_assert(not verbPos.empty(),
         "Invalid positive verb token is not allowed");
      static_assert(verbPos != verbNeg,
         "Verb can't have the same positive and negative tokens");
      definition.mToken = verbPos;
      definition.mTokenReverse = verbNeg;

      constexpr auto opPos = OperatorOfVerb<T>();
      constexpr auto opNeg = OperatorOfVerbReverse<T>();
      static_assert(opPos != opNeg or opPos.empty(),
         "Verb can't have the same positive and negative operators");
      definition.mOperator = opPos;
      definition.mOperatorReverse = opNeg;

      definition.template ReflectCommon<T>();

      if constexpr (CTTI::DefineVerb<T>::Enabled)
         definition.mPrecedence = CTTI::DefineVerb<T>::Precedence;
      else
         definition.mPrecedence = T::CTTI_DefineVerb::Precedence;

      definition.mExecuteDefaultMutable  = VerbDefaultMutable<T>();
      definition.mExecuteDefaultConstant = VerbDefaultConstant<T>();
      definition.mExecuteStateless       = VerbStateless<T>();

   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      // Save the boundary at time of reflection                        
      definition.mLibraryName = RTTI::Boundary;

      // After all properties have been set - generate a unique handle  
      definition.mHandle = Registry.GenerateHandle(&definition);

      if (definition.mOperator.size()) {
         Registry.RegisterVerbOperator(definition.mOperator, RTTI::Boundary);
         const auto op1 = IsolateOperator(definition.mOperator);
         Logger::Verbose(
            "Operator ", Logger::DarkGreen, op1,
            " (ID: ", definition.mHandle, ") ", Logger::Green,
            " registered (LIB: ", definition.mLibraryName, ")"
         );
      }

      if (not definition.mOperatorReverse.empty()) {
         Registry.RegisterVerbOperatorReverse(definition.mOperatorReverse, RTTI::Boundary);
         const auto op2 = IsolateOperator(definition.mOperatorReverse);
         Logger::Verbose(
            "Operator ", Logger::DarkGreen, op2,
            " (ID: ", definition.mHandle, ") ", Logger::Green,
            " registered (LIB: ", definition.mLibraryName, ")"
         );
      }

      Registry.RegisterVerbToken(definition.mToken, RTTI::Boundary);
      if (definition.mTokenReverse.empty()) {
         Logger::Verbose(
            "Verb ", Logger::DarkGreen, definition.mToken,
            " (ID: ", definition.mHandle, ") ", Logger::Green,
            " registered (LIB: ", definition.mLibraryName, ")"
         );
      }
      else {
         Registry.RegisterVerbTokenReverse(definition.mTokenReverse, RTTI::Boundary);
         Logger::Verbose(
            "Verb ", Logger::DarkGreen, definition.mToken, "/", definition.mTokenReverse,
            " (ID: ", definition.mHandle, ") ", Logger::Green,
            " registered (LIB: ", definition.mLibraryName, ")"
         );
      }
      return definition.mHandle;
   #else
      if (definition.mOperator.size()) {
         const auto op1 = IsolateOperator(definition.mOperator);
         Logger::Verbose(
            "Operator ", Logger::DarkGreen, op1, Logger::Green,
            " registered (LIB: ", definition.mLibraryName, ")"
         );
      }

      if (not definition.mOperatorReverse.empty()) {
         const auto op2 = IsolateOperator(definition.mOperatorReverse);
         Logger::Verbose(
            "Operator ", Logger::DarkGreen, op2, Logger::Green,
            " registered (LIB: ", definition.mLibraryName, ")"
         );
      }

      if (definition.mTokenReverse.empty()) {
         Logger::Verbose(
            "Verb ", Logger::DarkGreen, definition.mToken, Logger::Green,
            " registered (LIB: ", definition.mLibraryName, ")"
         );
      }
      else {
         Logger::Verbose(
            "Verb ", Logger::DarkGreen, definition.mToken, "/", definition.mTokenReverse,
            Logger::Green, " registered (LIB: ", definition.mLibraryName, ")"
         );
      }
      return VMeta {&definition};
   #endif
   }

} // namespace Langulus::RTTI