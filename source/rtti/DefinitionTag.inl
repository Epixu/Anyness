///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "DefinitionTag.hpp"
#include <Langulus/CT/ReflectAs.hpp>
#include <Langulus/CT/DefineTag.hpp>
#include <Langulus/CT/DefineVerb.hpp>
#include <Langulus/Logger.hpp>
#include <optional>


namespace Langulus::RTTI
{

   /// Reflect or return an already reflected tag                             
   /// Definition is generated only on decayed types to avoid static variable 
   /// duplication                                                            
   ///   @attention when making a shared library and reflecting your tags     
   ///      at library initialization, it is recommended you mark all other   
   ///      relevant instantiations of this function as extern template, to   
   ///      save on a lot of compiler resources:                              
   ///      https://stackoverflow.com/questions/8130602                       
   ///   @tparam T - the decayed trait to reflect                             
   template<CT::Decayed T> LANGULUS(NOINLINE)
   auto DefinitionTag::Reflect() -> DefinitionTag const* {
      static_assert(CT::Complete<T>,
         "Can't reflect incomplete tag - "
         "make sure you have included the corresponding headers "
         "before the point of reflection. "
         "This could also be triggered due to an incomplete member in T");
      static_assert(CT::Reflectable<T>,
         "Can't reflect tag that was explicitly marked unreflectable");
      static_assert(CT::DefineTag<T>,
         "Type is not reflected as a tag definition");
      static_assert(not CT::DefineVerb<T>,
         "Can't reflect a verb as a tag");
      static_assert(not ::std::is_function_v<T>,
         "Can't reflect this function signature as a tag");

      constexpr auto cppname = CppNameOf<T>();

      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
         // Try to get an already existing definition - the trait might 
         // have been reflected previously in another shared library.   
         // We can't keep a static pointer to the meta, because shared  
         // libraries might get unloaded, resulting in different memory 
         // spaces when reloaded. An individual definition is kept for  
         // each shared library boundary, because definitions will      
         // contain pointers to functions that reside in the library    
         // memory itself, and it is a bad idea to mix those with the   
         // main library itself.                                        
         DefinitionTag const* meta = Instance.GetMetaTagByCppName(cppname);
         if (meta and meta->IsInRelevantBoundary())
            return meta;

         DefinitionTag& definition = meta
            ? const_cast<DefinitionTag&>(*meta)
            : Instance.RegisterTag(cppname, Boundary);
      #else
         // There's no centralized registry when MANAGED_REFLECTION is  
         // disabled, so all we can do is keep a definition on the stack
         // for each translation unit, and rely on runtime checks to    
         // make sure that definitions match between those.             
         static constinit std::optional<DefinitionTag> s_definition;
         if (s_definition.has_value())
            return &s_definition.value();

         auto& definition = s_definition.emplace(cppname, "");
      #endif


      //                                                                
      // If this is reached, then tag is not defined yet                
      definition.template ReflectCommon<T>();

      constexpr auto token = NameOfTag<T>();
      static_assert(token != "", "Invalid tag token is not allowed - "
         "you have equipped your type (or its base) with an empty CTTI_DefineTag");

      // Tags are canonically always lowercased                         
      definition.mNameOf = Inner::ToLowercase(token);
      definition.mNameOfLowercased = definition.mNameOf;

   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      Logger::VerboseRaw(
         "Tag ", Logger::Purple, definition.mNameOf,
         " (ID: ", definition.mID, ") ", Logger::Green,
         " registered from ", Boundary
      );
   #else
      Logger::VerboseRaw(
         "Tag ", Logger::Purple, definition.mNameOf,
         Logger::Green, " reflected"
      );
   #endif

      return &definition;
   }

} // namespace Langulus::RTTI
