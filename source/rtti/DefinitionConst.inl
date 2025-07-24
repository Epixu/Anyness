///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "DefinitionConst.hpp"
#include <Langulus/CT/Info.hpp>
#include <Langulus/CT/Versioned.hpp>
#include <Langulus/Logger.hpp>
#include <Langulus/Assume.hpp>

#if not LANGULUS_FEATURE(MANAGED_REFLECTION)
   #include <optional>
#endif


namespace Langulus::RTTI
{
   /// Reflect or return an already reflected constant                        
   ///   @attention when making a shared library and reflecting your enums    
   ///      at library initialization, it is recommended you mark all other   
   ///      relevant instantiations of this function as extern template, to   
   ///      save on a lot of compiler resources:                              
   ///      https://stackoverflow.com/questions/8130602                       
   ///   @tparam E - the constant to reflect                                  
   template<auto E>
   auto DefinitionConst::Reflect() -> DefinitionConst const* {
      constexpr auto cppname = CppNameOf<E>();
      constexpr auto token = NameOf<E>();
      static_assert(token != "", "Invalid constant token is not allowed - "
         "you have reflected your constant with an empty CTTI::NamedValue");

      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
         // Try to get an already existing definition - the const might 
         // have been reflected previously in another shared library    
         DefinitionConst const* meta = Instance.GetMetaConstByCppName(cppname);
         if (meta and meta->IsInRelevantBoundary())
            return meta;

         DefinitionConst& definition = meta
            ? const_cast<DefinitionConst&>(*meta)
            : Instance.RegisterConst(cppname, token);
      #else
         // There's no centralized registry when MANAGED_REFLECTION is  
         // disabled, so all we can do is keep a definition on the stack
         // for each translation unit, and rely on hashing and runtime  
         // checks to make sure that definitions are the same           
         static constinit std::optional<DefinitionConst> s_definition;
         if (s_definition.has_value())
            return &s_definition.value();

         DefinitionConst& definition = s_definition.emplace(cppname);
         definition.mNameOf = token;
         definition.mNameOf[0] = ::std::toupper(token[0]);
      #endif


      //                                                                
      // If this is reached, then constant is not defined yet           
      // Reflected version                                              
      definition.mVersionMajor = VersionOf<E>().Major;
      definition.mVersionMinor = VersionOf<E>().Minor;

      // Save the boundary at time of reflection, but don't even        
      // bother if it is the main one                                   
      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
         if (Boundary) {
            Assert(Token{Boundary} != "MAIN", HERE(),
               "Boundary named `MAIN` is reserved - pick another name");
            definition.mBoundaries.insert(Boundary);
         }
      #endif

      // Reflected info                                                 
      constexpr auto info = InfoOf<E>();
      if constexpr (info != "")
         definition.mInfoOf = info;
      
      // Refer to a heap copy of the data                               
      using T = decltype(E);
      definition.mType = DefinitionData::Reflect<T>();
      if (not definition.mData) {
         definition.mData = new T {E};
         Assert(definition.mData, HERE(), "Insufficient memory on reflection");
         definition.mDestroyConstant = [](const void* p) {
            auto pt = static_cast<const T*>(p);
            delete pt;
         };
      }

      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
         Logger::VerboseRaw(
            Logger::Yellow, "Constant ", definition.mNameOf,
            " (ID: ", definition.mID, ") ", Logger::Green,
            "registered from ", (Boundary?Boundary:"MAIN")
         );
      #else
         Logger::VerboseRaw(
            Logger::Yellow, "Constant ", definition.mNameOf,
            Logger::Green, " reflected"
         );
      #endif
      
      return &definition;
   }
}
