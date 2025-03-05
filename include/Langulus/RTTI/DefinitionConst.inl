#pragma once
#include "DefinitionConst.hpp"
#include "MetaConst.hpp"
#include "../CT/Info.hpp"
#include "../CT/Version.hpp"
#include "../Logger.hpp"
#include <optional>


namespace Langulus::RTTI
{

   /// Reflect or return an already reflected constant                        
   ///   @attention when making a shared library and reflecting your enums    
   ///      at library initialization, it is recommended you mark all other   
   ///      relevant instantiations of this function as extern template, to   
   ///      save on a lot of compiler resources:                              
   ///      https://stackoverflow.com/questions/8130602                       
   ///   @tparam E - the constant to reflect                                  
   template<auto E> LANGULUS(NOINLINE)
   CMeta DefinitionConst::Reflect() {
      constexpr auto cppname = CppNameOf<E>();

      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
         // Try to get an already existing definition - the const might 
         // have been reflected previously in another shared library.   
         // We can't keep a static pointer to the meta, because shared  
         // libraries might get unloaded, resulting in different memory 
         // spaces when reloaded. An individual definition is kept for  
         // each shared library boundary, because definitions will      
         // contain pointers to functions that reside in the library    
         // memory itself, and it is a bad idea to mix those with the   
         // main library itself.                                        
         CMeta meta = Registry.GetMetaConst(cppname, RTTI::Boundary);
         if (meta)
            return meta;

         auto& definition = Registry.RegisterConst(cppname, RTTI::Boundary);
      #else
         // There's no centralized registry when MANAGED_REFLECTION is  
         // disabled, so all we can do is keep a definition on the stack
         // for each translation unit, and rely on runtime checks to    
         // make sure that definitions match between those.             
         static constinit std::optional<DefinitionConst> s_definition;
         if (s_definition.has_value())
            return CMeta {&s_definition.value()};

         auto& definition = s_definition.emplace(cppname);
      #endif


      //                                                                
      // If this is reached, then constant is not defined yet           
      // Save the original C++ name                                     
      constexpr auto token = NameOf<E>();
      static_assert(token != "", "Invalid constant token is not allowed - "
         "you have reflected your constant with an empty CTTI::NamedValue");
      definition.mToken = token;
      definition.mTokenSanitized = token;
      definition.mTokenSanitized[0] = ::std::toupper(definition.mTokenSanitized[0]);

      if constexpr (CT::InfoValue<E>) {
         // Reflected info                                              
         definition.mInfo = CTTI::InfoValue<E>::Text;
      }

      if constexpr (CT::VersionValue<E>) {
         // Reflected version                                           
         definition.mVersionMajor = CTTI::VersionValue<E>::Major;
         definition.mVersionMinor = CTTI::VersionValue<E>::Minor;
      }

      // Refer to a local copy of the data                              
      static const auto staticInstance = E;
      definition.mType = DefinitionData::Reflect<decltype(E)>();
      definition.mData = &staticInstance;

   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      // Save the boundary at time of reflection                        
      definition.mLibraryName = RTTI::Boundary;

      // After all properties have been set - generate a unique handle  
      definition.mHandle = Registry.GenerateHandle(&definition);

      Logger::Verbose(
         "Constant ", Logger::Yellow, definition.mToken,
         " (ID: ", definition.mHandle, ") ", Logger::Green,
         " registered (LIB: ", definition.mLibraryName, ")"
      );
   #else
      Logger::Verbose(
         "Constant ", Logger::Yellow, definition.mToken, Logger::Green,
         " registered (LIB: ", definition.mLibraryName, ")"
      );
   #endif

      return CMeta {&definition};
   }

} // namespace Langulus::RTTI