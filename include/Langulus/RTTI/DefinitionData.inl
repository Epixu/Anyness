#pragma once
#include "DefinitionData.hpp"
#include "MetaData.hpp"
#include "../CT/ReflectAs.hpp"
#include "../CT/DefineTrait.hpp"
#include "../CT/DefineVerb.hpp"
#include "../Logger.hpp"
#include <optional>


namespace Langulus::RTTI
{

   /// Reflect or return an already reflected data                            
   /// Definition is generated only on decayed types to avoid static variable 
   /// duplication                                                            
   ///   @attention when making a shared library and reflecting your types    
   ///      at library initialization, it is recommended you mark all other   
   ///      relevant instantiations of this function as extern template, to   
   ///      save on a lot of compiler resources:                              
   ///      https://stackoverflow.com/questions/8130602                       
   ///   @tparam T - the decayed type to reflect                              
   template<CT::Decayed T> LANGULUS(NOINLINE)
   DMeta DefinitionData::Reflect() {
      static_assert(not CT::Function<T>,
         "Can't reflect this function signature - "
         "make sure you're using a pointer to it instead");
      static_assert(CT::Complete<T>,
         "Can't reflect incomplete type - "
         "make sure you have included the corresponding headers "
         "before the point of reflection. "
         "This could also be triggered due to an incomplete member in T");
      static_assert(CT::Reflectable<T>,
         "Can't reflect data that was explicitly marked unreflectable");
      static_assert(not CT::DefineTrait<T>,
         "Can't reflect trait as data");
      static_assert(not CT::DefineVerb<T>,
         "Can't reflect constant as data");

      constexpr auto cppname = CppNameOf<T>();

      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
         // Try to get an already existing definition - the data might  
         // have been reflected previously in another shared library.   
         // We can't keep a static pointer to the meta, because shared  
         // libraries might get unloaded, resulting in different memory 
         // spaces when reloaded. An individual definition is kept for  
         // each shared library boundary, because definitions will      
         // contain pointers to functions that reside in the library    
         // memory itself, and it is a bad idea to mix those with the   
         // main library itself.                                        
         DMeta meta = Registry.GetMetaData(cppname, RTTI::Boundary);
         if (meta)
            return meta;

         auto& definition = Registry.RegisterData(cppname, RTTI::Boundary);
      #else
         // There's no centralized registry when MANAGED_REFLECTION is  
         // disabled, so all we can do is keep a definition on the stack
         // for each translation unit, and rely on runtime checks to    
         // make sure that definitions match between those.             
         static constinit std::optional<DefinitionData> s_definition;
         if (s_definition.has_value())
            return DMeta {&s_definition.value()};

         auto& definition = s_definition.emplace(cppname);
      #endif


      //                                                                
      // If this is reached, then data is not defined yet               
      constexpr auto token = NameOf<T>();
      static_assert(token != "", "Invalid data token is not allowed - "
         "you have equipped your type (or its base) with an empty CTTI_Named");
      definition.mToken = token;
      definition.mTokenSanitized = Inner::FindLastToken(token);
      definition.mTokenSanitized[0] = ::std::toupper(definition.mTokenSanitized[0]);

      definition.template ReflectCommon<T>();

   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      // Save the boundary at time of reflection                        
      definition.mLibraryName = RTTI::Boundary;

      // After all properties have been set - generate a unique handle  
      definition.mHandle = Registry.GenerateHandle(&definition);
      
      Logger::Verbose(
         "Data ", Logger::Cyan, definition.mToken,
         " (ID: ", definition.mHandle, ") ", Logger::Green,
         " registered (LIB: ", definition.mLibraryName, ")"
      );
   #else
      Logger::Verbose(
         "Data ", Logger::Cyan, definition.mToken, Logger::Green,
         " registered (LIB: ", definition.mLibraryName, ")"
      );
   #endif

      return DMeta {&definition};
   }

} // namespace Langulus::RTTI