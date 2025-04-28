#pragma once
#include "DefinitionData.hpp"
#include <Langulus/CT/ReflectAs.hpp>
#include <Langulus/CT/DefineTag.hpp>
#include <Langulus/CT/DefineVerb.hpp>
#include <Langulus/Logger.hpp>
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
      static_assert(not CT::DefineTag<T>,
         "Can't reflect tag as data");
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
      definition.mBoundary = RTTI::Boundary;

      // After all properties have been set - generate a unique handle  
      definition.mHandle = Registry.GenerateHandle(&definition);
      
      Logger::Verbose(
         "Data ", Logger::Cyan, definition.mToken,
         " (ID: ", definition.mHandle, ") ", Logger::Green,
         " registered (LIB: ", definition.mBoundary, ")"
      );
      return definition.mHandle;
   #else
      Logger::Verbose(
         "Data ", Logger::Cyan, definition.mToken, Logger::Green, " registered)"
      );
      return DMeta {&definition};
   #endif
   }

   /// Strip references or volatiles                                          
   /// Or reflect a pointer/constant                                          
   template<CT::NotDecayed T> LANGULUS(NOINLINE)
   DMeta DefinitionData::Reflect() {
      if constexpr (CT::Dense<T>) {
         if constexpr (CT::Reference<T> or CT::Volatile<T>)
            return Reflect<Devq<Deref<T>>>()
         else {

         }
      }
      else {
         static_assert(not CT::Array<T>,
            "Reflecting a bounded array is forbidden");

         constexpr auto cppname = CppNameOf<T>();
         
         #if LANGULUS_FEATURE(MANAGED_REFLECTION)
            DMeta meta = Registry.GetMetaData(cppname, RTTI::Boundary);
            if (meta)
               return meta;

            auto& definition = Registry.RegisterData(cppname, RTTI::Boundary);
         #else
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

         if constexpr (CT::Complete<Deptr<T>>) {
            // Reflect the denser type and propagate its origin properties 
            auto denser = MetaData::Of<Deptr<T>>();
            generated = *denser;
            generated.mDeptr = denser;

            #if LANGULUS_FEATURE(MANAGED_MEMORY)
               generated.mPool = nullptr;

               // Pool tactic is always default for pointers, unless these 
               // pointers have been registered outside RTTI::MainBoundary 
               #if LANGULUS_FEATURE(MANAGED_REFLECTION)
                  if (RTTI::Boundary != RTTI::MainBoundary)
                     generated.mPoolTactic = PoolTactic::Type;
               #endif
            #endif
         }
         else {
            // The denser type is incomplete, so nothing really is known   
            // about it                                                    
            generated.mInfo = "<incomplete type pointer>";
         }

         // Set library boundary - non-origin types are always associated  
         // with their origin type, if reflected in RTTI::MainBoundary     
         #if LANGULUS_FEATURE(MANAGED_REFLECTION)
            if (RTTI::Boundary == RTTI::MainBoundary and generated.mOrigin)
               generated.mLibraryName = generated.mOrigin->mLibraryName;
            else
               generated.mLibraryName = RTTI::Boundary;
         #endif

         LANGULUS_ASSERT(generated.mToken == token, Meta,
            "Token not set");
         LANGULUS_ASSERT(generated.mHash == HashOf(token), Meta,
            "Hash not set");

         // Overwrite pointer-specific stuff                               
         generated.mDecvq = MetaData::Of<DecvqAll<T>>();
         generated.mCppName = CppNameOf<T>();
         generated.mSize = sizeof(T);
         generated.mAlignment = alignof(T);
         generated.mIsSparse = true;
         generated.mIsConstant = CT::Constant<T>;
      
         // Calculate the allocation page and table                        
         // It is the same for all kinds of pointers                       
         generated.mAllocationPage = GetAllocationPageOf<void*>();
         constexpr auto minElements = GetAllocationPageOf<void*>() / sizeof(void*);
         for (Offset bit = 0; bit < sizeof(Offset) * 8; ++bit) {
            const Offset threshold = Offset {1} << bit;
            const Offset elements = threshold / sizeof(void*);
            generated.mAllocationTable[bit] = ::std::max(minElements, elements);
         }

         VERBOSE("Data ", Logger::PushCyan, generated.mToken,
            Logger::PopGreen, " registered (", generated.mLibraryName, ")");
         return &generated;
      }
   }

} // namespace Langulus::RTTI