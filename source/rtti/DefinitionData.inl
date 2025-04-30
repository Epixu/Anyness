#pragma once
#include "DefinitionData.hpp"
#include <Langulus/CT/ReflectAs.hpp>
#include <Langulus/CT/DefineTag.hpp>
#include <Langulus/CT/DefineVerb.hpp>
#include <Langulus/CT/Pooled.hpp>
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
   template<class T> LANGULUS(NOINLINE)
   DMeta DefinitionData::Reflect() {
      static_assert(not CT::Function<T>,
         "Can't reflect this function signature - "
         "make sure you're using a pointer to it instead");
      static_assert(CT::Complete<T>,
         "Can't reflect incomplete type - "
         "make sure you have included the corresponding headers "
         "before the point of reflection. "
         "This could also be triggered due to an incomplete member in T");
      static_assert(not CT::Array<T>,
         "Reflecting a bounded array is forbidden to avoid bloat");
      static_assert(not CT::Volatile<T>,
         "Can't reflect volatile type, use Devq before reflection");
      static_assert(not CT::Reference<T>,
         "Can't reflect reference type, use Deref before reflection");
      static_assert(not CT::DefineTag<T>,
         "Can't reflect tag as data");
      static_assert(not CT::DefineVerb<T>,
         "Can't reflect constant as data");
      static_assert(CT::Reflectable<T>,
         "Can't reflect data that was explicitly marked unreflectable");
      static_assert(CT::Exact<CT::ReflectedAs<T>, T>,
         "Data is marked to be reflected as something else, "
         "make sure this is respected before reaching this function");

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

      constexpr auto token = NameOf<T>();
      static_assert(token != "", "Invalid data token is not allowed - "
         "you have equipped your type (or its base) with an empty CTTI_Named");

      definition.template ReflectCommon<T>();
      definition.mToken = token;
      definition.mTokenSanitized = Inner::FindLastToken(token);
      definition.mTokenSanitized[0] = ::std::toupper(definition.mTokenSanitized[0]);
      definition.mSize = sizeof(T);
      definition.mAlign = alignof(T);
      definition.mConst = CT::Constant<T>;

      if constexpr (CT::Sparse<T> and CT::Complete<Deptr<T>>) {
         // Reflect the denser type and propagate its origin            
         definition.mDeptr = Reflect<Deptr<T>>();
      }

      // Calculate the allocation page and table                        
      // It is the same, regardless if T is const or not                
      definition.mAllocationPage = CT::GetMinPool<T>();
      constexpr auto minElements = CT::GetMinPool<T>() / sizeof(T);
      for (size_t bit = 0; bit < sizeof(size_t) * 8u; ++bit) {
         const size_t threshold = size_t {1} << bit;
         const size_t elements = threshold / sizeof(T);
         definition.mAllocationTable[bit] = ::std::max(minElements, elements);
      }
      
      #if LANGULUS_FEATURE(MANAGED_MEMORY)
         definition.mPool = nullptr;

         // Pool tactic is always default for pointers, unless          
         // these pointers have been registered outside                 
         // RTTI::MainBoundary                                          
         #if LANGULUS_FEATURE(MANAGED_REFLECTION)
            if (RTTI::Boundary != RTTI::MainBoundary)
               definition.mPoolTactic = PoolTactic::Type;
         #endif
      #endif

      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
         // Save the boundary at time of reflection                     
         definition.mBoundary = RTTI::Boundary;

         // After all properties have been set - generate a unique id   
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

} // namespace Langulus::RTTI