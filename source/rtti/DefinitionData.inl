///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "DefinitionData.hpp"
#include <Langulus/CT/ReflectAs.hpp>
#include <Langulus/CT/DefineTag.hpp>
#include <Langulus/CT/DefineVerb.hpp>
#include <Langulus/CT/Defaultable.hpp>
#include <Langulus/CT/Destroyable.hpp>
#include <Langulus/CT/Deep.hpp>
#include <Langulus/CT/Referenced.hpp>
#include <Langulus/CT/Resolvable.hpp>
#include <Langulus/IntentOf.hpp>
#include <Langulus/Logger.hpp>
#include <optional>

#if LANGULUS_FEATURE(MANAGED_REFLECTION)
   #include "Registry.hpp"
#endif


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
   auto DefinitionData::Reflect() -> DefinitionData const* {
      constexpr bool VERBOSE = false;

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
      static_assert(not ::std::is_function_v<T>,
         "Can't reflect this function signature - "
         "make sure you're using a pointer to it instead");

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
         DefinitionData const* meta = Instance.GetMetaDataByCppName(cppname);
         if (meta and meta->IsInRelevantBoundary())
            return meta;

         DefinitionData& definition = meta
            ? const_cast<DefinitionData&>(*meta)
            : Instance.RegisterData(cppname, Boundary);
      #else
         // There's no centralized registry when MANAGED_REFLECTION is  
         // disabled, so all we can do is keep a definition on the stack
         // for each translation unit, and rely on runtime checks to    
         // make sure that definitions match between those.             
         static constinit std::optional<DefinitionData> s_definition;
         if (s_definition.has_value())
            return &s_definition.value();

         auto& definition = s_definition.emplace(cppname, "");
      #endif
      
      //                                                                
      // If this is reached, then data is not defined yet               
      definition.template ReflectCommon<T>();
      
      constexpr auto token = NameOf<T>();
      static_assert(token != "", "Invalid data token is not allowed - "
         "you have equipped your type (or its base) with an empty CTTI_Named");

      // Data types canonically begin with a capital letter             
      definition.mNameOf = token;
      definition.mNameOf[0] = ::std::toupper(definition.mNameOf[0]);
      definition.mNameOfLowercased = Inner::ToLowercase(token);

      definition.mSize  = sizeof(T);
      definition.mAlign = alignof(T);
      definition.mConst = CT::Constant<T>;
      definition.mDeep  = CT::Deep<T>;

      if constexpr (CT::Sparse<T> and CT::Complete<Deptr<T>>) {
         // Reflect the denser type and propagate its origin            
         definition.mDeptr = Reflect<Deptr<T>>();
         const_cast<DefinitionData*>(definition.mDeptr)->mAddPtr = &definition;
      }

      // Reflect the dequalified type and generate/propagate IDs        
      using DTAll = DecvqAll<T>;
      if constexpr (not ::std::same_as<T, DTAll>) {
         definition.mDecvqAll = Reflect<DTAll>();

         // Always propagate the dequalified ID                         
         definition.mID = definition.mDecvqAll->mID;
      }
      else {
         definition.mDecvqAll = &definition;

         // No qualifiers, but propagate ID only if there's exactly     
         // one level of indirection, because that will be encoded in   
         // the structured meta data pointer. Otherwise we need a new   
         // ID to be reserved.                                          
         if (definition.mDeptr and not definition.mDeptr->mDeptr)
            definition.mID = definition.mDeptr->mID;
         else
            definition.mID = Instance.ReserveDataID(&definition);
      }

      //                                                                
      // Constructor reflections                                        
      // @note these are allowed even if T is constant                  
      if constexpr (CT::Defaultable<DTAll>) {
         // Generate a default constructor                              
         definition.mDefaultConstructor =
            [](void* at) noexcept(noexcept(DTAll {})) {
               auto atT = static_cast<DTAll*>(at);
               new (atT) DTAll {};
            };
      }

      if constexpr (CT::CopyConstructible<DTAll>) {
         // Generate a copy-constructor                                 
         definition.mCopyConstructor =
            [](const void* from, void* to) {
               auto fromT = static_cast<const DTAll*>(from);
               auto toT = static_cast<DTAll*>(to);
               IntentNew(toT, Copy(*fromT));
            };
      }
            
      if constexpr (CT::ReferConstructible<DTAll>) {
         // Generate a refer-constructor                                
         definition.mReferConstructor =
            [](const void* from, void* to) {
               auto fromT = static_cast<const DTAll*>(from);
               auto toT = static_cast<DTAll*>(to);
               IntentNew(toT, Refer(*fromT));
            };
      }
            
      if constexpr (CT::CloneConstructible<DTAll>) {
         // Generate a clone-constructor                                
         definition.mCloneConstructor =
            [](const void* from, void* to) {
               auto fromT = static_cast<const DTAll*>(from);
               auto toT = static_cast<DTAll*>(to);
               IntentNew(toT, Clone(*fromT));
            };
      }

      if constexpr (CT::DisownConstructible<DTAll>) {
         // Generate a disown-constructor                               
         definition.mDisownConstructor =
            [](const void* from, void* to) {
               auto fromT = static_cast<const DTAll*>(from);
               auto toT = static_cast<DTAll*>(to);
               IntentNew(toT, Disown(*fromT));
            };
      }

      if constexpr (CT::MoveConstructible<DTAll>) {
         // Generate a move-constructor                                 
         definition.mMoveConstructor =
            [](void* from, void* to) {
               auto fromT = static_cast<DTAll*>(from);
               auto toT = static_cast<DTAll*>(to);
               IntentNew(toT, Move(*fromT));
            };
      }

      if constexpr (CT::AbandonConstructible<DTAll>) {
         // Generate a abandon-constructor                              
         definition.mAbandonConstructor =
            [](void* from, void* to) {
               auto fromT = static_cast<DTAll*>(from);
               auto toT = static_cast<DTAll*>(to);
               IntentNew(toT, Abandon(*fromT));
            };
      }
      
      if constexpr (CT::Destroyable<DTAll>) {
         // Generate a destructor                                       
         definition.mDestructor =
            [](void* at) {
               auto atT = static_cast<DTAll*>(at);
               atT->~DTAll();
            };
      }
      

      //                                                                
      // Assignment reflections                                         
      // @note allowed only if T is mutable                             
      if constexpr (CT::CopyAssignable<T>) {
         // Generate a copy-assigner                                    
         definition.mCopyAssigner =
            [](void* from, void* to) {
               auto fromT = static_cast<T*>(from);
               auto toT = static_cast<T*>(to);
               IntentAssign(*toT, Copy(*fromT));
            };
      }
      
      if constexpr (CT::ReferAssignable<T>) {
         // Generate a refer-assigner                                   
         definition.mReferAssigner =
            [](void* from, void* to) {
               auto fromT = static_cast<T*>(from);
               auto toT = static_cast<T*>(to);
               IntentAssign(*toT, Refer(*fromT));
            };
      }

      if constexpr (CT::DisownAssignable<T>) {
         // Generate a disown-assigner                                  
         definition.mDisownAssigner =
            [](void* from, void* to) {
               auto fromT = static_cast<T*>(from);
               auto toT = static_cast<T*>(to);
               IntentAssign(*toT, Disown(*fromT));
            };
      }
            
      if constexpr (CT::CloneAssignable<T>) {
         // Generate a clone-assigner                                   
         definition.mCloneAssigner =
            [](void* from, void* to) {
               auto fromT = static_cast<T*>(from);
               auto toT = static_cast<T*>(to);
               IntentAssign(*toT, Clone(*fromT));
            };
      }

      if constexpr (CT::MoveAssignable<T>) {
         // Generate a move-assigner                                    
         definition.mMoveAssigner =
            [](void* from, void* to) {
               auto fromT = static_cast<T*>(from);
               auto toT = static_cast<T*>(to);
               IntentAssign(*toT, Move(*fromT));
            };
      }

      if constexpr (CT::AbandonAssignable<T>) {
         // Generate an abandon-assigner                                
         definition.mAbandonAssigner =
            [](void* from, void* to) {
               auto fromT = static_cast<T*>(from);
               auto toT = static_cast<T*>(to);
               IntentAssign(*toT, Abandon(*fromT));
            };
      }


      //                                                                
      // Other utilities                                                
      if constexpr (CT::Hashable<T>) {
         // Generate a hashing function                                 
         definition.mHasGetHashMethod = CT::HasGetHashMethod<T>;
         definition.mHasher = [](void* at) {
            auto self = static_cast<T*>(at);
            return HashOf<true>(*self);
         };
      }

      if constexpr (CT::Referenced<T>) {
         // Generate a referencing function                             
         definition.mReferencer =
            [](void* at, int modifier) -> int {
               auto atT = static_cast<T*>(at);
               return atT->Reference(modifier);
            };
      }

      if constexpr (CT::Comparable<T, T>) {
         // Generate a three-way comparison function                    
         definition.mComparer =
            [](void* t1, void* t2) -> Compared {
               auto t1T = static_cast<T*>(t1);
               auto t2T = static_cast<T*>(t2);

               if constexpr (CT::Sparse<DTAll>) {
                  // Pointers are either the same or not - not ordered  
                  // for security reasons                               
                  return *t1T == *t2T ? Compared::Equal : Compared::Unordered;
               }
               else if constexpr (CT::Fundamental<DTAll>) {
                  // Fundamental types are always strong-ordered        
                  if (*t1T == *t2T)  return Compared::Equal;
                  if (*t1T <  *t2T)  return Compared::Less;
                  return Compared::Greater;
               }
               else if constexpr (CT::ComparableStrong<DTAll>) {
                  switch (*t1T <=> *t2T) {
                  case ::std::strong_ordering::less:        return Compared::Less;
                  case ::std::strong_ordering::equal:
                  case ::std::strong_ordering::equivalent:  return Compared::Equal;
                  case ::std::strong_ordering::greater:     return Compared::Greater;
                  }
               }
               else if constexpr (CT::ComparableWeak<DTAll>) {
                  switch (*t1T <=> *t2T) {
                  case ::std::weak_ordering::less:          return Compared::Less;
                  case ::std::weak_ordering::equivalent:    return Compared::Equivalent;
                  case ::std::weak_ordering::greater:       return Compared::Greater;
                  }
               }
               else if constexpr (CT::ComparablePartial<DTAll>) {
                  switch (*t1T <=> *t2T) {
                  case ::std::partial_ordering::unordered:   return Compared::Unordered;
                  case ::std::partial_ordering::less:        return Compared::Less;
                  case ::std::partial_ordering::equivalent:  return Compared::Equivalent;
                  case ::std::partial_ordering::greater:     return Compared::Greater;
                  }
               }
               else static_assert(false, "Unsupported comparison");
               return Compared::Unordered;
            };
      }

      if constexpr (CT::Resolvable<T>) {
         // Generate a resolving function                               
         definition.mResolver =
            [](void* at) {
               auto atT = static_cast<T*>(at);
               return Anyness::Any {atT->GetResolved()};
            };
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
         definition.mPoolTactic = CT::GetPoolTactic<T>();

         // Make sure that types registered from an external shared     
         // library are always pooled by type, so that we're able to    
         // unregister them and free their dedicated pools when the     
         // shared library is unloaded                                  
         #if LANGULUS_FEATURE(MANAGED_REFLECTION)
            if (Boundary != MainBoundary)
               definition.mPoolTactic = PoolTactic::Type;
         #endif
      #endif
      
      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
         Logger::VerboseRaw<VERBOSE>(
            "Data ", Logger::Cyan, definition.mNameOf,
            " (ID: ", definition.mID, ") ", Logger::Green,
            " registered from ", Boundary
         );
      #else
         Logger::VerboseRaw<VERBOSE>(
            Logger::Green, "Data ", Logger::Cyan, definition.mNameOf,
            Logger::Green, " reflected"
         );
      #endif
      
      return &definition;
   }

} // namespace Langulus::RTTI
