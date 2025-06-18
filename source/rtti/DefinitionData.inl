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
         auto meta = Instance.GetMetaData(cppname, RTTI::Boundary);
         if (meta)
            return meta;

         auto& definition = Instance.RegisterData(cppname, RTTI::Boundary);
      #else
         // There's no centralized registry when MANAGED_REFLECTION is  
         // disabled, so all we can do is keep a definition on the stack
         // for each translation unit, and rely on runtime checks to    
         // make sure that definitions match between those.             
         static constinit std::optional<DefinitionData> s_definition;
         if (s_definition.has_value())
            return &s_definition.value();

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
      definition.mDeep = CT::Deep<T>;

      if constexpr (CT::Sparse<T> and CT::Complete<Deptr<T>>) {
         // Reflect the denser type and propagate its origin            
         definition.mDeptr = Reflect<Deptr<T>>();
      }

      using DT = Decvq<T>;

      if constexpr (not ::std::same_as<T, DT>)
         definition.mDecvq = Reflect<DT>();
      else
         definition.mDecvq = &definition;

      //                                                                
      // Constructor reflections                                        
      // @note these are allowed even if T is constant                  
      if constexpr (CT::Defaultable<DT>) {
         // Generate a default constructor                              
         definition.mDefaultConstructor =
            [](void* at) noexcept(noexcept(DT {})) {
               auto atT = static_cast<DT*>(at);
               new (atT) DT {};
            };
      }

      if constexpr (CT::CopyConstructible<DT>) {
         // Generate a copy-constructor                                 
         definition.mCopyConstructor =
            [](const void* from, void* to) {
               auto fromT = static_cast<const DT*>(from);
               auto toT = static_cast<DT*>(to);
               IntentNew(toT, Copy(*fromT));
            };
      }
            
      if constexpr (CT::ReferConstructible<DT>) {
         // Generate a refer-constructor                                
         definition.mReferConstructor =
            [](const void* from, void* to) {
               auto fromT = static_cast<const DT*>(from);
               auto toT = static_cast<DT*>(to);
               IntentNew(toT, Refer(*fromT));
            };
      }
            
      if constexpr (CT::CloneConstructible<DT>) {
         // Generate a clone-constructor                                
         definition.mCloneConstructor =
            [](const void* from, void* to) {
               auto fromT = static_cast<const DT*>(from);
               auto toT = static_cast<DT*>(to);
               IntentNew(toT, Clone(*fromT));
            };
      }

      if constexpr (CT::DisownConstructible<DT>) {
         // Generate a disown-constructor                               
         definition.mDisownConstructor =
            [](const void* from, void* to) {
               auto fromT = static_cast<const DT*>(from);
               auto toT = static_cast<DT*>(to);
               IntentNew(toT, Disown(*fromT));
            };
      }

      if constexpr (CT::MoveConstructible<DT>) {
         // Generate a move-constructor                                 
         definition.mMoveConstructor =
            [](void* from, void* to) {
               auto fromT = static_cast<DT*>(from);
               auto toT = static_cast<DT*>(to);
               IntentNew(toT, Move(*fromT));
            };
      }

      if constexpr (CT::AbandonConstructible<DT>) {
         // Generate a abandon-constructor                              
         definition.mAbandonConstructor =
            [](void* from, void* to) {
               auto fromT = static_cast<DT*>(from);
               auto toT = static_cast<DT*>(to);
               IntentNew(toT, Abandon(*fromT));
            };
      }
      
      if constexpr (CT::Destroyable<DT>) {
         // Generate a destructor                                       
         definition.mDestructor =
            [](void* at) {
               auto atT = static_cast<DT*>(at);
               atT->~DT();
            };
      }
      

      //                                                                
      // Assignment reflections                                         
      // @note allowed only if T is mutable                             
      if constexpr (CT::CopyAssignable<T>) {
         // Generate a copy-assigner                                    
         definition.mCopyAssigner =
            [](const void* from, void* to) {
               auto fromT = static_cast<const T*>(from);
               auto toT = static_cast<T*>(to);
               IntentAssign(*toT, Copy(*fromT));
            };
      }
      
      if constexpr (CT::ReferAssignable<T>) {
         // Generate a refer-assigner                                   
         definition.mReferAssigner =
            [](const void* from, void* to) {
               auto fromT = static_cast<const T*>(from);
               auto toT = static_cast<T*>(to);
               IntentAssign(*toT, Refer(*fromT));
            };
      }

      if constexpr (CT::DisownAssignable<T>) {
         // Generate a disown-assigner                                  
         definition.mDisownAssigner =
            [](const void* from, void* to) {
               auto fromT = static_cast<const T*>(from);
               auto toT = static_cast<T*>(to);
               IntentAssign(*toT, Disown(*fromT));
            };
      }
            
      if constexpr (CT::CloneAssignable<T>) {
         // Generate a clone-assigner                                   
         definition.mCloneAssigner =
            [](const void* from, void* to) {
               auto fromT = static_cast<const T*>(from);
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
      if constexpr (CT::Hashable<DT>) {
         // Generate a hashing function                                 
         definition.mHasGetHashMethod = CT::HasGetHashMethod<DT>;
         definition.mHasher = [](const void* at) {
            auto self = static_cast<const DT*>(at);
            return HashOf<true>(*self);
         };
      }

      if constexpr (CT::Referenced<T>) {
         // Generate a referencing function                             
         definition.mReferencer =
            [](void* at, int modifier) -> int {
               auto atT = static_cast<DT*>(at);
               return atT->Reference(modifier);
            };
      }

      if constexpr (CT::Comparable<DT, DT>) {
         // Generate a three-way comparison function                    
         definition.mComparer =
            [](const void* t1, const void* t2) -> Compared {
               auto t1T = static_cast<const DT*>(t1);
               auto t2T = static_cast<const DT*>(t2);

               if constexpr (CT::Sparse<DT>) {
                  // Pointers are either the same or not - not ordered  
                  // for security reasons                               
                  return *t1T == *t2T ? Compared::Equal : Compared::Unordered;
               }
               else if constexpr (CT::Fundamental<DT>) {
                  // Fundamental types are always strong-ordered        
                  if      (*t1T == *t2T)  return Compared::Equal;
                  else if (*t1T <  *t2T)  return Compared::Less;
                  else                    return Compared::Greater;
               }
               else if constexpr (CT::ComparableStrong<DT>) {
                  switch (*t1T <=> *t2T) {
                  case ::std::strong_ordering::less:        return Compared::Less;
                  case ::std::strong_ordering::equal:
                  case ::std::strong_ordering::equivalent:  return Compared::Equal;
                  case ::std::strong_ordering::greater:     return Compared::Greater;
                  }
               }
               else if constexpr (CT::ComparableWeak<DT>) {
                  switch (*t1T <=> *t2T) {
                  case ::std::weak_ordering::less:          return Compared::Less;
                  case ::std::weak_ordering::equivalent:    return Compared::Equivalent;
                  case ::std::weak_ordering::greater:       return Compared::Greater;
                  }
               }
               else if constexpr (CT::ComparablePartial<DT>) {
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

      if constexpr (CT::Resolvable<DT>) {
         // Generate a resolving function                               
         definition.mResolver =
            [](const void* at) {
               auto atT = static_cast<const DT*>(at);
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
            if (RTTI::Boundary != RTTI::MainBoundary)
               definition.mPoolTactic = PoolTactic::Type;
         #endif
      #endif

      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
         // Save the boundary at time of reflection                     
         definition.mBoundary = RTTI::Boundary;
      
         Logger::VerboseRaw<VERBOSE>(
            "Data ", Logger::Cyan, definition.mToken,
            " (ID: ", definition.mHandle, ") ", Logger::Green,
            " registered (LIB: ", definition.mBoundary, ")"
         );
      #else
         Logger::VerboseRaw<VERBOSE>(
            Logger::Green, "Data ", Logger::Cyan, definition.mToken, Logger::Green, " reflected"
         );
      #endif

      return &definition;
   }

} // namespace Langulus::RTTI