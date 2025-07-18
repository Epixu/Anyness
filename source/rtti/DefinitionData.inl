///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "DefinitionData.hpp"
#include <Langulus/CT/Abstract.hpp>
#include <Langulus/CT/ReflectAs.hpp>
#include <Langulus/CT/DefineTag.hpp>
#include <Langulus/CT/DefineVerb.hpp>
#include <Langulus/CT/Defaultable.hpp>
#include <Langulus/CT/Destroyable.hpp>
#include <Langulus/CT/Deep.hpp>
#include <Langulus/CT/Referenced.hpp>
#include <Langulus/CT/Resolvable.hpp>
#include <Langulus/CT/Nullable.hpp>
#include <Langulus/CT/POD.hpp>
#include <Langulus/CT/Concrete.hpp>
#include <Langulus/CT/Producer.hpp>
#include <Langulus/CT/Convertible.hpp>
#include <Langulus/CT/DefineConst.hpp>
#include <Langulus/CT/Members.hpp>
#include <Langulus/IntentOf.hpp>
#include <Langulus/Logger.hpp>
#include "Langulus/SuffixOf.hpp"
#include "Langulus/FilesOf.hpp"

#if not LANGULUS_FEATURE(MANAGED_REFLECTION)
   #include <optional>
#else
   #include "Registry.hpp"
#endif

#include "DefinitionVerb.hpp"
#include "DefinitionConst.hpp"
#include "DefinitionTag.hpp"


namespace Langulus::RTTI
{

   /// Reflect or return an already reflected data                            
   ///   @attention when making a shared library and reflecting your types    
   ///      at library initialization, it is recommended you mark all other   
   ///      relevant instantiations of this function as extern template, to   
   ///      save on a lot of compiler resources:                              
   ///      https://stackoverflow.com/questions/8130602                       
   ///   @tparam T - the decayed type to reflect                              
   template<class T>
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
         // have been reflected previously in another shared library    
         DefinitionData const* meta = Instance.GetMetaDataByCppName(cppname);
         if (meta and meta->IsInRelevantBoundary())
            return meta;

         DefinitionData& definition = meta
            ? const_cast<DefinitionData&>(*meta)
            : Instance.RegisterData(cppname);
      #else
         // There's no centralized registry when MANAGED_REFLECTION is  
         // disabled, so all we can do is keep a definition on the stack
         // for each translation unit, and rely on runtime checks to    
         // make sure that definitions match between those.             
         static constinit std::optional<DefinitionData> s_definition;
         if (s_definition.has_value())
            return &s_definition.value();

         DefinitionData& definition = s_definition.emplace(cppname);
      #endif
      
      //                                                                
      // If this is reached, then data is not defined yet from the      
      // viewpoint of the current boundary                              
      definition.template ReflectCommon<T>();
      
      constexpr auto token = NameOf<T>();
      static_assert(token != "", "Invalid data token is not allowed - "
         "you have equipped your type (or its base) with an empty CTTI_Named");

      // Data types canonically begin with a capital letter             
      definition.mNameOf = token;
      definition.mNameOf[0] = ::std::toupper(definition.mNameOf[0]);
      definition.mNameOfLowercased = Inner::ToLowercase(token);

      definition.mSize      = sizeof(T);
      definition.mAlign     = alignof(T);
      definition.mConst     = CT::Constant<T>;
      definition.mDeep      = CT::Deep<T>;
      definition.mPOD       = CT::POD<T> and not CT::Abstract<T>;
      definition.mNullable  = CT::Nullable<T> and not CT::Abstract<T>;
      definition.mAbstract  = CT::Abstract<T>;

      constexpr auto suffix = SuffixOf<T>();
      if constexpr (suffix != "")
         definition.mSuffixOf = suffix;

      constexpr auto files = FilesOf<T>();
      if constexpr (files != "")
         definition.mFilesOf = files;

      // Reflect the origin type                                        
      if constexpr (CT::Decayed<T>)
         definition.mOrigin = &definition;
      else if constexpr (CT::Complete<Decay<T>>)
         definition.mOrigin = Reflect<Decay<T>>();

      // Reflect the dequalified types and generate/propagate IDs       
      using DTOnce = Decvq<T>;
      using DTAll  = DecvqAll<T>;

      if constexpr (not ::std::same_as<T, DTAll>) {
         // T has qualifiers                                            
         definition.mDecvqOnce = Reflect<DTOnce>();
         definition.mDecvqAll  = Reflect<DTAll>();
         
         if constexpr (CT::Constant<T>) {
            auto decvq = const_cast<DefinitionData*>(definition.mDecvqOnce);
            decvq->mAddConst = &definition;
         }
      }
      else {
         // T has no qualifiers                                         
         definition.mDecvqOnce = &definition;
         definition.mDecvqAll  = &definition;
      }

      if constexpr (CT::Sparse<T>) {
         using DenserT = Deptr<T>;

         if constexpr (CT::Complete<DenserT>) {
            // Reflect the denser type                                  
            definition.mDeptr = Reflect<DenserT>();
            auto deptr = const_cast<DefinitionData*>(definition.mDeptr);
            deptr->mAddPtr = definition.mDecvqOnce;

            #if LANGULUS_FEATURE(MANAGED_REFLECTION)
               // Propagate ID only if there's exactly one level of     
               // indirection, because that will be encoded in the      
               // packed meta data pointer - otherwise we need a new ID 
               if constexpr (CT::Dense<DenserT>)
                  definition.mID = deptr->mID;
            #endif
         }
         else {
            // An incomplete sparse type always has mDeptr of 1         
            definition.mDeptr = reinterpret_cast<DefinitionData*>(intptr_t {1});
         }

         #if LANGULUS_FEATURE(MANAGED_REFLECTION)
            if constexpr (CT::Sparse<DenserT> or not CT::Complete<DenserT>) {
               // Multiple indirections always result in a unique ID    
               // Incomplete types are always considered an indirection 
               auto decvq = const_cast<DefinitionData*>(definition.mDecvqOnce);
               decvq->mID = Instance.ReserveDataID(decvq);
               decvq->mPtrIncludedInID = true;

               if constexpr (CT::Convoluted<T>) {
                  definition.mID = decvq->mID;
                  definition.mPtrIncludedInID = true;
               }
            }
         #endif
      }
      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      else if constexpr (CT::Convoluted<T>) {
         // Const/volatile dense encountered, propagate ID              
         definition.mID = definition.mDecvqOnce->mID;
      }
      else {
         // Origin type encountered, time to reserve a new ID           
         definition.mID = Instance.ReserveDataID(&definition);
      }
      #endif

      // Reflect the concrete type                                      
      if constexpr (CT::Concretizable<T>) {
         static_assert(CT::Abstract<T>,
            "Only abstract types can have concretizations");
         static_assert(not CT::Abstract<ConcreteOf<T>>,
            "Concrete type can't be abstract");
         definition.mCurrentBoundary.mConcrete = Reflect<ConcreteOf<T>>;
      }

      // Reflect the producer type                                      
      if constexpr (CT::Producible<T>) {
         static_assert(not CT::Abstract<ProducerOf<T>>,
            "Producer type can't be abstract");
         definition.mCurrentBoundary.mProducer = Reflect<ProducerOf<T>>;
      }

      //                                                                
      // Constructor reflections                                        
      // @note these are allowed even if T is constant                  
      if constexpr (CT::Defaultable<DTAll>) {
         // Generate a default constructor                              
         definition.mCurrentBoundary.mDefaultConstructor =
            [](void* at) noexcept(noexcept(DTAll {})) {
               auto atT = static_cast<DTAll*>(at);
               new (atT) DTAll {};
            };
      }

      if constexpr (CT::CopyConstructible<DTAll>) {
         // Generate a copy-constructor                                 
         definition.mCurrentBoundary.mCopyConstructor =
            [](void* from, void* to) {
               auto fromT = static_cast<const DTAll*>(from);
               auto toT = static_cast<DTAll*>(to);
               IntentNew(toT, Copy(*fromT));
            };
      }
            
      if constexpr (CT::ReferConstructible<DTAll>) {
         // Generate a refer-constructor                                
         definition.mCurrentBoundary.mReferConstructor =
            [](void* from, void* to) {
               auto fromT = static_cast<const DTAll*>(from);
               auto toT = static_cast<DTAll*>(to);
               IntentNew(toT, Refer(*fromT));
            };
      }
            
      if constexpr (CT::CloneConstructible<DTAll>) {
         // Generate a clone-constructor                                
         definition.mCurrentBoundary.mCloneConstructor =
            [](void* from, void* to) {
               auto fromT = static_cast<const DTAll*>(from);
               auto toT = static_cast<DTAll*>(to);
               IntentNew(toT, Clone(*fromT));
            };
      }

      if constexpr (CT::DisownConstructible<DTAll>) {
         // Generate a disown-constructor                               
         definition.mCurrentBoundary.mDisownConstructor =
            [](void* from, void* to) {
               auto fromT = static_cast<const DTAll*>(from);
               auto toT = static_cast<DTAll*>(to);
               IntentNew(toT, Disown(*fromT));
            };
      }

      if constexpr (CT::MoveConstructible<DTAll>) {
         // Generate a move-constructor                                 
         definition.mCurrentBoundary.mMoveConstructor =
            [](void* from, void* to) {
               auto fromT = static_cast<DTAll*>(from);
               auto toT = static_cast<DTAll*>(to);
               IntentNew(toT, Move(*fromT));
            };
      }

      if constexpr (CT::AbandonConstructible<DTAll>) {
         // Generate a abandon-constructor                              
         definition.mCurrentBoundary.mAbandonConstructor =
            [](void* from, void* to) {
               auto fromT = static_cast<DTAll*>(from);
               auto toT = static_cast<DTAll*>(to);
               IntentNew(toT, Abandon(*fromT));
            };
      }
      
      if constexpr (CT::Destroyable<DTAll>) {
         // Generate a destructor                                       
         definition.mCurrentBoundary.mDestructor =
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
         definition.mCurrentBoundary.mCopyAssigner =
            [](void* from, void* to) {
               auto fromT = static_cast<T*>(from);
               auto toT = static_cast<T*>(to);
               IntentAssign(*toT, Copy(*fromT));
            };
      }
      
      if constexpr (CT::ReferAssignable<T>) {
         // Generate a refer-assigner                                   
         definition.mCurrentBoundary.mReferAssigner =
            [](void* from, void* to) {
               auto fromT = static_cast<T*>(from);
               auto toT = static_cast<T*>(to);
               IntentAssign(*toT, Refer(*fromT));
            };
      }

      if constexpr (CT::DisownAssignable<T>) {
         // Generate a disown-assigner                                  
         definition.mCurrentBoundary.mDisownAssigner =
            [](void* from, void* to) {
               auto fromT = static_cast<T*>(from);
               auto toT = static_cast<T*>(to);
               IntentAssign(*toT, Disown(*fromT));
            };
      }
            
      if constexpr (CT::CloneAssignable<T>) {
         // Generate a clone-assigner                                   
         definition.mCurrentBoundary.mCloneAssigner =
            [](void* from, void* to) {
               auto fromT = static_cast<T*>(from);
               auto toT = static_cast<T*>(to);
               IntentAssign(*toT, Clone(*fromT));
            };
      }

      if constexpr (CT::MoveAssignable<T>) {
         // Generate a move-assigner                                    
         definition.mCurrentBoundary.mMoveAssigner =
            [](void* from, void* to) {
               auto fromT = static_cast<T*>(from);
               auto toT = static_cast<T*>(to);
               IntentAssign(*toT, Move(*fromT));
            };
      }

      if constexpr (CT::AbandonAssignable<T>) {
         // Generate an abandon-assigner                                
         definition.mCurrentBoundary.mAbandonAssigner =
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
         definition.mCurrentBoundary.mHasher = [](void* at) {
            auto self = static_cast<T*>(at);
            return HashOf<true>(*self);
         };
      }

      if constexpr (CT::Referenced<T>) {
         // Generate a referencing function                             
         definition.mCurrentBoundary.mReferencer =
            [](void* at, int modifier) -> int {
               auto atT = static_cast<T*>(at);
               return atT->Reference(modifier);
            };
      }

      if constexpr (CT::Comparable<T, T>) {
         // Generate a three-way comparison function                    
         definition.mCurrentBoundary.mComparer =
            [](void* t1, void* t2) -> Compared {
               auto t1T = static_cast<const T*>(t1);
               auto t2T = static_cast<const T*>(t2);

               if constexpr (CT::Sparse<DTAll>) {
                  // Pointers are either the same or not - not ordered  
                  // for security reasons                               
                  return *t1T == *t2T ? Compared::Equal : Compared::Unordered;
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
               else {
                  if (*t1T == *t2T)  return Compared::Equal;
                  if (*t1T <  *t2T)  return Compared::Less;
                  return Compared::Greater;
               }
            };
      }

      if constexpr (CT::Resolvable<T>) {
         // Generate a resolving function                               
         definition.mCurrentBoundary.mResolver =
            [](void* at) {
               auto atT = static_cast<T*>(at);
               return Anyness::Any {atT->GetResolved()};
            };
      }
      
      #if LANGULUS_FEATURE(MANAGED_MEMORY)
         // Calculate the allocation page and table using reflection    
         definition.mPoolTactic = CT::GetPoolTactic<T>();
         definition.mMinimalPoolSize = CT::GetMinPool<T>();
         constexpr auto minElements = CT::GetMinPool<T>() / sizeof(T);
         for (size_t bit = 0; bit < sizeof(size_t) * 8u; ++bit) {
            const size_t threshold = size_t {1} << bit;
            const size_t elements = threshold / sizeof(T);
            definition.mAllocationTable[bit] = ::std::max(minElements, elements);
         }

         // Make sure that types registered from an external shared     
         // library are always pooled by type, so that we're able to    
         // unregister them and free their dedicated pools when the     
         // shared library is unloaded                                  
         #if LANGULUS_FEATURE(MANAGED_REFLECTION)
            if (Boundary != MainBoundary)
               definition.mPoolTactic = PoolTactic::Type;
         #endif
      #else
         // Calculate the allocation page and table using configuration 
         definition.mMinimalPoolSize = sizeof(T) * 256 <= LANGULUS_MIN_POOL
            ? LANGULUS_MIN_POOL
            : sizeof(T) * 256;
         const auto minElements = definition.mMinimalPoolSize / sizeof(T);
         for (size_t bit = 0; bit < sizeof(size_t) * 8u; ++bit) {
            const size_t threshold = size_t {1} << bit;
            const size_t elements = threshold / sizeof(T);
            definition.mAllocationTable[bit] = ::std::max(minElements, elements);
         }
      #endif
      
      using BASES = BasesOf<T>;
      if constexpr (not CT::Void<BASES>) {
         // Set reflected bases                                         
         BASES::ForEach([&definition]<class B>{
            definition.mCurrentBoundary.mBases.push_back(
               Base::From<T, B>()
            );
         });
      }

      using VERBS = VerbsOf<T>;
      if constexpr (not CT::Void<VERBS>) {
         // Set reflected abilities                                     
         VERBS::ForEach([&definition]<class V>{
            static_assert(CT::DefineVerb<V>,
               "Verb list must contain only verbs");
            static_assert(CT::Decayed<V>,
               "Verbs must be fully decayed when listed");
            static_assert(V::template IsAble<T>,
               "T doesn't have the required verb method/specialization");

            definition.mCurrentBoundary.mVerbs.emplace(
               DefinitionVerb::Reflect<V>(),
               [](void* self, Flow::Verb& verb) -> bool {
                  auto in = static_cast<T*>(self);
                  return V::template In<T>::Execute(*in, verb);
               }
            );
         });
      }

      using MAPTO = MorphismsTo<T>;
      if constexpr (not CT::Void<MAPTO>) {
         // Set reflected morphisms                                     
         // @attention morphisms assume that source is initialized, but 
         //    destination is only allocated and not yet constructed    
         MAPTO::ForEach([&definition]<class TO>{
            definition.mCurrentBoundary.mMorphismsTo.emplace(
               DefinitionData::Reflect<TO>(),
               [](void* from, void* to) {
                  auto fromT = static_cast<T*>(from);
                  auto toT   = static_cast<TO*>(to);

                  if constexpr (requires { TO (*fromT); })
                     new (toT) TO (*fromT);
                  else if constexpr (requires { TO (static_cast<TO>(*fromT)); })
                     new (toT) TO (static_cast<TO>(*fromT));
                  else {
                     static_assert(false,
                        "T can't be converted to TO - add "
                        "explicit/implicit constructors and/or cast operators"
                     );
                  }
               }
            );
         });
      }

      using MAPFROM = MorphismsFrom<T>;
      if constexpr (not CT::Void<MAPFROM>) {
         // Set reflected morphisms                                     
         // @attention morphisms assume that source is initialized, but 
         //    destination is only allocated and not yet constructed    
         MAPFROM::ForEach([&definition]<class FROM>{
            definition.mCurrentBoundary.mMorphismsFrom.emplace(
               DefinitionData::Reflect<FROM>(),
               [](void* from, void* to) {
                  auto fromT = static_cast<FROM*>(from);
                  auto toT   = static_cast<T*>(to);

                  if constexpr (requires { T (*fromT); })
                     new (toT) T (*fromT);
                  else if constexpr (requires { T (static_cast<T>(*fromT)); })
                     new (toT) T (static_cast<T>(*fromT));
                  else {
                     static_assert(false,
                        "FROM can't be converted to T - add "
                        "explicit/implicit constructors and/or cast operators"
                     );
                  }
               }
            );
         });
      }

      using CONSTANTS = NamedValuesOf<T>;
      if constexpr (not CT::Void<CONSTANTS>) {
         // Reflecting named values                                     
         CONSTANTS::ForEach([&definition]<auto C>{
            definition.mNamedValues.push_back(
               DefinitionConst::Reflect<C>()
            );
         });
      }

      using MEMBERS = MembersOf<T>;
      if constexpr (not CT::Void<MEMBERS>) {
         // Reflecting members                                          
         MEMBERS::ForEach([&definition]<class M>{
            definition.mCurrentBoundary.mMembers.push_back(
               DefinitionData::Member::From<M>()
            );
         });
      }

      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
         Logger::Verbose<VERBOSE>(
            "Data ", Logger::Cyan, definition.mNameOf,
            " (ID: ", definition.mID, ") ", Logger::Green,
            " registered from ", Boundary
         );
      #else
         Logger::Verbose<VERBOSE>(
            Logger::Green, "Data ", Logger::Cyan, definition.mNameOf,
            Logger::Green, " reflected"
         );
      #endif
      
      return &definition;
   }
   
   /// Generate a member definition                                           
   ///   @return the generated member descriptor                              
   template<class HANDLE>
   auto DefinitionData::Member::From() -> Member {
      using THIS = typename HANDLE::Owner;
      using DATA = typename HANDLE::Type;

      Member m;
      m.extent = ExtentOf<DATA>;
      m.member = [](void* owner) -> void* {
         auto context = reinterpret_cast<THIS*>(owner);
         return &(context->*HANDLE::Handle);
      };

      using TAGS = TagsOf<DATA>;
      if constexpr (not CT::Void<TAGS>) {
         // Reflect the trait tag                                       
         m.type = Reflect<DATA>;
         m.getTag = [](unsigned index) -> DefinitionTag const* {
            DefinitionTag const* found = nullptr;
            TAGS::ForEach([&index, &found]<class T>{
               static_assert(CT::DefineTag<T>, "T is not a tag definition");
               if (index == 0)
                  found = DefinitionTag::Reflect<T>();
               --index;
            });
            return found;
         };
      }
      else {
         m.type = Reflect<Deext<DATA>>;
         m.getTag = nullptr;
      }
      return m;
   }
   
   /// Create a base descriptor for the derived type T                        
   ///   @return the generated base descriptor                                
   template<CT::Dense T, CT::Dense BASE>
   auto DefinitionData::Base::From() has_assumptions -> Base {
      static_assert(not CT::Void<BASE>,
         "Can't have void as base");
      static_assert(not CT::Same<T, BASE>,
         "Can't have base of the same type as the derived");
      static_assert(NameOf<T>() != NameOf<BASE>(),
         "T and BASE have the same LANGULUS(NAME) token, possibly due to "
         "inheritance. Specify a different LANGULUS(NAME) for each!");

      Base result;
      result.type = Reflect<BASE>();

      if constexpr (CT::DerivedFrom<T, BASE>) {
         // This will fail if base is private                           
         // This is detectable by is_convertible_v                      
         if constexpr (::std::is_convertible_v<T*, BASE*>) {
            if constexpr (CT::VirtuallyDerivedFrom<T, BASE>) {
               // Can't use pointer arithmetics when base is virtual    
               result.virtualBase = [](void* from) -> void* {
                  return dynamic_cast<BASE*>(reinterpret_cast<T*>(from));
               };
            }
            else {
               // The devil's work, right here                          
               // @attention works only with conventional inheritance   
               alignas(T) static const uint8_t storage[sizeof(T)] {};
               // First reinterpret the storage as T                    
               const auto derived = reinterpret_cast<const T*>(storage);
               // Then cast it down to base                             
               const auto base = static_cast<const BASE*>(derived);
               // Then reinterpret back to byte array and get difference
               const auto offset =
                  reinterpret_cast<const uint8_t*>(base) -
                  reinterpret_cast<const uint8_t*>(derived);

               Assert(offset >= 0, HERE(),
                  "BASE is laid (memorywise) before T");
               result.offset = static_cast<size_t>(offset);
            }
         }
         else static_assert(false, "Can't reflect private base");
      }
      else {
         // If not inherited in C++, then always imposed                
         // Imposed bases are excluded from serialization               
         result.imposed = true;

         if constexpr (not CT::Abstract<BASE> and sizeof(BASE) < sizeof(T)) {
            // The imposed type has a chance of being binary            
            // compatible when having a specific count                  
            result.binaryCompatible = 0 == sizeof(T) % sizeof(BASE);
            result.count = sizeof(T) / sizeof(BASE);
         }
      }

      // If sizes match and there's no byte offset, then the base       
      // and the derived type are binary compatible                     
      if constexpr (sizeof(BASE) == sizeof(T)
      and not CT::VirtuallyDerivedFrom<T, BASE>)
         result.binaryCompatible = (0 == result.offset);
      return result;
   }

} // namespace Langulus::RTTI
