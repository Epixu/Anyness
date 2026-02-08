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
#include <Langulus/CT/MinAlloc.hpp>
#include <Langulus/CT/Files.hpp>
#include <Langulus/CT/Suffix.hpp>
#include <Langulus/CT/Serializer.hpp>
#include <Langulus/CT/Executable.hpp>
#include <Langulus/IntentOf.hpp>

#if not LANGULUS_FEATURE(MANAGED_REFLECTION)
   #include <optional>
#else
   #include "Registry.hpp"
#endif

#include "DefinitionVerb.hpp"
#include "DefinitionConst.hpp"
#include "DefinitionTag.hpp"

#if 0 or LANGULUS_META_VERBOSITY_MASTER_SWITCH()
   #include <Langulus/Logger/EnableVerbose.hpp>
#else
   #include <Langulus/Logger/NoVerbose.hpp>
#endif


namespace Langulus::RTTI
{
   namespace Inner
   {
      /// These functions are used to reduce the number of generated unique   
      /// lambdas at reflection time                                          
      template<CT::Sparse T> LANGULUS(NOINLINE)
      void SparseDefaultDeref(void* from, void* to) {
         static_assert(CT::NotConvolutedAnywhere<T>,
            "Strip qualifiers to avoid unnecessary instantiations");
         if constexpr (::std::is_same_v<T, void*>) {
            auto typed_from = static_cast<void**>(from);
            auto typed_to   = static_cast<void**>(to);
            *typed_to = *typed_from;
         }
         else {
            static_assert(CT::CustomPointer<T>,
               "T should be a custom pointer, use void* if not");
            using DenserT = Deptr<T>;
            auto typed_from = static_cast<T*>(from);
            auto typed_to   = static_cast<DenserT*>(to);
            *typed_to = **typed_from;
         }
      };

      template<CT::Sparse T> LANGULUS(NOINLINE)
      void SparseDefaultConstructor(void* at) noexcept {
         static_assert(CT::NotConvolutedAnywhere<T>,
            "Strip qualifiers to avoid unnecessary instantiations");
         auto atT = static_cast<T*>(at);
         new (atT) T{};
      };

      template<CT::Sparse T> LANGULUS(NOINLINE)
      void SparseCopyConstructor(void* from, void* to) noexcept {
         static_assert(CT::NotConvolutedAnywhere<T>,
            "Strip qualifiers to avoid unnecessary instantiations");
         auto fromT = static_cast<T*>(from);
         auto toT = static_cast<T*>(to);
         *toT = *fromT;
      };
      
      template<CT::Sparse T> LANGULUS(NOINLINE)
      auto SparseCompare(const void* lhs, const void* rhs) noexcept -> Compared {
         static_assert(CT::NotConvolutedAnywhere<T>,
            "Strip qualifiers to avoid unnecessary instantiations");
         
         // Pointers are either the same or not - not ordered for       
         // security reasons                                            
         if constexpr (::std::is_same_v<T, void*>) {
            auto lhsT = static_cast<void const* const*>(lhs);
            auto rhsT = static_cast<void const* const*>(rhs);
            return *lhsT == *rhsT ? Compared::Equal : Compared::Unordered;
         }
         else {
            auto lhsT = static_cast<T const*>(lhs);
            auto rhsT = static_cast<T const*>(rhs);
            return *lhsT == *rhsT ? Compared::Equal : Compared::Unordered;
         }
      };

      template<CT::Sparse T> LANGULUS(NOINLINE)
      bool SparseCompareEqual(const void* lhs, const void* rhs) noexcept {
         static_assert(CT::NotConvolutedAnywhere<T>,
            "Strip qualifiers to avoid unnecessary instantiations");
         
         if constexpr (::std::is_same_v<T, void*>) {
            auto lhsT = static_cast<void const* const*>(lhs);
            auto rhsT = static_cast<void const* const*>(rhs);
            return *lhsT == *rhsT;
         }
         else {
            auto lhsT = static_cast<T const*>(lhs);
            auto rhsT = static_cast<T const*>(rhs);
            return *lhsT == *rhsT;
         }
      };

      template<CT::Sparse T> LANGULUS(NOINLINE)
      auto SparseHash(void* lhs) noexcept -> Hash {
         static_assert(CT::NotConvolutedAnywhere<T>,
            "Strip qualifiers to avoid unnecessary instantiations");
         auto lhsT = static_cast<T*>(lhs);
         return HashOf<true>(*lhsT);
      };
   }

   /// Reflect or return an already reflected data                            
   ///   @attention when making a shared library and reflecting your types    
   ///      at library initialization, it is recommended you mark all other   
   ///      relevant instantiations of this function as extern template, to   
   ///      save on a lot of compiler resources:                              
   ///      https://stackoverflow.com/questions/8130602                       
   ///   @tparam T the type to reflect                                        
   template<class T> requires (CT::Dense<T> and not ::std::is_const_v<T>)
   auto DefinitionData::Reflect() -> DefinitionData const* {
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
      static_assert(CT::Decayed<T>,
         "Unsupported qualifiers detected");
      static_assert(CT::Reflectable<T>,
         "Can't reflect data that was explicitly marked unreflectable");
      static_assert(Exact<CT::ReflectedAs<T>, T>,
         "Data is marked to be reflected as something else, "
         "make sure this is respected before reaching this function");
      static_assert(not ::std::is_function_v<T>,
         "Can't reflect this function signature - "
         "make sure you're using a pointer to it instead");

      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
         // Try to get an already existing definition - the data might  
         // have been reflected previously in another shared library    
         const auto cppname = CppNameOf<T>();
         DefinitionData const* meta = Instance.GetMetaDataByCppName(cppname);
         if (meta and meta->IsInRelevantBoundary())
            return meta;

         const auto token = NameOf<T, false>();
         DefinitionData& definition = meta
            ? const_cast<DefinitionData&>(*meta)
            : Instance.RegisterData(cppname, token);
      #else
         // There's no centralized registry when MANAGED_REFLECTION is  
         // disabled, so all we can do is keep a definition on the stack
         // for each translation unit, and rely on runtime checks to    
         // make sure that definitions match between those.             
         static constinit std::optional<DefinitionData> s_definition;
         if (s_definition.has_value())
            return &s_definition.value();

         const auto cppname = CppNameOf<T>();
         DefinitionData& definition = s_definition.emplace(cppname);

         definition.mNameOf = Inner::NormalizeAtRuntime(NameOf<T, false>());
         LglsAssert(not definition.mNameOf.empty(),
            "Invalid data token is not allowed - "
            "you have equipped your type (or its base) with an empty CTTI_Named. "
            "The type in question is: ", cppname
         );
         definition.mNameOf[0] = ToUppercase(definition.mNameOf[0]);
      #endif
      
      //                                                                
      // If this is reached, then data is not defined yet from the      
      // viewpoint of the current boundary                              
      definition.ReflectCommon<T>();
      definition.mSize       = sizeof(T);
      definition.mAlign      = alignof(T);
      definition.mConst      = false;
      definition.mDeep       = CT::Deep<T>;
      definition.mPOD        = CT::POD<T> and not CT::Abstract<T>;
      definition.mNullable   = CT::Nullable<T> and not CT::Abstract<T>;
      definition.mAbstract   = CT::Abstract<T>;
      definition.mExecutable = CT::Executable<T>;
      definition.mOrigin     = &definition;
      definition.mDecvqOnce  = &definition;
      definition.mDecvqAll   = &definition;

      if constexpr (CT::Suffix<T>)
         definition.mSuffixOf = SuffixOf<T>();

      if constexpr (CT::Files<T>)
         definition.mFilesOf = FilesOf<T>();

      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
         // Origin type encountered, time to reserve a new ID           
         definition.mID = Instance.ReserveDataID(&definition);
         IF_SAFE(LglsAssumeDev(not definition.mDedicatedID,
            "ID has already been reserved"));
         IF_SAFE(definition.mDedicatedID = true);
      #endif

      // Reflect the concrete type                                      
      if constexpr (CT::Concretizable<T>) {
         using C = CT::ReflectedAs<ConcreteOf<T>>;
         static_assert(not CT::Abstract<C>,
            "Concrete type can't be abstract");
         definition.mCurrentBoundary.mConcrete = Reflect<C>;
      }

      // Reflect the producer type                                      
      if constexpr (CT::Producible<T>) {
         using P = CT::ReflectedAs<ProducerOf<T>>;
         definition.mCurrentBoundary.mProducer = Reflect<P>;
      }

      //                                                                
      // Constructor reflections                                        
      if constexpr (CT::Defaultable<T>) {
         // Generate a default constructor                              
         definition.mCurrentBoundary.mDefaultConstructor =
            [](void* at) noexcept(noexcept(T {})) {
               auto atT = static_cast<T*>(at);
               new (atT) T {};
            };
      }

      if constexpr (CT::CopyConstructible<T>) {
         // Generate a copy-constructor                                 
         definition.mCurrentBoundary.mCopyConstructor =
            [](void* from, void* to) {
               auto fromT = static_cast<const T*>(from);
               auto toT = static_cast<T*>(to);
               IntentNew(toT, Copy(*fromT));
            };
      }
         
      if constexpr (CT::ReferConstructible<T>) {
         // Generate a refer-constructor                                
         definition.mCurrentBoundary.mReferConstructor =
            [](void* from, void* to) {
               auto fromT = static_cast<const T*>(from);
               auto toT = static_cast<T*>(to);
               IntentNew(toT, Refer(*fromT));
            };
      }
         
      if constexpr (CT::CloneConstructible<T>) {
         // Generate a clone-constructor                                
         definition.mCurrentBoundary.mCloneConstructor =
            [](void* from, void* to) {
               auto fromT = static_cast<const T*>(from);
               auto toT = static_cast<T*>(to);
               IntentNew(toT, Clone(*fromT));
            };
      }

      if constexpr (CT::DisownConstructible<T>) {
         // Generate a disown-constructor                               
         definition.mCurrentBoundary.mDisownConstructor =
            [](void* from, void* to) {
               auto fromT = static_cast<const T*>(from);
               auto toT = static_cast<T*>(to);
               IntentNew(toT, Disown(*fromT));
            };
      }

      if constexpr (CT::MoveConstructible<T>) {
         // Generate a move-constructor                                 
         definition.mCurrentBoundary.mMoveConstructor =
            [](void* from, void* to) {
               auto fromT = static_cast<T*>(from);
               auto toT = static_cast<T*>(to);
               IntentNew(toT, Move(*fromT));
            };
      }

      if constexpr (CT::AbandonConstructible<T>) {
         // Generate a abandon-constructor                              
         definition.mCurrentBoundary.mAbandonConstructor =
            [](void* from, void* to) {
               auto fromT = static_cast<T*>(from);
               auto toT = static_cast<T*>(to);
               IntentNew(toT, Abandon(*fromT));
            };
      }
   
      if constexpr (CT::Destroyable<T>) {
         // Generate a destructor                                       
         definition.mCurrentBoundary.mDestructor =
            [](void* at) {
               auto atT = static_cast<T*>(at);
               atT->~T();
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
            [](void const* t1, void const* t2) -> Compared {
               auto t1T = static_cast<T const*>(t1);
               auto t2T = static_cast<T const*>(t2);
               return FromOrdering(*t1T, *t2T);
            };

         // Generate an equality comparison function                    
         definition.mCurrentBoundary.mComparerEqual =
            [](void const* t1, void const* t2) -> bool {
               auto t1T = static_cast<T const*>(t1);
               auto t2T = static_cast<T const*>(t2);
               return *t1T == *t2T;
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

      // Reflect the minimal allocation in bytes                        
      definition.mMinimalAllocation = CT::GetMinAlloc<T>();

      #if LANGULUS_FEATURE(MANAGED_MEMORY)
         // Reflect pooling properties                                  
         definition.mMinimalPoolSize = CT::GetMinPool<T>();
         definition.mPoolTactic = CT::GetPoolTactic<T>();
      
         // Make sure that types registered from an external shared     
         // library are _always_ pooled by type, so that we're able to  
         // unregister them and free their dedicated pools when the     
         // shared library is unloaded                                  
         #if LANGULUS_FEATURE(MANAGED_REFLECTION)
            if (Boundary)
               definition.mPoolTactic = PoolTactic::Type;
         #endif
      #endif
      
      // Calculate the allocation table                                 
      auto minElements = CT::GetMinAlloc<T>() / sizeof(T);
      for (size_t bit = 0; bit < sizeof(size_t) * 8u; ++bit) {
         const size_t threshold = size_t {1} << bit;
         const size_t elements = threshold / sizeof(T);
         definition.mAllocationTable[bit] = minElements > elements
            ? minElements : elements;
      }

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

            auto verb_definition = const_cast<DefinitionVerb*>(
               DefinitionVerb::Reflect<V>());
            definition.mCurrentBoundary.mVerbs.emplace(
               verb_definition,
               [](void* self, Flow::Verb& verb) -> bool {
                  auto in = static_cast<T*>(self);
                  return V::template In<T>::Execute(*in, verb);
               }
            );
            verb_definition->mAble.insert(&definition);
         });
      }
   
      using MAPTO = MorphismsFrom<T>;
      if constexpr (not CT::Void<MAPTO>) {
         // Set reflected morphisms                                     
         // @attention morphisms assume that source is initialized,     
         //    but destination is only allocated and not yet constructed
         MAPTO::ForEach([&definition]<class TO_RAW>{
            using TO = CT::ReflectedAs<TO_RAW>;

            auto destination_type = const_cast<DefinitionData*>(Reflect<TO>());
            auto converter_function = [](void* from, void* to) {
               auto fromT = static_cast<T*>(from);
               auto toT   = static_cast<TO*>(to);
               new (toT) TO {Langulus::Convert<TO>(*fromT)};
            };
            
            if constexpr (CT::Serializer<TO>) {
               // Destination type can act as a serializer, too         
               // @attention serialization assumes both sides are valid 
               //    and constructed pointers. Context is optional.     
               using S = SerializerOf<TO>;

               auto serializer_function = [](void* from, void* to, void* context) -> size_t {
                  auto fromT = static_cast<T*>(from);
                  auto toT   = static_cast<TO*>(to);
                  auto conT  = static_cast<typename S::Context*>(context);
                  return Langulus::Serialize(*fromT, *toT, conT);
               };
            
               definition.mCurrentBoundary.mMorphismsTo.emplace(
                  destination_type,
                  Morphism {converter_function, serializer_function}
               );
            }
            else {
               definition.mCurrentBoundary.mMorphismsTo.emplace(
                  destination_type,
                  Morphism {converter_function, nullptr}
               );
            }
         });
      }
   
      using MAPFROM = MorphismsTo<T>;
      if constexpr (not CT::Void<MAPFROM>) {
         // Set reflected morphisms                                     
         // @attention morphisms assume that source is initialized,     
         //    but destination is only allocated and not yet constructed
         MAPFROM::ForEach([&definition]<class FROM_RAW>{
            using FROM = CT::ReflectedAs<FROM_RAW>;

            auto source_type = const_cast<DefinitionData*>(Reflect<FROM>());
            auto converter_function = [](void* from, void* to) {
               auto fromT = static_cast<FROM*>(from);
               auto toT   = static_cast<T*>(to);
               new (toT) T {Langulus::Convert<T>(*fromT)};
            };
            
            if constexpr (CT::Serializer<T>) {
               // Destination type can act as a serializer, too         
               // @attention serialization assumes both sides are valid 
               //    and constructed pointers. Context is optional.     
               using S = SerializerOf<T>;

               auto serializer_function = [](void* from, void* to, void* context) -> size_t {
                  auto fromT = static_cast<FROM*>(from);
                  auto toT   = static_cast<T*>(to);
                  auto conT  = static_cast<typename S::Context*>(context);
                  return Langulus::Serialize(*fromT, *toT, conT);
               };
            
               definition.mCurrentBoundary.mMorphismsFrom.emplace(
                  source_type,
                  Morphism {converter_function, serializer_function}
               );
            }
            else {
               definition.mCurrentBoundary.mMorphismsFrom.emplace(
                  source_type,
                  Morphism {converter_function, nullptr}
               );
            }
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
               Member::From<M>()
            );
         });
      }
      
      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
         LglsVerbose(
            Logger::Cyan, "Data ", definition.mNameOf,
            " (ID: ", definition.mID, ") ", Logger::Green,
            "registered from ", (Boundary?Boundary:"MAIN")
         );
      #else
         LglsVerbose(
            Logger::Cyan, "Data ", definition.mNameOf,
            Logger::Green, " reflected"
         );
      #endif
      
      return &definition;
   }
   
   /// Reflect or return an already reflected data                            
   ///   @attention when making a shared library and reflecting your types    
   ///      at library initialization, it is recommended you mark all other   
   ///      relevant instantiations of this function as extern template, to   
   ///      save on a lot of compiler resources:                              
   ///      https://stackoverflow.com/questions/8130602                       
   ///   @tparam T the type to reflect                                        
   template<class T> requires (CT::Dense<T> and ::std::is_const_v<T>)
   auto DefinitionData::Reflect() -> DefinitionData const* {
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
      static_assert(Exact<CT::ReflectedAs<T>, T>,
         "Data is marked to be reflected as something else, "
         "make sure this is respected before reaching this function");
      static_assert(not ::std::is_function_v<T>,
         "Can't reflect this function signature - "
         "make sure you're using a pointer to it instead");

      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
         // Try to get an already existing definition - the data might  
         // have been reflected previously in another shared library    
         const auto cppname {CppNameOf<Decvq<T>>() + " const"};
         DefinitionData const* meta = Instance.GetMetaDataByCppName(cppname);
         if (meta and meta->IsInRelevantBoundary())
            return meta;
      
         const auto token {NameOf<Decvq<T>, false>() + " const"};
         DefinitionData& definition = meta
            ? const_cast<DefinitionData&>(*meta)
            : Instance.RegisterData(cppname, token);
      #else
         // There's no centralized registry when MANAGED_REFLECTION is  
         // disabled, so all we can do is keep a definition on the stack
         // for each translation unit, and rely on runtime checks to    
         // make sure that definitions match between those.             
         static constinit std::optional<DefinitionData> s_definition;
         if (s_definition.has_value())
            return &s_definition.value();

         const auto cppname {CppNameOf<Decvq<T>>() + " const"};
         DefinitionData& definition = s_definition.emplace(cppname);
      
         definition.mNameOf = Inner::NormalizeAtRuntime(NameOf<Decvq<T>, false>());
         definition.mNameOf += " const";
         definition.mNameOf[0] = ToUppercase(definition.mNameOf[0]);
      #endif
      
      //                                                                
      // If this is reached, then data is not defined yet from the      
      // viewpoint of the current boundary                              
      definition.ReflectCommon<T>();
      definition.mSize       = sizeof(T);
      definition.mAlign      = alignof(T);
      definition.mConst      = true;
      definition.mDeep       = CT::Deep<T>;
      definition.mPOD        = CT::POD<T> and not CT::Abstract<T>;
      definition.mNullable   = CT::Nullable<T> and not CT::Abstract<T>;
      definition.mAbstract   = CT::Abstract<T>;
      definition.mExecutable = CT::Executable<T>;

      // Reflect the origin type                                        
      definition.mOrigin    = Reflect<CT::ReflectedAs<Decay<T>>>();
      definition.mSuffixOf  = definition.mOrigin->mSuffixOf;
      definition.mFilesOf   = definition.mOrigin->mFilesOf;
      definition.mDecvqOnce = definition.mOrigin;
      definition.mDecvqAll  = definition.mOrigin;
      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
         definition.mID     = definition.mDecvqOnce->mID;
      #endif
      
      auto decvq = const_cast<DefinitionData*>(definition.mDecvqOnce);
      decvq->mAddConst = &definition;
      
      definition.mCurrentBoundary.mConcrete
         = definition.mOrigin->mCurrentBoundary.mConcrete;
      definition.mCurrentBoundary.mProducer
         = definition.mOrigin->mCurrentBoundary.mProducer;

      //                                                                
      // Constructor reflections                                        
      // Reuse the same lambdas as the dequalified reflection           
      // (template bloat reduction)                                     
      definition.mCurrentBoundary.mDefaultConstructor
         = definition.mOrigin->mCurrentBoundary.mDefaultConstructor;
      definition.mCurrentBoundary.mCopyConstructor
         = definition.mOrigin->mCurrentBoundary.mCopyConstructor;
      definition.mCurrentBoundary.mReferConstructor
         = definition.mOrigin->mCurrentBoundary.mReferConstructor;
      definition.mCurrentBoundary.mCloneConstructor
         = definition.mOrigin->mCurrentBoundary.mCloneConstructor;
      definition.mCurrentBoundary.mDisownConstructor
         = definition.mOrigin->mCurrentBoundary.mDisownConstructor;
      definition.mCurrentBoundary.mMoveConstructor
         = definition.mOrigin->mCurrentBoundary.mMoveConstructor;
      definition.mCurrentBoundary.mAbandonConstructor
         = definition.mOrigin->mCurrentBoundary.mAbandonConstructor;
      definition.mCurrentBoundary.mDestructor
         = definition.mOrigin->mCurrentBoundary.mDestructor;

      //                                                                
      // Other utilities                                                
      definition.mCurrentBoundary.mHasher
         = definition.mOrigin->mCurrentBoundary.mHasher;
      definition.mCurrentBoundary.mReferencer
         = definition.mOrigin->mCurrentBoundary.mReferencer;
      definition.mCurrentBoundary.mComparer
         = definition.mOrigin->mCurrentBoundary.mComparer;
      definition.mCurrentBoundary.mComparerEqual
         = definition.mOrigin->mCurrentBoundary.mComparerEqual;
      definition.mCurrentBoundary.mResolver
         = definition.mOrigin->mCurrentBoundary.mResolver;
      definition.mMinimalAllocation
         = definition.mOrigin->mMinimalAllocation;

      #if LANGULUS_FEATURE(MANAGED_MEMORY)
         definition.mPoolTactic
            = definition.mOrigin->mPoolTactic;
         definition.mMinimalPoolSize
            = definition.mOrigin->mMinimalPoolSize;

         // Make sure that types registered from an external shared     
         // library are always pooled by type, so that we're able to    
         // unregister them and free their dedicated pools when the     
         // shared library is unloaded                                  
         #if LANGULUS_FEATURE(MANAGED_REFLECTION)
            if (Boundary)
               definition.mPoolTactic = PoolTactic::Type;
         #endif
      #endif

      memcpy(
         definition.mAllocationTable,
         definition.mOrigin->mAllocationTable,
         sizeof(definition.mAllocationTable)
      );

      definition.mCurrentBoundary.mBases
         = definition.mOrigin->mCurrentBoundary.mBases;
      definition.mCurrentBoundary.mMorphismsTo
         = definition.mOrigin->mCurrentBoundary.mMorphismsTo;
      definition.mNamedValues
         = definition.mOrigin->mNamedValues;
      definition.mCurrentBoundary.mMembers
         = definition.mOrigin->mCurrentBoundary.mMembers;

      using VERBS = VerbsOf<T>;
      if constexpr (not CT::Void<VERBS>) {
         // Set reflected abilities                                     
         // These can be different for constant/mutable types           
         VERBS::ForEach([&definition]<class V>{
            static_assert(CT::DefineVerb<V>,
               "Verb list must contain only verbs");
            static_assert(CT::Decayed<V>,
               "Verbs must be fully decayed when listed");
            static_assert(V::template IsAble<T>,
               "T doesn't have the required verb method/specialization");

            auto verb_definition = const_cast<DefinitionVerb*>(
               DefinitionVerb::Reflect<V>());
            definition.mCurrentBoundary.mVerbs.emplace(
               verb_definition,
               [](void* self, Flow::Verb& verb) -> bool {
                  auto in = static_cast<T*>(self);
                  return V::template In<T>::Execute(*in, verb);
               }
            );
            verb_definition->mAble.insert(&definition);
         });
      }

      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
         LglsVerbose(
            Logger::Cyan, "Data ", definition.mNameOf,
            " (ID: ", definition.mID, ") ", Logger::Green,
            "registered from ", (Boundary?Boundary:"MAIN")
         );
      #else
         LglsVerbose(
            Logger::Cyan, "Data ", definition.mNameOf,
            Logger::Green, " reflected"
         );
      #endif
      
      return &definition;
   }
   
   /// Reflect or return an already reflected data                            
   ///   @attention when making a shared library and reflecting your types    
   ///      at library initialization, it is recommended you mark all other   
   ///      relevant instantiations of this function as extern template, to   
   ///      save on a lot of compiler resources:                              
   ///      https://stackoverflow.com/questions/8130602                       
   ///   @tparam T the type to reflect                                        
   template<class T> requires CT::Sparse<T>
   auto DefinitionData::Reflect() -> DefinitionData const* {
      static_assert(not CT::Array<T>,
         "Reflecting a bounded array is forbidden to avoid bloat");
      static_assert(not CT::Volatile<T>,
         "Can't reflect volatile type, use Devq before reflection");
      static_assert(not CT::Reference<T>,
         "Can't reflect reference type, use Deref before reflection");
      static_assert(CT::Reflectable<T>,
         "Can't reflect data that was explicitly marked unreflectable");
      static_assert(Exact<CT::ReflectedAs<T>, T>,
         "Data is marked to be reflected as something else, "
         "make sure this is respected before reaching this function");

      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
         // Try to get an already existing definition - the data might  
         // have been reflected previously in another shared library    
         ::std::string cppname;
         ::std::string token;
         DefinitionData const* meta;

         if constexpr (::std::is_pointer_v<T>) {
            // Recostruct pointer name and token at runtime to avoid a lot of compilation time
            // @attention we do this for conventional pointers only
            cppname = CppNameOf<Decvq<Deptr<T>>>();
            if constexpr (CT::Constant<Deptr<T>>) cppname += " const";
            if constexpr (CT::Constant<T>) cppname += "* const";
            else cppname += "*";
            meta = Instance.GetMetaDataByCppName(cppname);
            if (meta and meta->IsInRelevantBoundary())
               return meta;

            token = NameOf<Decvq<Deptr<T>>, false>();
            if constexpr (CT::Constant<Deptr<T>>) token += " const";
            if constexpr (CT::Constant<T>) token += "* const";
            else token += "*";
         }
         else {
            // Custom pointers
            cppname = CppNameOf<Decvq<T>>();
            meta = Instance.GetMetaDataByCppName(cppname);
            if (meta and meta->IsInRelevantBoundary())
               return meta;

            token = NameOf<Decvq<T>, false>();
         }

         DefinitionData& definition = meta
            ? const_cast<DefinitionData&>(*meta)
            : Instance.RegisterData(cppname, token);
      #else
         // There's no centralized registry when MANAGED_REFLECTION is  
         // disabled, so all we can do is keep a definition on the stack
         // for each translation unit, and rely on runtime checks to    
         // make sure that definitions match between those.             
         static constinit std::optional<DefinitionData> s_definition;
         if (s_definition.has_value())
            return &s_definition.value();

         ::std::string cppname;
         if constexpr (::std::is_pointer_v<T>) {
            cppname = CppNameOf<Decvq<Deptr<T>>>();
            if constexpr (CT::Constant<Deptr<T>>) cppname += " const";
            if constexpr (CT::Constant<T>) cppname += "* const";
            else cppname += "*";
         }
         else cppname = CppNameOf<Decvq<T>>();

         DefinitionData& definition = s_definition.emplace(cppname);
         if constexpr (::std::is_pointer_v<T>) {
            definition.mNameOf = Inner::NormalizeAtRuntime(NameOf<Decvq<Deptr<T>>, false>());
            if constexpr (CT::Constant<Deptr<T>>)
               definition.mNameOf += " const";
            if constexpr (CT::Constant<T>)
               definition.mNameOf += "* const";
            else
               definition.mNameOf += "*";
            definition.mNameOf[0] = ToUppercase(definition.mNameOf[0]);
         }
         else {
            definition.mNameOf = Inner::NormalizeAtRuntime(NameOf<Decvq<T>, false>());
            definition.mNameOf[0] = ToUppercase(definition.mNameOf[0]);
         }
      #endif
      
      //                                                                
      // If this is reached, then data is not defined yet from the      
      // viewpoint of the current boundary/library                      
      definition.ReflectCommon<T>();
      definition.mSize      = sizeof(T);
      definition.mAlign     = alignof(T);
      definition.mConst     = CT::Constant<T>;
      definition.mPOD       = true;
      definition.mNullable  = true;
      definition.mAbstract  = false;

      if constexpr (CT::Complete<Decay<T>>) {
         definition.mDeep = CT::Deep<T>;
         definition.mExecutable = CT::Executable<T>;
      }
      else {
         definition.mDeep = false;
         definition.mExecutable = false;
      }

      // Reflect the origin type                                        
      if constexpr (CT::Complete<Decay<T>>)
         definition.mOrigin = Reflect<CT::ReflectedAs<Decay<T>>>();

      // Reflect the dequalified types and generate/propagate IDs       
      using DTOnce = Decvq<T>;
      using DTAll  = DecvqAll<T>;

      if constexpr (CT::CustomPointer<T>)
         definition.mPointerSpecification = DTAll::Specification;

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

      using LambdaT = Tif<CT::CustomPointer<T>, DecvqAll<T>, void*>;
      using DenserT = Deref<Deptr<T>>;
      if constexpr (CT::Complete<DenserT>) {
         // Reflect the denser type                                     
         definition.mDeptr = Reflect<CT::ReflectedAs<DenserT>>();
         auto deptr = const_cast<DefinitionData*>(definition.mDeptr);

         if constexpr (not CT::CustomPointer<T>)
            deptr->mAddPtr = definition.mDecvqOnce;
         else if (not deptr->mAddPtr)
            deptr->mAddPtr = definition.mDecvqOnce;

         #if LANGULUS_FEATURE(MANAGED_REFLECTION)
            // Propagate ID only if there's exactly one level of        
            // unqualifided indirection, because that will be encoded   
            // in the packed meta data pointer perfectly - otherwise    
            // we need a new ID                                         
            if constexpr (CT::Dense<DenserT> and not CT::Constant<DenserT> and not CT::CustomPointer<T>)
               definition.mID = deptr->mID;
         #endif

         definition.mCurrentBoundary.mDereference = Inner::SparseDefaultDeref<LambdaT>;
      }
      else {
         // An incomplete sparse type always has mDeptr of 1            
         definition.mDeptr = reinterpret_cast<DefinitionData*>(intptr_t {1});
      }

      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
         if constexpr (CT::CustomPointer<T> or not CT::Complete<DenserT> or ::std::is_const_v<DenserT>
         or LglsSif(CT::Complete<DenserT>, return CT::Sparse<DenserT>, return false)) {
            // Custom pointers always result in a unique ID             
            // Multiple indirections always result in a unique ID       
            // Incomplete types are always considered an indirection    
            // A constant denser type (at any level of indirection)     
            // also requires a unique ID                                
            auto decvq = const_cast<DefinitionData*>(definition.mDecvqOnce);
            decvq->mID = Instance.ReserveDataID(decvq);
            decvq->mPtrIncludedInID = true;
            IF_SAFE(LglsAssumeDev(not definition.mDedicatedID,
               "ID has already been reserved"));
            IF_SAFE(definition.mDedicatedID = true);

            if constexpr (CT::Constant<T>) {
               definition.mID = decvq->mID;
               definition.mPtrIncludedInID = true;
            }
         }
      #endif

      //                                                                
      // Constructor reflections                                        
      // @note these are allowed even if T is constant                  
      definition.mCurrentBoundary.mDefaultConstructor
         = Inner::SparseDefaultConstructor<LambdaT>;
      definition.mCurrentBoundary.mCopyConstructor
         = Inner::SparseCopyConstructor<LambdaT>;
      definition.mCurrentBoundary.mReferConstructor
         = Inner::SparseCopyConstructor<LambdaT>;
      definition.mCurrentBoundary.mDisownConstructor
         = Inner::SparseCopyConstructor<LambdaT>;
      definition.mCurrentBoundary.mMoveConstructor
         = Inner::SparseCopyConstructor<LambdaT>;
      definition.mCurrentBoundary.mAbandonConstructor
         = Inner::SparseCopyConstructor<LambdaT>;

      if constexpr (CT::Complete<Decay<T>>) {
         // Always use the origin cloning routine                       
         definition.mCurrentBoundary.mCloneConstructor
            = definition.mOrigin->mCurrentBoundary.mCloneConstructor;
      }
      
      //                                                                
      // Assignment reflections                                         
      // @note allowed only if T is mutable                             
      if constexpr (CT::Mutable<T>) {
         definition.mCurrentBoundary.mCopyAssigner
            = Inner::SparseCopyConstructor<LambdaT>;
         definition.mCurrentBoundary.mReferAssigner
            = Inner::SparseCopyConstructor<LambdaT>;
         definition.mCurrentBoundary.mDisownAssigner
            = Inner::SparseCopyConstructor<LambdaT>;
         definition.mCurrentBoundary.mMoveAssigner
            = Inner::SparseCopyConstructor<LambdaT>;
         definition.mCurrentBoundary.mAbandonAssigner
            = Inner::SparseCopyConstructor<LambdaT>;
      
         if constexpr (CT::Complete<Decay<T>>) {
            // Always use the origin cloning routine                    
            definition.mCurrentBoundary.mCloneAssigner
               = definition.mOrigin->mCurrentBoundary.mCloneAssigner;
         }
      }

      //                                                                
      // Other utilities                                                
      definition.mCurrentBoundary.mHasher
         = Inner::SparseHash<LambdaT>;   
      definition.mCurrentBoundary.mComparer
         = Inner::SparseCompare<LambdaT>;         
      definition.mCurrentBoundary.mComparerEqual
         = Inner::SparseCompareEqual<LambdaT>;         

      // Reflect the minimal allocation in bytes                        
      definition.mMinimalAllocation = CT::GetMinAlloc<T>();

      #if LANGULUS_FEATURE(MANAGED_MEMORY)
         // Reflect pooling properties                                  
         definition.mPoolTactic = CT::GetPoolTactic<T>();
         definition.mMinimalPoolSize = CT::GetMinPool<T>();

         // Make sure that types registered from an external shared     
         // library are always pooled by type, so that we're able to    
         // unregister them and free their dedicated pools when the     
         // shared library is unloaded                                  
         #if LANGULUS_FEATURE(MANAGED_REFLECTION)
            if (Boundary)
               definition.mPoolTactic = PoolTactic::Type;
         #endif
      #endif
      
      // Calculate the allocation table                                 
      auto minElements = CT::GetMinAlloc<T>() / sizeof(T);
      for (size_t bit = 0; bit < sizeof(size_t) * 8u; ++bit) {
         const size_t threshold = size_t {1} << bit;
         const size_t elements = threshold / sizeof(T);
         definition.mAllocationTable[bit] = minElements > elements
            ? minElements : elements;
      }

      if constexpr (CT::Complete<Decay<T>>) {
         // Bases, verbs, morphisms and members come from origin        
         // so that we don't have unnecessary indirections when checking
         // type properties at runtime                                  
         definition.mCurrentBoundary.mBases
            = definition.mOrigin->mCurrentBoundary.mBases;
         definition.mCurrentBoundary.mVerbs
            = definition.mOrigin->mCurrentBoundary.mVerbs;
         definition.mCurrentBoundary.mMorphismsTo
            = definition.mOrigin->mCurrentBoundary.mMorphismsTo;
         definition.mCurrentBoundary.mMembers
            = definition.mOrigin->mCurrentBoundary.mMembers;
      }
      
      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
         LglsVerbose(
            Logger::Cyan, "Data ", definition.mNameOf,
            " (ID: ", definition.mID, ") ", Logger::Green,
            "registered from ", (Boundary?Boundary:"MAIN")
         );
      #else
         LglsVerbose(
            Logger::Cyan, "Data ", definition.mNameOf,
            Logger::Green, " reflected"
         );
      #endif
      
      return &definition;
   }
   
   inline DefinitionData::~DefinitionData() {
      LglsVerbose(Logger::Red, "Destroying data definition: ", Logger::Cyan, mNameOf);
   }
   

   /// Generate a member definition                                           
   ///   @return the generated member descriptor                              
   template<class HANDLE>
   auto DefinitionData::Member::From() -> Member {
      using THIS = typename HANDLE::Owner;
      static_assert(CT::NotConvoluted<THIS>, "Can't have qualifiers here");
      using DATA = typename HANDLE::Type;

      Member m;
      m.extent = ExtentOf<DATA>;
      m.member = [](void* owner) -> void* {
         auto context = static_cast<THIS*>(owner);
         return &(context->*HANDLE::Handle);
      };
      m.name = HANDLE::Name;
      m.name = m.name.substr(m.name.find_last_of(':') + 1);

      using AS = CT::ReflectedAs<Deext<DATA>>;
      using TAGS = TagsOf<DATA>;
      if constexpr (not CT::Void<TAGS>) {
         // Reflect the trait tag                                       
         m.type = Reflect<AS>;
         TAGS::ForEach([&m]<class T>{
            static_assert(CT::DefineTag<T>, "T is not a tag definition");
            m.tags.insert(DefinitionTag::Reflect<T>());
         });
      }
      else m.type = Reflect<AS>;
      return m;
   }
   
   /// Create a base descriptor for the derived type T                        
   ///   @attention private bases will end up as imposed                      
   ///   @return the generated base descriptor                                
   template<CT::Dense T, CT::Dense B>
   auto DefinitionData::Base::From() assumptious -> Base {
      using BASE = CT::ReflectedAs<B>;
      static_assert(not CT::Void<BASE>,
         "Can't have void as base");
      static_assert(CT::NotConvoluted<T, BASE>,
         "Can't have qualifiers here");
      static_assert(not Akin<T, BASE>,
         "Can't have base of the same type as the derived");
      static_assert(NameOf<T, false>() != NameOf<BASE, false>(),
         "T and BASE have the same NameOf, possibly due to inheritance. "
         "Specify a different CTTI::Named<T> or T::CTTI_Named for each!");

      Base result;
      result.type = Reflect<BASE>();

      if constexpr (CT::DerivedFrom<T, BASE>) {
         if constexpr (CT::VirtuallyDerivedFrom<T, BASE>) {
            // Needs to use slower dynamic_cast when base is virtual    
            result.getBase = [](void* from) noexcept -> void* {
               return dynamic_cast<BASE*>(static_cast<T*>(from));
            };
         }
         else {
            result.getBase = [](void* from) noexcept -> void* {
               return static_cast<BASE*>(static_cast<T*>(from));
            };
            
            // If sizes match and there's no byte offset, then the      
            // base and the derived type are binary compatible          
            result.binaryCompatible = (sizeof(BASE) == sizeof(T));
         }
      }

      return result;
   }
}

#include <Langulus/Logger/DisableVerbose.hpp>
