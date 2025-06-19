///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Definition.hpp"
#include <Langulus/CT/Comparable.hpp>

#if LANGULUS_FEATURE(MANAGED_MEMORY)
   #include <Langulus/CT/Pooled.hpp>
#endif


namespace Langulus::Anyness
{

   struct Many;
   struct Any;

} // namespace Langulus::Anyness

namespace Langulus::Flow
{

   struct Verb;

} // namespace Langulus::Flow

namespace Langulus::RTTI
{

   ///                                                                        
   /// A data definition                                                      
   ///                                                                        
   class DefinitionData : public Inner::Definition {
   protected:
      friend struct Inner::MetaDataNaked;
      friend struct Inner::MetaDataStructured_8_8;
      friend struct Inner::MetaDataStructured_16_16;
      friend struct Inner::MetaDataStructured_24_8;

      // The origin type, with all qualifiers and sparseness removed    
      // Will be nullptr for incomplete types                           
      DefinitionData const* mOrigin;
      // The type, when a single pointer is removed                     
      // Will be null if data is dense                                  
      DefinitionData const* mDeptr;
      // The type, when all qualifiers are removed down to the origin   
      DefinitionData const* mDecvq;

      // Data instance size in bytes, set by sizeof()                   
      size_t mSize;
      // Data instance alignment in bytes, set by alignof()             
      size_t mAlign;
      // True if data is constant, set by CT::Constant                  
      bool mConst;
      // True if data is deep, set by CT::Deep                          
      bool mDeep;
      // True if data is pod, set by CT::POD                            
      bool mPOD;
      // Minimal pool allocation, in bytes                              
      size_t mAllocationPage;
      // Precomputed counts indexed by MSB (avoids division by stride   
      // for that extra oompf)                                          
      size_t mAllocationTable[sizeof(size_t) * 8 + 1];
      
      #if LANGULUS_FEATURE(MANAGED_MEMORY)
         // The reflected pool tactic                                   
         PoolTactic mPoolTactic = PoolTactic::Default;
         // The start of the pool chain for the type                    
         mutable void* mPoolChain {};
      #endif

      //                                                                
      //   These methods are sought in each reflected type              
      //                                                                
      // The default constructor, wrapped in a lambda expression if     
      // available. Takes a pointer for a placement-new expression      
      using FDefaultConstruct = void(*)(void* self);
      FDefaultConstruct mDefaultConstructor {};

      // Constructor by descriptor                                      
      // Takes a pointer for a placement-new expression, and a Many     
      using FDescribeConstruct = void(*)(void* self, const Anyness::Many& describe);
      FDescribeConstruct mDescribeConstructor {};

      // The refer/copy/disown/clone constructor, wrapped in lambda     
      // Takes a pointer for a placement-new expression, and a source   
      using FCopyConstruct = void(*)(const void* from, void* to);
      FCopyConstruct mReferConstructor {};
      FCopyConstruct mCopyConstructor {};
      FCopyConstruct mDisownConstructor {};
      FCopyConstruct mCloneConstructor {};

      // The move/abandon constructor, wrapped in a lambda expression   
      // Takes a pointer for a placement-new expression, and a source   
      using FMoveConstruct = void(*)(void* from, void* to);
      FMoveConstruct mMoveConstructor {};
      FMoveConstruct mAbandonConstructor {};

      // The destructor, wrapped in a lambda expression                 
      // Takes the pointer to the instance for destruction              
      using FDestroy = void(*)(void* self);
      FDestroy mDestructor {};

      // The <=> operator, wrapped in a lambda expression if available  
      using FCompare = Compared(*)(const void* lhs, const void* rhs);
      FCompare mComparer {};

      // The refer/copy/disown/clone assignment, wrapped in a lambda    
      using FCopyAssign = void(*)(const void* from, void* to);
      FCopyAssign mReferAssigner {};
      FCopyAssign mCopyAssigner {};
      FCopyAssign mDisownAssigner {};
      FCopyAssign mCloneAssigner {};

      // The move/abandon-assignment operator, wrapped in a lambda      
      // expression                                                     
      using FMoveAssign = void(*)(void* from, void* to);
      FMoveAssign mMoveAssigner {};
      FMoveAssign mAbandonAssigner {};

      // The class type function, wrapped in a lambda expression        
      // Returns a typed container with the most concrete class instance
      using FResolve = Anyness::Any(*)(const void* self);
      FResolve mResolver {};

      // The hash getter, wrapped in a lambda expression                
      // Takes the pointer to the instance for hashing, returns the hash
      using FHash = Hash(*)(const void* self);
      FHash mHasher {};
      // Decides whether POD data is batch-hashable or not              
      // If there's a custom GetHash() method, POD data is not batchable
      bool mHasGetHashMethod = false;

      // The reference function wrapped in a lambda                             
      // Takes the pointer to the instance for referencing                      
      // Returns the number of references after being referenced                
      // (use 0 modifier to just get references)                                
      using FReference = int(*)(void* self, int modifier);
      FReference mReferencer {};

      // A custom verb dispatcher, wrapped in a lambda expression               
      // Takes the pointer to the instance that will dispatch, and a verb       
      // There is a mutable and immutable version of this                       
      using FDispatchMutable = void(*)(void* self, Flow::Verb& verb);
      using FDispatchConstant = void(*)(void const* self, Flow::Verb& verb);
      FDispatchMutable mDispatcherMut {};
      FDispatchConstant mDispatcher {};


      /*using FVerbMutable = FDispatchMutable;
      using FVerbConstant = FDispatchConstant;
      using FTypeRetriever = DMeta(*)();
      using FTraitRetriever = TMeta(*)(int);
      using FDynamicCast = void* (*)(void*);*/
      explicit DefinitionData(const Token& cppname) : Definition {cppname} {}

   public:
      template<class>
      static auto Reflect() -> DefinitionData const*;
   };

} // namespace Langulus::RTTI

#include "DefinitionData.inl"
