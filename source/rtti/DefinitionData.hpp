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
}

namespace Langulus::Flow
{
   struct Verb;
}

#if LANGULUS_FEATURE(MANAGED_MEMORY)
   namespace Langulus::Fractalloc
   {
      class Pool;
   }
#endif

namespace Langulus::RTTI
{

   ///                                                                        
   /// A data definition                                                      
   ///                                                                        
   class DefinitionData final : public Inner::Definition {
   protected:
      friend class Registry;
      friend class Inner::Definition;
      friend struct Inner::MetaDataNaked;
      friend struct Inner::MetaDataStructured_8_8;
      friend struct Inner::MetaDataStructured_16_16;
      friend struct Inner::MetaDataStructured_24_8;

      // The origin type, with all qualifiers and sparseness removed    
      // Will be nullptr for incomplete types                           
      DefinitionData const* mOrigin = nullptr;
      // The type, when a single level of indirection is removed        
      // Will be null if data is dense                                  
      DefinitionData const* mDeptr = nullptr;
      // The type, when all qualifiers are removed down to the origin   
      DefinitionData const* mDecvqAll IF_SAFE(= nullptr);
      // The type, when шдзпдяш qualifiers are removed                  
      DefinitionData const* mDecvqOnce IF_SAFE(= nullptr);

      // The type, but with an additional level of indirection          
      // @attention this is not null only after the pointer type has    
      //    been reflected elsewhere at runtime                         
      DefinitionData const* mAddPtr = nullptr;

      // The type, but constant                                         
      // @attention this is not null only after the constant type has   
      //    been reflected elsewhere at runtime                         
      DefinitionData const* mAddConst = nullptr;

      // Data instance size in bytes, set by sizeof()                   
      size_t mSize IF_SAFE(= 0);
      // Data instance alignment in bytes, set by alignof()             
      size_t mAlign IF_SAFE(= Alignment);
      // True if data is constant, set by CT::Constant                  
      bool mConst IF_SAFE(= false);
      // True if data is deep, set by CT::Deep                          
      bool mDeep IF_SAFE(= false);
      // True if data is pod, set by CT::POD                            
      bool mPOD IF_SAFE(= false);
      // Minimal pool allocation, in bytes                              
      size_t mAllocationPage IF_SAFE(= 0);
      // Precomputed counts indexed by MSB (avoids division by stride   
      // for that extra oompf)                                          
      size_t mAllocationTable[sizeof(size_t) * 8 + 1] IF_SAFE(= {});

      #if LANGULUS_FEATURE(MANAGED_MEMORY)
         // The reflected pool tactic                                   
         PoolTactic mPoolTactic = PoolTactic::Default;
         // The start of the pool chain for the type                    
         mutable Fractalloc::Pool* mPoolChain {};
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
      using FCompare = Compared(*)(void* lhs, void* rhs);
      FCompare mComparer {};

      // The refer/copy/disown/clone assignment, wrapped in a lambda    
      using FCopyAssign = void(*)(void* from, void* to);
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
      using FResolve = Anyness::Any(*)(void* self);
      FResolve mResolver {};

      // The hash getter, wrapped in a lambda expression                
      // Takes the pointer to the instance for hashing, returns the hash
      using FHash = Hash(*)(void* self);
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
      using FDispatch = void(*)(void* self, Flow::Verb& verb);
      FDispatch mDispatcherMut {};
      FDispatch mDispatcher {};


      /*using FVerbMutable = FDispatchMutable;
      using FVerbConstant = FDispatchConstant;
      using FTypeRetriever = DMeta(*)();
      using FTraitRetriever = TMeta(*)(int);
      using FDynamicCast = void* (*)(void*);*/
      DefinitionData(const Token& cppname, const Token& boundary)
         : Definition {cppname, boundary} {}

   public:
      template<class>
      static auto Reflect() -> DefinitionData const*;
   };

} // namespace Langulus::RTTI

#include "DefinitionData.inl"
