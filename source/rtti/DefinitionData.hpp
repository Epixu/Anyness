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
      template<unsigned, unsigned>
      friend struct Inner::MetaDataStructured_XY;

      // The origin type, with all qualifiers and sparseness removed    
      // Will be nullptr for incomplete types                           
      DefinitionData const* mOrigin = nullptr;
      // The type, when a single level of indirection is removed        
      // Will be null if data is dense                                  
      DefinitionData const* mDeptr = nullptr;
      // The type, when all qualifiers are removed down to the origin   
      DefinitionData const* mDecvqAll IF_SAFE(= nullptr);
      // The type, when topmost qualifiers are removed                  
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
      // True if data is nullable, set by CT::Nullable                  
      bool mNullable IF_SAFE(= false);
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

      // Decides whether POD data is batch-hashable or not              
      // If there's a custom GetHash() method, POD data is not batchable
      bool mHasGetHashMethod = false;
      
      //                                                                
      //    These methods are sought in each reflected type             
      //                                                                
      //    These function pointers will be different for different     
      // libraries. We just collect them all. If a shared object is     
      // unloaded, we simply pick a pointer from another. Once the data 
      // is reflected from the MainBoundary, the maps are cleared and   
      // only the main code is used, because it is most persistent.     
      using FUnary = void(*)(void* self);
      using FBinary = void(*)(void* from, void* to);
      using FDescribeConstruct = void(*)(void* self, const Anyness::Many& describe);
      using FCompare = Compared(*)(void* lhs, void* rhs);
      using FResolve = Anyness::Any(*)(void* self);
      using FHash = Hash(*)(void* self);
      using FReference = int(*)(void* self, int modifier);
      using FDispatch = void(*)(void* self, Flow::Verb& verb);

      struct BoundaryDependent {
         // The default constructor, wrapped in a lambda expression if  
         // available. Takes a pointer for a placement-new expression   
         FUnary mDefaultConstructor {};

         // Constructor by descriptor                                   
         // Takes a pointer for a placement-new expression, and a Many  
         FDescribeConstruct mDescribeConstructor {};

         // The refer/copy/disown/clone/move/abandon constructors,      
         // wrapped in lambdas. They take a pointer for a placement-new 
         // expression and a source                                     
         FBinary mReferConstructor {};
         FBinary mCopyConstructor {};
         FBinary mDisownConstructor {};
         FBinary mCloneConstructor {};
         FBinary mMoveConstructor {};
         FBinary mAbandonConstructor {};

         // The destructor, wrapped in a lambda expression              
         // Takes the pointer to the instance for destruction           
         FUnary mDestructor {};

         // The <=> operator, wrapped in lambda expression if available 
         FCompare mComparer {};

         // The refer/copy/disown/clone/move/abandon assignment, wrapped
         // in a lambdas                                                
         FBinary mReferAssigner {};
         FBinary mCopyAssigner {};
         FBinary mDisownAssigner {};
         FBinary mCloneAssigner {};
         FBinary mMoveAssigner {};
         FBinary mAbandonAssigner {};

         // The class type function, wrapped in a lambda expression     
         // Returns typed container with most concrete class instance   
         FResolve mResolver {};

         // The hash getter, wrapped in a lambda expression             
         // Takes pointer to the instance for hashing, returns the hash 
         FHash mHasher {};

         // The reference function wrapped in a lambda                  
         // Takes the pointer to the instance for referencing           
         // Returns the number of references after being referenced     
         // (use 0 modifier to just get references)                     
         FReference mReferencer {};

         // A custom verb dispatcher, wrapped in a lambda expression    
         // Takes pointer to the instance that will dispatch, and a verb
         // There is a mutable and immutable version of this            
         FDispatch mDispatcherMut {};
         FDispatch mDispatcher {};
      };

      // The currently used boundary                                    
      BoundaryDependent mCurrentBoundary;
      
      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
         // All functions, reflected from all points of view            
         // If this map is empty, then data has been reflected from the 
         // main boundary                                               
         ::std::unordered_map<Token, BoundaryDependent> mOtherBoundaries;
      #endif
      
      explicit DefinitionData(const Token& cppname) noexcept
         : Definition {cppname} {}

   public:
      template<class>
      static auto Reflect() -> DefinitionData const*;
   };

} // namespace Langulus::RTTI

#include "DefinitionData.inl"
