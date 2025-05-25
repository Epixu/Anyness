#pragma once
#include "Definition.hpp"
#include "MetaData.hpp"


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
      // The origin type, with all qualifiers and sparseness removed    
      // Will be nullptr for incomplete types                           
      MetaData mOrigin;
      // The type, when a single pointer is removed                     
      // Will be null if data is dense                                  
      MetaData mDeptr;
      // A unique handle for this definition                            
      MetaData mThis;

      // Data instance size in bytes, set by sizeof()                   
      size_t mSize;
      // Data instance alignment in bytes, set by alignof()             
      size_t mAlign;
      // True if data is constant, set by CT::Constant                  
      bool mConst;
      // Minimal pool allocation, in bytes                              
      size_t mAllocationPage;
      // Precomputed counts indexed by MSB (avoids division by stride)  
      size_t mAllocationTable[sizeof(size_t) * 8 + 1];

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
      // Compares two instances for lesser (-1)/equal(0)/greater(1)     
      using FCompare = int(*)(const void* lhs, const void* rhs);
      FCompare mComparer {};

      // The refer/copy/disown/clone assignment, wrapped in a lambda    
      using FCopyAssign = void(*)(const void* from, void* to);
      FCopyAssign mReferAssignment {};
      FCopyAssign mCopyAssignment {};
      FCopyAssign mDisownAssignment {};
      FCopyAssign mCloneAssignment {};

      // The move/abandon-assignment operator, wrapped in a lambda      
      // expression                                                     
      using FMoveAssign = void(*)(void* from, void* to);
      FMoveAssign mMoveAssignment {};
      FMoveAssign mAbandonAssignment {};

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

   public:
      friend struct MetaData;
      DefinitionData(const Token& cppname) : Definition {cppname} {}

      template<class>
      static DMeta Reflect();
   };

} // namespace Langulus::RTTI

#include "DefinitionData.inl"