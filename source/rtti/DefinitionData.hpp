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
#include <Langulus/CT/DefineTag.hpp>
#include <Langulus/CT/DefineVerb.hpp>

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
      // True if data is abstract, set by CT::Abstract                  
      bool mAbstract IF_SAFE(= false);
      // Minimal pool allocation, in bytes                              
      size_t mAllocationPage IF_SAFE(= 0);
      // Precomputed counts indexed by MSB (avoids division by stride   
      // for that extra oompf)                                          
      size_t mAllocationTable[sizeof(size_t) * 8 + 1] IF_SAFE(= {});
      // Reflected suffix                                               
      ::std::string mSuffixOf;
      // Reflected file extensions, separated with commas               
      ::std::string mFilesOf;
      
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
      using FDescribe = void(*)(void* self, const Anyness::Many& describe);
      using FCompare = Compared(*)(void* lhs, void* rhs);
      using FResolve = Anyness::Any(*)(void* self);
      using FHash = Hash(*)(void* self);
      using FReference = int(*)(void* self, int modifier);
      using FDispatch = void(*)(void* self, Flow::Verb& verb);
      using FAccessMember = void* (*)(void* owner);
      using FTagRetriever = DefinitionTag const* (*)(int index);
      using FTypeRetriever = DefinitionData const* (*)();
      
      /// Type-erased member variable reflection                              
      struct Member {
         using CTTI_ReflectAs = void;

         // Type of data                                                
         FTypeRetriever type IF_SAFE(= nullptr);
         // Get pointer to the member                                   
         FAccessMember member IF_SAFE(= nullptr);
         // Number of elements in mData (in case of an array)           
         size_t extent = 1;
         // Tags                                                        
         FTagRetriever getTag = nullptr;

         Member(const auto&);

      private:
         template<CT::DefineTag...T>
         static auto TagSelector(int, Types<T...>&&) -> DefinitionTag const*;
      };
      
      /// Ability reflection                                                  
      struct Ability {
         using CTTI_ReflectAs = void;

         // For functions that can mutate the context                   
         FDispatch callMut = nullptr;
         // For functions that can't mutate the context                 
         FDispatch call    = nullptr;

         template<class T, CT::DefineVerb V>
         static Ability From() noexcept;
      };

      /// Used to reflect a base for a t                                      
      struct Base {
         using CTTI_ReflectAs = void;

         // Type of the base                                            
         DefinitionData const* type IF_SAFE(= nullptr);
         // Number of bases that fit in the type                        
         size_t count = 1;
         // Offset of the base, relative to the derived type            
         // @attention valid only if not 'virtualBase'                  
         size_t offset = 0;
         // Used to map one type onto another                           
         // Usually true when base completely fills the derived type    
         bool binaryCompatible = false;
         // Whether or not this base is considered 'imposed'            
         // Basically, imposed bases are not serialized and don't       
         // act in distance computation or dispatching                  
         // An imposed base can be added only manually                  
         bool imposed = false;
         // Only possible way to get pointer to a virtual base is       
         // through a lambda. Nullptr if base is not virtual            
         FAccessMember virtualBase = nullptr;
         
         template<CT::Dense T, CT::Dense BASE>
         static Base From() has_assumptions;
      };
      
      /// Used to reflect data coversions                                     
      struct Morphism {
         using CTTI_ReflectAs = void;

         // The data ID we're converting to                             
         //DefinitionData const* type IF_SAFE(= nullptr);
         // Address of function to call                                 
         FBinary call IF_SAFE(= nullptr);

         template<CT::Decayed FROM, CT::Decayed TO>
         static Morphism From(DefinitionData const*) noexcept;
      };

      using MemberList = ::std::vector<Member>;
      using AbilityList = ::std::unordered_map<DefinitionVerb const*, Ability>;
      using BaseList = ::std::vector<Base>;
      using MorphismList = ::std::unordered_map<DefinitionData const*, Morphism>;
      using ValuesList = ::std::vector<DefinitionConst const*>;
      
      ///                                                                     
      struct BoundaryDependent {
         using CTTI_ReflectAs = void;

         // The default constructor, wrapped in a lambda expression if  
         // available. Takes a pointer for a placement-new expression   
         FUnary mDefaultConstructor = nullptr;

         // Constructor by descriptor                                   
         // Takes a pointer for a placement-new expression, and a Many  
         FDescribe mDescribeConstructor = nullptr;

         // The refer/copy/disown/clone/move/abandon constructors,      
         // wrapped in lambdas. They take a pointer for a placement-new 
         // expression and a source                                     
         FBinary mReferConstructor = nullptr;
         FBinary mCopyConstructor = nullptr;
         FBinary mDisownConstructor = nullptr;
         FBinary mCloneConstructor = nullptr;
         FBinary mMoveConstructor = nullptr;
         FBinary mAbandonConstructor = nullptr;

         // The destructor, wrapped in a lambda expression              
         // Takes the pointer to the instance for destruction           
         FUnary mDestructor = nullptr;

         // The <=> operator, wrapped in lambda expression if available 
         FCompare mComparer = nullptr;

         // The refer/copy/disown/clone/move/abandon assignment, wrapped
         // in a lambdas                                                
         FBinary mReferAssigner = nullptr;
         FBinary mCopyAssigner = nullptr;
         FBinary mDisownAssigner = nullptr;
         FBinary mCloneAssigner = nullptr;
         FBinary mMoveAssigner = nullptr;
         FBinary mAbandonAssigner = nullptr;

         // The class type function, wrapped in a lambda expression     
         // Returns typed container with most concrete class instance   
         FResolve mResolver = nullptr;

         // The hash getter, wrapped in a lambda expression             
         // Takes pointer to the instance for hashing, returns the hash 
         FHash mHasher = nullptr;

         // The reference function wrapped in a lambda                  
         // Takes the pointer to the instance for referencing           
         // Returns the number of references after being referenced     
         // (use 0 modifier to just get references)                     
         FReference mReferencer = nullptr;

         // A custom verb dispatcher, wrapped in a lambda expression    
         // Takes pointer to the instance that will dispatch, and a verb
         // There is a mutable and immutable version of this            
         FDispatch mDispatcherMut = nullptr;
         FDispatch mDispatcher = nullptr;

         // Default concretization                                      
         // Used as redirection when requesting the creation of abstract
         FTypeRetriever mConcrete = nullptr;
         
         // Types with producers can be instantiated only by the        
         // invocation of Verbs::Create in the context of the producer  
         FTypeRetriever mProducer = nullptr;

         // List of reflected members of the origin type                
         MemberList mMembers;
         
         // List of reflected abilities of the origin type              
         AbilityList mAbilities;

         // List of reflected bases of the origin type                  
         BaseList mBases;
         
         // List of reflected converters to/from the origin type        
         MorphismList mMorphismsTo;
         MorphismList mMorphismsFrom;
      };
      
      // List of named values                                           
      ValuesList mNamedValues;

      // The currently used boundary                                    
      BoundaryDependent mCurrentBoundary;
      
      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
         // All functions, reflected from all points of view            
         // If this map is empty, then data has been reflected from the 
         // main boundary                                               
         ::std::unordered_map<Token, BoundaryDependent> mOtherBoundaries;
      #endif

   public:
      using CTTI_ReflectAs = void;

      template<class>
      static auto Reflect() -> DefinitionData const*;
      
      DefinitionData(const Token& cppname) noexcept
         : Definition {cppname} {}
   };

} // namespace Langulus::RTTI

#include "DefinitionData.inl"
