///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once

#if not LANGULUS_FEATURE(MANAGED_REFLECTION)
   #error "This file shouldn't be included if MANAGED_REFLECTION feature is disabled"
#endif

#define TEMPLATE() template<unsigned ID_SIZE, unsigned PT_SIZE>
#define ME() MetaDataStructured_XY<ID_SIZE, PT_SIZE>


namespace Langulus::RTTI::Inner
{
   /// Empty data ID construction                                             
   TEMPLATE()
   constexpr ME()::MetaDataStructured_XY(nullptr_t) noexcept
      : Base {0} {}

   /// ID from definition                                                     
   TEMPLATE()
   constexpr ME()::MetaDataStructured_XY(DefinitionData const* d) noexcept
      : Base {d ? d->mID : 0} {
      if (d) {
         sparse     = d->mDeptr != nullptr;
         constant   = d->mConst;
         deep       = d->mDeep;
         pod        = d->mPOD;
         nullable   = d->mNullable;
         referenced = d->mCurrentBoundary.mReferencer != nullptr;
         resolvable = d->mCurrentBoundary.mResolver != nullptr;
         dispatcher = d->mCurrentBoundary.mDispatcher != nullptr;
      }
   }

   /// Reset data ID                                                          
   TEMPLATE()
   constexpr auto ME()::operator = (nullptr_t)
   noexcept -> MetaDataStructured_XY& {
      Base::operator = (0);
      return *this;
   }

   /// Reassign data ID                                                       
   TEMPLATE()
   constexpr auto ME()::operator = (DefinitionData const* d)
   noexcept -> MetaDataStructured_XY& {
      Base::operator = (d ? d->mID : 0);

      if (d) {
         sparse     = d->mConst;
         constant   = d->mConst;
         deep       = d->mDeep;
         pod        = d->mPOD;
         nullable   = d->mNullable;
         referenced = d->mCurrentBoundary.mReferencer != nullptr;
         resolvable = d->mCurrentBoundary.mResolver != nullptr;
         dispatcher = d->mCurrentBoundary.mDispatcher != nullptr;
      }
      return *this;
   }

   /// Get definition                                                         
   TEMPLATE()
   auto ME()::GetDefinition() const noexcept
   -> DefinitionData const* {
      return Instance.GetMetaDataByID(Base::GetID(), sparse, constant);
   }

   /// Check if type origins match                                            
   /// Disregards all cv-qualifiers, pointers, array extents, etc.            
   ///   @param other - the type to compare against                           
   ///   @return true if types match                                          
   TEMPLATE()
   bool ME()::Is(const MetaDataStructured_XY& other) const noexcept {
      return GetDefinition()->mOrigin == other.GetDefinition()->mOrigin;
   }

   /// Check if two meta definitions match exactly                            
   ///   @param other - the type to compare against                           
   ///   @return true if types match                                          
   TEMPLATE()
   constexpr bool ME()::IsExact(const MetaDataStructured_XY& other) const noexcept {
      return all == other.all and Base::operator == (other);
   }

   TEMPLATE()
   constexpr bool ME()::operator==(const MetaDataStructured_XY& other) const noexcept {
      return IsExact(other);
   }
   
   /// Check if two meta definitions match origin and sparseness, but ignores 
   /// `const` and `volatile` qualifiers. The qualifiers aren't ignored only  
   /// on the current level of indirection, but on the entire way to origin   
   ///   @param other - the type to compare against                           
   ///   @return true if types match                                          
   TEMPLATE()
   constexpr bool ME()::IsSame(const MetaDataStructured_XY& other) const noexcept {
      return Base::operator == (other);
   }

   /// Get the size of the type                                               
   TEMPLATE()
   constexpr auto ME()::GetSize() const noexcept -> size_t {
      if constexpr (PT_SIZE > 1)
         return Structured<PT_SIZE>::size ? Structured<PT_SIZE>::size : GetDefinition()->mSize;
      else
         return GetDefinition()->mSize;
   }

   /// Get the alignment of the type                                          
   TEMPLATE()
   auto ME()::GetAlignment() const noexcept -> size_t {
      return GetDefinition()->mAlign;
   }

   /// Get the name of the type, the result of NameOf                         
   TEMPLATE()
   auto ME()::GetName() const noexcept -> Token {
      return GetDefinition()->mNameOf;
   }
   
   /// Get the info of the type, the result of InfoOf                         
   TEMPLATE()
   auto ME()::GetInfo() const noexcept -> Token {
      return GetDefinition()->mInfoOf;
   }

   /// Get the name of the type as it appearch in C++                         
   TEMPLATE()
   auto ME()::GetCppName() const noexcept -> Token {
      return GetDefinition()->mCppNameOf;
   }

   /// Get the type hash                                                      
   TEMPLATE()
   auto ME()::GetHash() const noexcept -> Hash {
      return GetDefinition()->mHash;
   }

   /// Get the associated file extensions, separated with commas              
   TEMPLATE()
   auto ME()::GetFiles() const noexcept -> Token {
      return GetDefinition()->mFilesOf;
   }

   /// Get the associated suffix                                              
   TEMPLATE()
   auto ME()::GetSuffix() const noexcept -> Token {
      return GetDefinition()->mSuffixOf;
   }

   /// Get the type boundaries                                                
   TEMPLATE()
   auto ME()::GetBoundaries() const noexcept -> Definition::BoundarySet const& {
      return GetDefinition()->mBoundaries;
   }

   /// Get the major version                                                  
   TEMPLATE()
   auto ME()::GetVersionMajor()  const noexcept -> unsigned {
      return GetDefinition()->mVersionMajor;
   }

   /// Get the minor version                                                  
   TEMPLATE()
   auto ME()::GetVersionMinor()  const noexcept -> unsigned {
      return GetDefinition()->mVersionMinor;
   }
   
   /// Get the minimal allocation size in bytes                               
   TEMPLATE()
   auto ME()::GetMinAllocation() const noexcept -> size_t {
      return GetDefinition()->mMinimalAllocation;      
   }

   #if LANGULUS_FEATURE(MANAGED_MEMORY)
      /// Get the reflected allocation page                                   
      TEMPLATE()
      auto ME()::GetMinPoolsize() const noexcept -> size_t {
         return GetDefinition()->mMinimalPoolSize;      
      }
   
      /// Get the reflected pool tactic                                       
      TEMPLATE()
      auto ME()::GetPoolTactic() const noexcept -> PoolTactic {
         return GetDefinition()->mPoolTactic;
      }

      /// Get the poolchain                                                   
      TEMPLATE()
      auto ME()::GetPoolchain() const noexcept -> Fractalloc::Pool* {
         return GetDefinition()->mPoolChain;
      }
      
      /// Allows the memory manager to set a new pool chain                   
      TEMPLATE()
      void ME()::SetPoolchain(Fractalloc::Pool* pool) const noexcept {
         GetDefinition()->mPoolChain = pool;
      }
   #endif

   /// Check if type is CT::Dense                                             
   TEMPLATE()
   constexpr bool ME()::IsDense() const noexcept {
      return not sparse;
   }

   /// Check if type is CT::Sparse                                            
   TEMPLATE()
   constexpr bool ME()::IsSparse() const noexcept {
      return sparse;
   }

   /// Check if the type is CT::Constant                                      
   TEMPLATE()
   constexpr bool ME()::IsConstant() const noexcept {
      return constant;
   }

   /// Check if the type is CT::Mutable                                       
   TEMPLATE()
   constexpr bool ME()::IsMutable() const noexcept {
      return not constant;
   }

   /// Check if type is CT::Deep                                              
   TEMPLATE()
   constexpr bool ME()::IsDeep() const noexcept {
      return deep;
   }

   /// Check if type is CT::POD                                               
   TEMPLATE()
   constexpr bool ME()::IsPOD() const noexcept {
      return pod;
   }

   /// Check if type is CT::Nullable                                          
   TEMPLATE()
   constexpr bool ME()::IsNullable() const noexcept {
      return nullable;
   }

   /// Check if type is CT::Abstract                                          
   TEMPLATE()
   constexpr bool ME()::IsAbstract() const noexcept {
      return GetDefinition()->mAbstract;
   }

   /// Check if type has an explicit GetHash() method                         
   TEMPLATE()
   constexpr bool ME()::HasGetHashMethod() const noexcept {
      return GetDefinition()->mHasGetHashMethod;
   }
   
   /// Get the reflected destructor                                           
   TEMPLATE()
   auto ME()::GetDestructor()
   const noexcept -> DefinitionData::FUnary {
      return GetDefinition()->mCurrentBoundary.mDestructor;
   }

   /// Get the reflected referencer                                           
   TEMPLATE()
   auto ME()::GetReferencer()
   const noexcept -> DefinitionData::FReference {
      return GetDefinition()->mCurrentBoundary.mReferencer;
   }

   /// Get the reflected resolver                                             
   TEMPLATE()
   auto ME()::GetResolver()
   const noexcept -> DefinitionData::FResolve {
      return GetDefinition()->mCurrentBoundary.mResolver;
   }

   /// Get the reflected default constructor                                  
   TEMPLATE()
   auto ME()::GetDefaultConstructor() const noexcept -> DefinitionData::FUnary {
      return GetDefinition()->mCurrentBoundary.mDefaultConstructor;
   }
   
   /// Get the reflected describe-constructo                                  
   TEMPLATE()
   auto ME()::GetDescribeConstructor() const noexcept -> DefinitionData::FDescribe {
      return GetDefinition()->mCurrentBoundary.mDescribeConstructor;
   }   

   /// Get the reflected refer-constructor                                    
   TEMPLATE()
   auto ME()::GetReferConstructor()
   const noexcept -> DefinitionData::FBinary {
      return GetDefinition()->mCurrentBoundary.mReferConstructor;
   }

   /// Get the reflected refer-assigner                                       
   TEMPLATE()
   auto ME()::GetReferAssigner()
   const noexcept -> DefinitionData::FBinary {
      return GetDefinition()->mCurrentBoundary.mReferAssigner;
   }

   /// Get the reflected move-constructor                                     
   TEMPLATE()
   auto ME()::GetMoveConstructor()
   const noexcept -> DefinitionData::FBinary {
      return GetDefinition()->mCurrentBoundary.mMoveConstructor;
   }

   /// Get the reflected move-assigner                                        
   TEMPLATE()
   auto ME()::GetMoveAssigner()
   const noexcept -> DefinitionData::FBinary {
      return GetDefinition()->mCurrentBoundary.mMoveAssigner;
   }

   /// Get the reflected abandon-constructor                                  
   TEMPLATE()
   auto ME()::GetAbandonConstructor()
   const noexcept -> DefinitionData::FBinary {
      return GetDefinition()->mCurrentBoundary.mAbandonConstructor;
   }

   /// Get the reflected abandon-assigner                                     
   TEMPLATE()
   auto ME()::GetAbandonAssigner()
   const noexcept -> DefinitionData::FBinary {
      return GetDefinition()->mCurrentBoundary.mAbandonAssigner;
   }

   /// Get the reflected disown-constructor                                   
   TEMPLATE()
   auto ME()::GetDisownConstructor()
   const noexcept -> DefinitionData::FBinary {
      return GetDefinition()->mCurrentBoundary.mDisownConstructor;
   }

   /// Get the reflected disown-assigner                                      
   TEMPLATE()
   auto ME()::GetDisownAssigner()
   const noexcept -> DefinitionData::FBinary {
      return GetDefinition()->mCurrentBoundary.mDisownAssigner;
   }

   /// Get the reflected clone-constructor                                    
   TEMPLATE()
   auto ME()::GetCloneConstructor()
   const noexcept -> DefinitionData::FBinary {
      return GetDefinition()->mCurrentBoundary.mCloneConstructor;
   }

   /// Get the reflected clone-assigner                                       
   TEMPLATE()
   auto ME()::GetCloneAssigner()
   const noexcept -> DefinitionData::FBinary {
      return GetDefinition()->mCurrentBoundary.mCloneAssigner;
   }

   /// Get the reflected copy-constructor                                     
   TEMPLATE()
   auto ME()::GetCopyConstructor()
   const noexcept -> DefinitionData::FBinary {
      return GetDefinition()->mCurrentBoundary.mCopyConstructor;
   }

   /// Get the reflected copy-assigner                                        
   TEMPLATE()
   auto ME()::GetCopyAssigner()
   const noexcept -> DefinitionData::FBinary {
      return GetDefinition()->mCurrentBoundary.mCopyAssigner;
   }

   /// Get the reflected comparer                                             
   TEMPLATE()
   auto ME()::GetComparer()
   const noexcept -> DefinitionData::FCompare {
      return GetDefinition()->mCurrentBoundary.mComparer;
   }

   /// Get the reflected hasher                                               
   TEMPLATE()
   auto ME()::GetHasher()
   const noexcept -> DefinitionData::FHash {
      return GetDefinition()->mCurrentBoundary.mHasher;
   }

   /// Get the reflected dispatcher                                           
   TEMPLATE()
   auto ME()::GetDispatcher()
   const noexcept -> DefinitionData::FDispatch {
      return GetDefinition()->mCurrentBoundary.mDispatcher;  
   }

   /// Remove a layer of indirection                                          
   ///   @attention will return invalid meta if type is incomplete            
   TEMPLATE()
   auto ME()::GetDeptr()
   const -> MetaDataStructured_XY {
      auto d = GetDefinition();
      return d->mDeptr <= reinterpret_cast<DefinitionData*>(intptr_t {1})
         ? nullptr
         : d->mDeptr;
   }
   
   /// Get the origin type, if complete                                       
   /// The origin type has all indirections and qualifiers removed            
   TEMPLATE()
   auto ME()::GetOrigin()
   const -> MetaDataStructured_XY {
      return GetDefinition()->mOrigin;
   }
   
   /// Strip all qualifiers from all levels of indirection                    
   TEMPLATE()
   auto ME()::GetDecvqAll()
   const -> MetaDataStructured_XY {
      return GetDefinition()->mDecvqAll;
   }
   
   /// Strip topmost qualifiers                                               
   TEMPLATE()
   auto ME()::GetDecvq()
   const -> MetaDataStructured_XY {
      return GetDefinition()->mDecvqOnce;
   }
   
   /// Add a level of indirection to the type                                 
   ///   @attention this is possible only if that level of indirection has    
   ///      already been reflected at runtime prior to calling this function  
   TEMPLATE()
   auto ME()::AddPtr()
   const -> MetaDataStructured_XY {
      auto ptr = GetDefinition()->mAddPtr;
      LglsAssert(ptr, "Pointer type hasn't been reflected yet");
      return ptr;
   }
   
   /// Add a constant qualifier to the type                                   
   ///   @attention this is possible only if the qualified type has           
   ///      already been reflected at runtime prior to calling this function  
   TEMPLATE()
   auto ME()::AddConst()
   const -> MetaDataStructured_XY {
      auto cnst = GetDefinition()->mAddConst;
      LglsAssert(cnst, "Constant type hasn't been reflected yet");
      return cnst;
   }
   
   /// Get the default concretization for an abstract type                    
   TEMPLATE()
   auto ME()::GetConcrete()
   const -> MetaDataStructured_XY {
      auto d = GetDefinition();
      return d->mCurrentBoundary.mConcrete
         ? d->mCurrentBoundary.mConcrete()
         : nullptr;
   }
   
   /// Get the runtime producer of the type, if any                           
   TEMPLATE()
   auto ME()::GetProducer()
   const -> MetaDataStructured_XY {
      auto d = GetDefinition();
      return d->mCurrentBoundary.mProducer
         ? d->mCurrentBoundary.mProducer()
         : nullptr;
   }

   /// Get the reflected bases                                                
   TEMPLATE()
   auto ME()::GetBases()
   const noexcept -> DefinitionData::BaseList const& {
      return GetDefinition()->mCurrentBoundary.mBases;
   }
   
   /// Get the reflected verbs                                                
   TEMPLATE()
   auto ME()::GetVerbs()
   const noexcept -> DefinitionData::VerbList const& {
      return GetDefinition()->mCurrentBoundary.mVerbs;
   }
   
   /// Get the reflected members                                              
   TEMPLATE()
   auto ME()::GetMembers()
   const noexcept -> DefinitionData::MemberList const& {
      return GetDefinition()->mCurrentBoundary.mMembers;
   }
   
   /// Get the reflected named values                                         
   TEMPLATE()
   auto ME()::GetNamedValues()
   const noexcept -> DefinitionData::ValuesList const& {
      return GetDefinition()->mNamedValues;
   }
   
   /// Get morphisms to other types                                           
   TEMPLATE()
   auto ME()::GetMorphismsTo()
   const noexcept -> DefinitionData::MorphismList const& {
      return GetDefinition()->mCurrentBoundary.mMorphismsTo;
   }
   
   /// Get morphisms from other types                                         
   TEMPLATE()
   auto ME()::GetMorphismsFrom()
   const noexcept -> DefinitionData::MorphismList const& {
      return GetDefinition()->mCurrentBoundary.mMorphismsFrom;
   }

   /// Get a specific coverter, if it exists                                  
   TEMPLATE()
   auto ME()::GetMorphism(MetaDataStructured_XY to)
   const noexcept -> DefinitionData::FBinary {
      auto toDef = to.GetDefinition();
      auto found = GetDefinition()->mCurrentBoundary.mMorphismsTo.find(toDef);
      if (found != GetDefinition()->mCurrentBoundary.mMorphismsTo.end())
         return found->second;
      return nullptr;
   }

#if LANGULUS(SAFE)
   TEMPLATE() ME()::operator bool() const noexcept {
      if (Base::operator bool()) {
         LglsAssert(GetDefinition(), "Valid meta with invalid definition");
         return true;
      }
      return false;
   }
#endif
}

#undef TEMPLATE
#undef ME
