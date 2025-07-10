///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once


namespace Langulus::RTTI::Inner
{

   /// Empty data ID construction                                             
   template<unsigned S1, unsigned S2>
   constexpr MetaDataStructured_XY<S1, S2>::MetaDataStructured_XY(nullptr_t) noexcept
      : Base {0} {}

   /// ID from definition                                                     
   template<unsigned S1, unsigned S2>
   constexpr MetaDataStructured_XY<S1, S2>::MetaDataStructured_XY(DefinitionData const* d) noexcept
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
   template<unsigned S1, unsigned S2>
   constexpr auto MetaDataStructured_XY<S1, S2>::operator = (nullptr_t)
   noexcept -> MetaDataStructured_XY& {
      Base::operator = (0);
      return *this;
   }

   /// Reassign data ID                                                       
   template<unsigned S1, unsigned S2>
   constexpr auto MetaDataStructured_XY<S1, S2>::operator = (DefinitionData const* d)
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
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetDefinition() const noexcept
   -> DefinitionData const* {
      return Instance.GetMetaDataByID(Base::GetID(), sparse, constant);
   }

   /// Check if type origins match                                            
   /// Disregards all cv-qualifiers, pointers, array extents, etc.            
   ///   @param other - the type to compare against                           
   ///   @return true if types match                                          
   template<unsigned S1, unsigned S2>
   bool MetaDataStructured_XY<S1, S2>::Is(const MetaDataStructured_XY& other) const noexcept {
      return GetDefinition()->mOrigin == other.GetDefinition()->mOrigin;
   }

   /// Check if two meta definitions match exactly                            
   ///   @param other - the type to compare against                           
   ///   @return true if types match                                          
   template<unsigned S1, unsigned S2>
   constexpr bool MetaDataStructured_XY<S1, S2>::IsExact(const MetaDataStructured_XY& other) const noexcept {
      return all == other.all and Base::operator == (other);
   }

   template<unsigned S1, unsigned S2>
   constexpr bool MetaDataStructured_XY<S1, S2>::operator==(const MetaDataStructured_XY& other) const noexcept {
      return IsExact(other);
   }
   
   /// Check if two meta definitions match origin and sparseness, but ignores 
   /// `const` and `volatile` qualifiers. The qualifiers aren't ignored only  
   /// on the current level of indirection, but on the entire way to origin   
   ///   @param other - the type to compare against                           
   ///   @return true if types match                                          
   template<unsigned S1, unsigned S2>
   constexpr bool MetaDataStructured_XY<S1, S2>::IsSimilar(const MetaDataStructured_XY& other) const noexcept {
      return Base::operator == (other);
   }

   /// Get the size of the type                                               
   template<unsigned S1, unsigned S2>
   constexpr auto MetaDataStructured_XY<S1, S2>::GetSize() const noexcept -> size_t {
      if constexpr (S2 > 1)
         return Structured<S2>::size ? Structured<S2>::size : GetDefinition()->mSize;
      else
         return GetDefinition()->mSize;
   }

   /// Get the minimal allocation page                                        
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetMinAllocation() const noexcept -> size_t {
      return GetDefinition()->mAllocationPage;
   }

   /// Get the alignment of the type                                          
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetAlignment() const noexcept -> size_t {
      return GetDefinition()->mAlign;
   }

   /// Get the name of the type, the result of NameOf                         
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetName() const noexcept -> Token {
      return GetDefinition()->mNameOf;
   }
   
   /// Get the info of the type, the result of InfoOf                         
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetInfo() const noexcept -> Token {
      return GetDefinition()->mInfoOf;
   }

   /// Get the name of the type as it appearch in C++                         
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetCppName() const noexcept -> Token {
      return GetDefinition()->mCppNameOf;
   }

   /// Get the type hash                                                      
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetHash() const noexcept -> Hash {
      return GetDefinition()->mHash;
   }

   /// Get the associated file extensions, separated with commas              
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetFiles() const noexcept -> Token {
      return GetDefinition()->mFilesOf;
   }

   /// Get the associated suffix                                              
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetSuffix() const noexcept -> Token {
      return GetDefinition()->mSuffixOf;
   }

   /// Get the type boundaries                                                
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetBoundaries() const noexcept -> Definition::BoundarySet const& {
      return GetDefinition()->mBoundaries;
   }

   /// Get the major version                                                  
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetVersionMajor()  const noexcept -> unsigned {
      return GetDefinition()->mVersionMajor;
   }

   /// Get the minor version                                                  
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetVersionMinor()  const noexcept -> unsigned {
      return GetDefinition()->mVersionMinor;
   }

   /// Get the reflected allocation page                                      
   template<unsigned ID_SIZE, unsigned PT_SIZE>
   auto MetaDataStructured_XY<ID_SIZE, PT_SIZE>::GetAllocationPage() const noexcept -> size_t {
      return GetDefinition()->mAllocationPage;      
   }

   #if LANGULUS_FEATURE(MANAGED_MEMORY)
      /// Get the reflected pool tactic                                       
      template<unsigned S1, unsigned S2>
      auto MetaDataStructured_XY<S1, S2>::GetPoolTactic() const noexcept -> PoolTactic {
         return GetDefinition()->mPoolTactic;
      }

      /// Get the poolchain                                                   
      template<unsigned S1, unsigned S2>
      auto MetaDataStructured_XY<S1, S2>::GetPoolchain() const noexcept -> Fractalloc::Pool* {
         return GetDefinition()->mPoolChain;
      }
      
      /// Allows the memory manager to set a new pool chain                   
      template<unsigned S1, unsigned S2>
      void MetaDataStructured_XY<S1, S2>::SetPoolchain(Fractalloc::Pool* pool) const noexcept {
         GetDefinition()->mPoolChain = pool;
      }
   #endif

   /// Check if type is CT::Dense                                             
   template<unsigned S1, unsigned S2>
   constexpr bool MetaDataStructured_XY<S1, S2>::IsDense() const noexcept {
      return not sparse;
   }

   /// Check if type is CT::Sparse                                            
   template<unsigned S1, unsigned S2>
   constexpr bool MetaDataStructured_XY<S1, S2>::IsSparse() const noexcept {
      return sparse;
   }

   /// Check if the type is CT::Constant                                      
   template<unsigned S1, unsigned S2>
   constexpr bool MetaDataStructured_XY<S1, S2>::IsConstant() const noexcept {
      return constant;
   }

   /// Check if the type is CT::Mutable                                       
   template<unsigned S1, unsigned S2>
   constexpr bool MetaDataStructured_XY<S1, S2>::IsMutable() const noexcept {
      return not constant;
   }

   /// Check if type is CT::Deep                                              
   template<unsigned S1, unsigned S2>
   constexpr bool MetaDataStructured_XY<S1, S2>::IsDeep() const noexcept {
      return deep;
   }

   /// Check if type is CT::POD                                               
   template<unsigned S1, unsigned S2>
   constexpr bool MetaDataStructured_XY<S1, S2>::IsPOD() const noexcept {
      return pod;
   }

   /// Check if type is CT::Nullable                                          
   template<unsigned S1, unsigned S2>
   constexpr bool MetaDataStructured_XY<S1, S2>::IsNullable() const noexcept {
      return nullable;
   }

   /// Check if type is CT::Abstract                                          
   template<unsigned S1, unsigned S2>
   constexpr bool MetaDataStructured_XY<S1, S2>::IsAbstract() const noexcept {
      return GetDefinition()->mAbstract;
   }

   /// Check if type has an explicit GetHash() method                         
   template<unsigned S1, unsigned S2>
   constexpr bool MetaDataStructured_XY<S1, S2>::HasGetHashMethod() const noexcept {
      return GetDefinition()->mHasGetHashMethod;
   }
   
   /// Get the reflected destructor                                           
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetDestructor()
   const noexcept -> DefinitionData::FUnary {
      return GetDefinition()->mCurrentBoundary.mDestructor;
   }

   /// Get the reflected referencer                                           
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetReferencer()
   const noexcept -> DefinitionData::FReference {
      return GetDefinition()->mCurrentBoundary.mReferencer;
   }

   /// Get the reflected resolver                                             
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetResolver()
   const noexcept -> DefinitionData::FResolve {
      return GetDefinition()->mCurrentBoundary.mResolver;
   }

   /// Get the reflected default constructor                                  
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetDefaultConstructor() const noexcept -> DefinitionData::FUnary {
      return GetDefinition()->mCurrentBoundary.mDefaultConstructor;
   }
   
   /// Get the reflected describe-constructo                                  
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetDescribeConstructor() const noexcept -> DefinitionData::FDescribe {
      return GetDefinition()->mCurrentBoundary.mDescribeConstructor;
   }   

   /// Get the reflected refer-constructor                                    
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetReferConstructor()
   const noexcept -> DefinitionData::FBinary {
      return GetDefinition()->mCurrentBoundary.mReferConstructor;
   }

   /// Get the reflected refer-assigner                                       
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetReferAssigner()
   const noexcept -> DefinitionData::FBinary {
      return GetDefinition()->mCurrentBoundary.mReferAssigner;
   }

   /// Get the reflected move-constructor                                     
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetMoveConstructor()
   const noexcept -> DefinitionData::FBinary {
      return GetDefinition()->mCurrentBoundary.mMoveConstructor;
   }

   /// Get the reflected move-assigner                                        
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetMoveAssigner()
   const noexcept -> DefinitionData::FBinary {
      return GetDefinition()->mCurrentBoundary.mMoveAssigner;
   }

   /// Get the reflected abandon-constructor                                  
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetAbandonConstructor()
   const noexcept -> DefinitionData::FBinary {
      return GetDefinition()->mCurrentBoundary.mAbandonConstructor;
   }

   /// Get the reflected abandon-assigner                                     
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetAbandonAssigner()
   const noexcept -> DefinitionData::FBinary {
      return GetDefinition()->mCurrentBoundary.mAbandonAssigner;
   }

   /// Get the reflected disown-constructor                                   
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetDisownConstructor()
   const noexcept -> DefinitionData::FBinary {
      return GetDefinition()->mCurrentBoundary.mDisownConstructor;
   }

   /// Get the reflected disown-assigner                                      
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetDisownAssigner()
   const noexcept -> DefinitionData::FBinary {
      return GetDefinition()->mCurrentBoundary.mDisownAssigner;
   }

   /// Get the reflected clone-constructor                                    
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetCloneConstructor()
   const noexcept -> DefinitionData::FBinary {
      return GetDefinition()->mCurrentBoundary.mCloneConstructor;
   }

   /// Get the reflected clone-assigner                                       
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetCloneAssigner()
   const noexcept -> DefinitionData::FBinary {
      return GetDefinition()->mCurrentBoundary.mCloneAssigner;
   }

   /// Get the reflected copy-constructor                                     
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetCopyConstructor()
   const noexcept -> DefinitionData::FBinary {
      return GetDefinition()->mCurrentBoundary.mCopyConstructor;
   }

   /// Get the reflected copy-assigner                                        
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetCopyAssigner()
   const noexcept -> DefinitionData::FBinary {
      return GetDefinition()->mCurrentBoundary.mCopyAssigner;
   }

   /// Get the reflected comparer                                             
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetComparer()
   const noexcept -> DefinitionData::FCompare {
      return GetDefinition()->mCurrentBoundary.mComparer;
   }

   /// Get the reflected hasher                                               
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetHasher()
   const noexcept -> DefinitionData::FHash {
      return GetDefinition()->mCurrentBoundary.mHasher;
   }

   /// Get the reflected dispatcher                                           
   template<unsigned ID_SIZE, unsigned PT_SIZE>
   auto MetaDataStructured_XY<ID_SIZE, PT_SIZE>::GetDispatcher()
   const noexcept -> DefinitionData::FDispatch {
      return GetDefinition()->mCurrentBoundary.mDispatcher;  
   }

   template<unsigned ID_SIZE, unsigned PT_SIZE>
   auto MetaDataStructured_XY<ID_SIZE, PT_SIZE>::GetDeptr()
   const -> MetaDataStructured_XY {
      return GetDefinition()->mDeptr;
   }
   
   template<unsigned ID_SIZE, unsigned PT_SIZE>
   auto MetaDataStructured_XY<ID_SIZE, PT_SIZE>::GetOrigin()
   const -> MetaDataStructured_XY {
      return GetDefinition()->mOrigin;
   }
   
   template<unsigned ID_SIZE, unsigned PT_SIZE>
   auto MetaDataStructured_XY<ID_SIZE, PT_SIZE>::GetDecvqAll()
   const -> MetaDataStructured_XY {
      return GetDefinition()->mDecvqAll;
   }
   
   template<unsigned ID_SIZE, unsigned PT_SIZE>
   auto MetaDataStructured_XY<ID_SIZE, PT_SIZE>::GetDecvq()
   const -> MetaDataStructured_XY {
      return GetDefinition()->mDecvqOnce;
   }
   
   template<unsigned ID_SIZE, unsigned PT_SIZE>
   auto MetaDataStructured_XY<ID_SIZE, PT_SIZE>::AddPtr()
   const -> MetaDataStructured_XY {
      return GetDefinition()->mAddPtr;
   }
   
   template<unsigned ID_SIZE, unsigned PT_SIZE>
   auto MetaDataStructured_XY<ID_SIZE, PT_SIZE>::AddConst()
   const -> MetaDataStructured_XY {
      return GetDefinition()->mAddConst;
   }
   
   template<unsigned ID_SIZE, unsigned PT_SIZE>
   auto MetaDataStructured_XY<ID_SIZE, PT_SIZE>::GetConcrete()
   const -> MetaDataStructured_XY {
      return GetDefinition()->mCurrentBoundary.mConcrete();
   }
   
   template<unsigned ID_SIZE, unsigned PT_SIZE>
   auto MetaDataStructured_XY<ID_SIZE, PT_SIZE>::GetProducer()
   const -> MetaDataStructured_XY {
      return GetDefinition()->mCurrentBoundary.mProducer();
   }

   template<unsigned ID_SIZE, unsigned PT_SIZE>
   auto MetaDataStructured_XY<ID_SIZE, PT_SIZE>::GetBases()
   const noexcept -> DefinitionData::BaseList const& {
      return GetDefinition()->mCurrentBoundary.mBases;
   }
   
   template<unsigned ID_SIZE, unsigned PT_SIZE>
   auto MetaDataStructured_XY<ID_SIZE, PT_SIZE>::GetAbilities()
   const noexcept -> DefinitionData::AbilityList const& {
      return GetDefinition()->mCurrentBoundary.mAbilities;
   }
   
   template<unsigned ID_SIZE, unsigned PT_SIZE>
   auto MetaDataStructured_XY<ID_SIZE, PT_SIZE>::GetMembers()
   const noexcept -> DefinitionData::MemberList const& {
      return GetDefinition()->mCurrentBoundary.mMembers;
   }
   
   template<unsigned ID_SIZE, unsigned PT_SIZE>
   auto MetaDataStructured_XY<ID_SIZE, PT_SIZE>::GetNamedValues()
   const noexcept -> DefinitionData::ValuesList const& {
      return GetDefinition()->mNamedValues;
   }
   
   template<unsigned ID_SIZE, unsigned PT_SIZE>
   auto MetaDataStructured_XY<ID_SIZE, PT_SIZE>::GetMorphismsTo()
   const noexcept -> DefinitionData::MorphismList const& {
      return GetDefinition()->mCurrentBoundary.mMorphismsTo;
   }
   
   template<unsigned ID_SIZE, unsigned PT_SIZE>
   auto MetaDataStructured_XY<ID_SIZE, PT_SIZE>::GetMorphismsFrom()
   const noexcept -> DefinitionData::MorphismList const& {
      return GetDefinition()->mCurrentBoundary.mMorphismsFrom;
   }
   
} // namespace Langulus::RTTI::Inner
