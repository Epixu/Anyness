///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "MetaData.hpp"


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
         sparse = d->mDeptr != nullptr;
         constant = d->mConst;
         deep = d->mDeep;
         pod = d->mPOD;
         nullable = d->mNullable;
         referenced = d->mCurrentBoundary.mReferencer != nullptr;
         resolvable = d->mCurrentBoundary.mResolver != nullptr;
         dispatcher = d->mCurrentBoundary.mDispatcherMut != nullptr
                   or d->mCurrentBoundary.mDispatcher != nullptr;
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
         sparse = d->mConst;
         constant = d->mConst;
         deep = d->mDeep;
         pod = d->mPOD;
         nullable = d->mNullable;
         referenced = d->mCurrentBoundary.mReferencer != nullptr;
         resolvable = d->mCurrentBoundary.mResolver != nullptr;
         dispatcher = d->mCurrentBoundary.mDispatcherMut != nullptr
                   or d->mCurrentBoundary.mDispatcher != nullptr;
      }
      return *this;
   }

   /// Check if type origins match                                            
   /// Disregards all cv-qualifiers, pointers, array extents, etc.            
   ///   @param other - the type to compare against                           
   ///   @return true if types match                                          
   template<unsigned S1, unsigned S2>
   bool MetaDataStructured_XY<S1, S2>::Is(const MetaDataStructured_XY& other) const noexcept {
      return Instance.GetMetaDataByID(*this)->mOrigin
          == Instance.GetMetaDataByID(other)->mOrigin;
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
      if constexpr (S2 > 1) {
         return Structured<S2>::size
            ? Structured<S2>::size
            : Instance.GetMetaDataByID(*this)->mSize;
      }
      else return Instance.GetMetaDataByID(*this)->mSize;
   }

   /// Get the minimal allocation page                                        
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetMinAllocation() const noexcept -> size_t {
      return Instance.GetMetaDataByID(*this)->mAllocationPage;
   }

   /// Get the alignment of the type                                          
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetAlignment() const noexcept -> size_t {
      return Instance.GetMetaDataByID(*this)->mAlign;
   }

   /// Get the name of the type, the result of NameOf                         
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetName() const noexcept -> Token {
      return Instance.GetMetaDataByID(*this)->mNameOf;
   }

   /// Get the name of the type as it appearch in C++                         
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetCppName() const noexcept -> Token {
      return Instance.GetMetaDataByID(*this)->mCppNameOf;
   }

   /// Get the type hash                                                      
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetHash() const noexcept -> Hash {
      return Instance.GetMetaDataByID(*this)->mHash;
   }
   
   /// Get the type boundary                                                  
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetBoundaries() const noexcept -> Definition::BoundarySet const& {
      return Instance.GetMetaDataByID(*this)->mBoundaries;
   }

#if LANGULUS_FEATURE(MANAGED_MEMORY)
   /// Get the reflected pool tactic                                          
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetPoolTactic() const noexcept -> PoolTactic {
      return Instance.GetMetaDataByID(*this)->mPoolTactic;
   }

   /// Get the poolchain                                                      
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetPoolchain() const noexcept -> Fractalloc::Pool* {
      return Instance.GetMetaDataByID(*this)->mPoolChain;
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

   /// Get the reflected destructor                                           
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetDestructor()
   const noexcept -> DefinitionData::FUnary {
      return Instance.GetMetaDataByID(*this)->mCurrentBoundary.mDestructor;
   }

   /// Get the reflected referencer                                           
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetReferencer()
   const noexcept -> DefinitionData::FReference {
      return Instance.GetMetaDataByID(*this)->mCurrentBoundary.mReferencer;
   }

   /// Get the reflected resolver                                             
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetResolver()
   const noexcept -> DefinitionData::FResolve {
      return Instance.GetMetaDataByID(*this)->mCurrentBoundary.mResolver;
   }

   /// Get the reflected refer-constructor                                    
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetReferConstructor()
   const noexcept -> DefinitionData::FBinary {
      return Instance.GetMetaDataByID(*this)->mCurrentBoundary.mReferConstructor;
   }

   /// Get the reflected refer-assigner                                       
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetReferAssigner()
   const noexcept -> DefinitionData::FBinary {
      return Instance.GetMetaDataByID(*this)->mCurrentBoundary.mReferAssigner;
   }

   /// Get the reflected move-constructor                                     
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetMoveConstructor()
   const noexcept -> DefinitionData::FBinary {
      return Instance.GetMetaDataByID(*this)->mCurrentBoundary.mMoveConstructor;
   }

   /// Get the reflected move-assigner                                        
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetMoveAssigner()
   const noexcept -> DefinitionData::FBinary {
      return Instance.GetMetaDataByID(*this)->mCurrentBoundary.mMoveAssigner;
   }

   /// Get the reflected abandon-constructor                                  
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetAbandonConstructor()
   const noexcept -> DefinitionData::FBinary {
      return Instance.GetMetaDataByID(*this)->mCurrentBoundary.mAbandonConstructor;
   }

   /// Get the reflected abandon-assigner                                     
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetAbandonAssigner()
   const noexcept -> DefinitionData::FBinary {
      return Instance.GetMetaDataByID(*this)->mCurrentBoundary.mAbandonAssigner;
   }

   /// Get the reflected disown-constructor                                   
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetDisownConstructor()
   const noexcept -> DefinitionData::FBinary {
      return Instance.GetMetaDataByID(*this)->mCurrentBoundary.mDisownConstructor;
   }

   /// Get the reflected disown-assigner                                      
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetDisownAssigner()
   const noexcept -> DefinitionData::FBinary {
      return Instance.GetMetaDataByID(*this)->mCurrentBoundary.mDisownAssigner;
   }

   /// Get the reflected clone-constructor                                    
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetCloneConstructor()
   const noexcept -> DefinitionData::FBinary {
      return Instance.GetMetaDataByID(*this)->mCurrentBoundary.mCloneConstructor;
   }

   /// Get the reflected clone-assigner                                       
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetCloneAssigner()
   const noexcept -> DefinitionData::FBinary {
      return Instance.GetMetaDataByID(*this)->mCurrentBoundary.mCloneAssigner;
   }

   /// Get the reflected copy-constructor                                     
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetCopyConstructor()
   const noexcept -> DefinitionData::FBinary {
      return Instance.GetMetaDataByID(*this)->mCurrentBoundary.mCopyConstructor;
   }

   /// Get the reflected copy-assigner                                        
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetCopyAssigner()
   const noexcept -> DefinitionData::FBinary {
      return Instance.GetMetaDataByID(*this)->mCurrentBoundary.mCopyAssigner;
   }

   /// Get the reflected comparer                                             
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetComparer()
   const noexcept -> DefinitionData::FCompare {
      return Instance.GetMetaDataByID(*this)->mCurrentBoundary.mComparer;
   }

   /// Get the reflected hasher                                               
   template<unsigned S1, unsigned S2>
   auto MetaDataStructured_XY<S1, S2>::GetHasher()
   const noexcept -> DefinitionData::FHash {
      return Instance.GetMetaDataByID(*this)->mCurrentBoundary.mHasher;
   }

   /// Check if type has an explicit GetHash() method                         
   template<unsigned S1, unsigned S2>
   bool MetaDataStructured_XY<S1, S2>::HasGetHashMethod() const noexcept {
      return Instance.GetMetaDataByID(*this)->mHasGetHashMethod;
   }

   /// Allows the memory manager to set a new pool chain                      
   template<unsigned S1, unsigned S2>
   void MetaDataStructured_XY<S1, S2>::SetPoolchain(Fractalloc::Pool* pool) const noexcept {
      Instance.GetMetaDataByID(*this)->mPoolChain = pool;
   }

} // namespace Langulus::RTTI::Inner
