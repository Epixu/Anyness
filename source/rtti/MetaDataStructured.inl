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
   constexpr MetaDataStructured_16_16::MetaDataStructured_16_16(nullptr_t) noexcept
      : Base {0} {}

   /// ID from definition                                                     
   constexpr MetaDataStructured_16_16::MetaDataStructured_16_16(DefinitionData const* d) noexcept
      : Base {d ? d->mID : 0} {}

   /// Reset data ID                                                          
   constexpr auto MetaDataStructured_16_16::operator = (nullptr_t)
   noexcept -> MetaDataStructured_16_16& {
      Base::operator = (0);
      return *this;
   }

   /// Reassign data ID                                                       
   constexpr auto MetaDataStructured_16_16::operator = (DefinitionData const* d)
   noexcept -> MetaDataStructured_16_16& {
      Base::operator = (d ? d->mID : 0);
      return *this;
   }

   /// Check if type origins match                                            
   /// Disregards all cv-qualifiers, pointers, array extents, etc.            
   ///   @param other - the type to compare against                           
   ///   @return true if types match                                          
   inline bool MetaDataStructured_16_16::Is(const MetaDataStructured_16_16& other) const noexcept {
      return Instance.GetMetaData(*this)->mOrigin
          == Instance.GetMetaData(other)->mOrigin;
   }

   /// Check if two meta definitions match exactly                            
   ///   @param other - the type to compare against                           
   ///   @return true if types match                                          
   constexpr bool MetaDataStructured_16_16::IsExact(const MetaDataStructured_16_16& other) const noexcept {
      return all == other.all and Base::operator == (other);
   }

   constexpr bool MetaDataStructured_16_16::operator==(const MetaDataStructured_16_16& other) const noexcept {
      return IsExact(other);
   }
   
   /// Check if two meta definitions match origin and sparseness, but ignores 
   /// `const` and `volatile` qualifiers. The qualifiers aren't ignored only  
   /// on the current level of indirection, but on the entire way to origin   
   ///   @param other - the type to compare against                           
   ///   @return true if types match                                          
   constexpr bool MetaDataStructured_16_16::IsSimilar(const MetaDataStructured_16_16& other) const noexcept {
      return Base::operator == (other);
   }

   /// Get the size of the type                                               
   constexpr auto MetaDataStructured_16_16::GetSize() const noexcept -> size_t {
      return size ? size : Instance.GetMetaData(*this)->mSize;
   }

   /// Get the minimal allocation page                                        
   inline auto MetaDataStructured_16_16::GetMinAllocation() const noexcept -> size_t {
      return Instance.GetMetaData(*this)->mAllocationPage;
   }

   /// Get the alignment of the type                                          
   inline auto MetaDataStructured_16_16::GetAlignment() const noexcept -> size_t {
      return Instance.GetMetaData(*this)->mAlign;
   }

   /// Get the name of the type, the result of NameOf                         
   inline auto MetaDataStructured_16_16::GetName() const noexcept -> Token {
      return Instance.GetMetaData(*this)->mToken;
   }

   /// Get the name of the type as it appearch in C++                         
   inline auto MetaDataStructured_16_16::GetCppName() const noexcept -> Token {
      return Instance.GetMetaData(*this)->mCppName;
   }

   /// Get the type hash                                                      
   inline auto MetaDataStructured_16_16::GetHash() const noexcept -> Hash {
      return Instance.GetMetaData(*this)->mHash;
   }
   
   /// Get the type boundary                                                  
   inline auto MetaDataStructured_16_16::GetBoundary() const noexcept -> Token {
      return Instance.GetMetaData(*this)->mBoundary;
   }

   /// Get the reflected pool tactic                                          
   inline auto MetaDataStructured_16_16::GetPoolTactic() const noexcept -> PoolTactic {
      return Instance.GetMetaData(*this)->mPoolTactic;
   }

   /// Get the poolchain                                                      
   inline auto MetaDataStructured_16_16::GetPoolchain() const noexcept -> Fractalloc::Pool* {
      return Instance.GetMetaData(*this)->mPoolChain;
   }

   /// Check if type is CT::Dense                                             
   constexpr bool MetaDataStructured_16_16::IsDense() const noexcept {
      return not sparse;
   }

   /// Check if type is CT::Sparse                                            
   constexpr bool MetaDataStructured_16_16::IsSparse() const noexcept {
      return sparse;
   }

   /// Check if the type is CT::Constant                                      
   constexpr bool MetaDataStructured_16_16::IsConstant() const noexcept {
      return constant;
   }

   /// Check if the type is CT::Mutable                                       
   constexpr bool MetaDataStructured_16_16::IsMutable() const noexcept {
      return not constant;
   }

   /// Check if type is CT::Deep                                              
   constexpr bool MetaDataStructured_16_16::IsDeep() const noexcept {
      return deep;
   }

   /// Check if type is CT::POD                                               
   constexpr bool MetaDataStructured_16_16::IsPOD() const noexcept {
      return pod;
   }

   /// Get the reflected destructor                                           
   inline auto MetaDataStructured_16_16::GetDestructor()
   const noexcept -> DefinitionData::FDestroy {
      return Instance.GetMetaData(*this)->mDestructor;
   }

   /// Get the reflected referencer                                           
   inline auto MetaDataStructured_16_16::GetReferencer()
   const noexcept -> DefinitionData::FReference {
      return Instance.GetMetaData(*this)->mReferencer;
   }

   /// Get the reflected resolver                                             
   inline auto MetaDataStructured_16_16::GetResolver()
   const noexcept -> DefinitionData::FResolve {
      return Instance.GetMetaData(*this)->mResolver;
   }

   /// Get the reflected refer-constructor                                    
   inline auto MetaDataStructured_16_16::GetReferConstructor()
   const noexcept -> DefinitionData::FCopyConstruct {
      return Instance.GetMetaData(*this)->mReferConstructor;
   }

   /// Get the reflected refer-assigner                                       
   inline auto MetaDataStructured_16_16::GetReferAssigner()
   const noexcept -> DefinitionData::FCopyAssign {
      return Instance.GetMetaData(*this)->mReferAssigner;
   }

   /// Get the reflected move-constructor                                     
   inline auto MetaDataStructured_16_16::GetMoveConstructor()
   const noexcept -> DefinitionData::FMoveConstruct {
      return Instance.GetMetaData(*this)->mMoveConstructor;
   }

   /// Get the reflected move-assigner                                        
   inline auto MetaDataStructured_16_16::GetMoveAssigner()
   const noexcept -> DefinitionData::FMoveAssign {
      return Instance.GetMetaData(*this)->mMoveAssigner;
   }

   /// Get the reflected abandon-constructor                                  
   inline auto MetaDataStructured_16_16::GetAbandonConstructor()
   const noexcept -> DefinitionData::FMoveConstruct {
      return Instance.GetMetaData(*this)->mAbandonConstructor;
   }

   /// Get the reflected abandon-assigner                                     
   inline auto MetaDataStructured_16_16::GetAbandonAssigner()
   const noexcept -> DefinitionData::FMoveAssign {
      return Instance.GetMetaData(*this)->mAbandonAssigner;
   }

   /// Get the reflected disown-constructor                                   
   inline auto MetaDataStructured_16_16::GetDisownConstructor()
   const noexcept -> DefinitionData::FCopyConstruct {
      return Instance.GetMetaData(*this)->mDisownConstructor;
   }

   /// Get the reflected disown-assigner                                      
   inline auto MetaDataStructured_16_16::GetDisownAssigner()
   const noexcept -> DefinitionData::FCopyAssign {
      return Instance.GetMetaData(*this)->mDisownAssigner;
   }

   /// Get the reflected clone-constructor                                    
   inline auto MetaDataStructured_16_16::GetCloneConstructor()
   const noexcept -> DefinitionData::FCopyConstruct {
      return Instance.GetMetaData(*this)->mCloneConstructor;
   }

   /// Get the reflected clone-assigner                                       
   inline auto MetaDataStructured_16_16::GetCloneAssigner()
   const noexcept -> DefinitionData::FCopyAssign {
      return Instance.GetMetaData(*this)->mCloneAssigner;
   }

   /// Get the reflected copy-constructor                                     
   inline auto MetaDataStructured_16_16::GetCopyConstructor()
   const noexcept -> DefinitionData::FCopyConstruct {
      return Instance.GetMetaData(*this)->mCopyConstructor;
   }

   /// Get the reflected copy-assigner                                        
   inline auto MetaDataStructured_16_16::GetCopyAssigner()
   const noexcept -> DefinitionData::FCopyAssign {
      return Instance.GetMetaData(*this)->mCopyAssigner;
   }

   /// Get the reflected comparer                                             
   inline auto MetaDataStructured_16_16::GetComparer()
   const noexcept -> DefinitionData::FCompare {
      return Instance.GetMetaData(*this)->mComparer;
   }

   /// Get the reflected hasher                                               
   inline auto MetaDataStructured_16_16::GetHasher()
   const noexcept -> DefinitionData::FHash {
      return Instance.GetMetaData(*this)->mHasher;
   }

   /// Check if type has an explicit GetHash() method                         
   inline bool MetaDataStructured_16_16::HasGetHashMethod() const noexcept {
      return Instance.GetMetaData(*this)->mHasGetHashMethod;
   }

   /// Allows the memory manager to set a new pool chain                      
   inline void MetaDataStructured_16_16::SetPoolchain(Fractalloc::Pool* pool) const noexcept {
      Instance.GetMetaData(*this)->mPoolChain = pool;
   }

} // namespace Langulus::RTTI::Inner
