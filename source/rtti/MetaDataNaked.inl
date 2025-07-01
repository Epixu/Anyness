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

   /// Check if type origins match                                            
   /// Disregards all cv-qualifiers, pointers, array extents, etc.            
   ///   @param other - the type to compare against                           
   ///   @return true if types match                                          
   inline bool MetaDataNaked::Is(const MetaDataNaked& other) const noexcept {
      return mDefinition->mOrigin and other
         and mDefinition->mOrigin == other.mDefinition->mOrigin;
   }

   /// Check if two meta definitions match origin and sparseness, but ignores 
   /// `const` and `volatile` qualifiers. The qualifiers aren't ignored only  
   /// on the current level of indirection, but on the entire way to origin   
   ///   @param other - the type to compare against                           
   ///   @return true if types match                                          
   inline bool MetaDataNaked::IsSimilar(const MetaDataNaked& other) const noexcept {
      return other and mDefinition->mDecvqAll == other.mDefinition->mDecvqAll;
   }

   /// Get the minimal allocation page                                        
   inline auto MetaDataNaked::GetMinAllocation() const noexcept -> size_t {
      return mDefinition->mAllocationPage;
   }

   /// Get the size of the type                                               
   inline auto MetaDataNaked::GetSize() const noexcept -> size_t {
      return mDefinition->mSize;
   }

   /// Get the alignment of the type                                          
   inline auto MetaDataNaked::GetAlignment() const noexcept -> size_t {
      return mDefinition->mAlign;
   }

   /// Get the name of the type, the result of NameOf                         
   inline auto MetaDataNaked::GetName() const noexcept -> Token {
      return mDefinition->mNameOf;
   }
   
   /// Get the name of the type as it appearch in C++                         
   inline auto MetaDataNaked::GetCppName() const noexcept -> Token {
      return mDefinition->mCppNameOf;
   }

   /// Get the type hash                                                      
   inline auto MetaDataNaked::GetHash() const noexcept -> Hash {
      return mDefinition->mHash;
   }
   
   /// Get the type boundary                                                  
   inline bool MetaDataNaked::CheckBoundary(const Token& boundary) const noexcept {
      return mDefinition->mBoundaries.contains(boundary);
   }

   /// Check if type is CT::Dense                                             
   inline bool MetaDataNaked::IsDense() const noexcept {
      return not mDefinition->mDeptr;
   }

   /// Check if type is CT::Sparse                                            
   inline bool MetaDataNaked::IsSparse() const noexcept {
      return mDefinition->mDeptr;
   }

   /// Check if the type is CT::Constant                                      
   inline bool MetaDataNaked::IsConstant() const noexcept {
      return mDefinition->mConst;
   }

   /// Check if the type is CT::Mutable                                       
   inline bool MetaDataNaked::IsMutable() const noexcept {
      return not mDefinition->mConst;
   }

   /// Check if type is CT::Deep                                              
   inline bool MetaDataNaked::IsDeep() const noexcept {
      return mDefinition->mDeep;
   }

   /// Check if type is CT::POD                                               
   inline bool MetaDataNaked::IsPOD() const noexcept {
      return mDefinition->mPOD;
   }

   /// Get the reflected destructor                                           
   inline auto MetaDataNaked::GetDestructor()
   const noexcept -> DefinitionData::FDestroy {
      return mDefinition->mDestructor;
   }

   /// Get the reflected referencer                                           
   inline auto MetaDataNaked::GetReferencer()
   const noexcept -> DefinitionData::FReference {
      return mDefinition->mReferencer;
   }

   /// Get the reflected resolver                                             
   inline auto MetaDataNaked::GetResolver()
   const noexcept -> DefinitionData::FResolve {
      return mDefinition->mResolver;
   }

   /// Get the reflected refer-constructor                                    
   inline auto MetaDataNaked::GetReferConstructor()
   const noexcept -> DefinitionData::FCopyConstruct {
      return mDefinition->mReferConstructor;
   }

   /// Get the reflected refer-assigner                                       
   inline auto MetaDataNaked::GetReferAssigner()
   const noexcept -> DefinitionData::FCopyAssign {
      return mDefinition->mReferAssigner;
   }

   /// Get the reflected move-constructor                                     
   inline auto MetaDataNaked::GetMoveConstructor()
   const noexcept -> DefinitionData::FMoveConstruct {
      return mDefinition->mMoveConstructor;
   }

   /// Get the reflected move-assigner                                        
   inline auto MetaDataNaked::GetMoveAssigner()
   const noexcept -> DefinitionData::FMoveAssign {
      return mDefinition->mMoveAssigner;
   }

   /// Get the reflected abandon-constructor                                  
   inline auto MetaDataNaked::GetAbandonConstructor()
   const noexcept -> DefinitionData::FMoveConstruct {
      return mDefinition->mAbandonConstructor;
   }

   /// Get the reflected abandon-assigner                                     
   inline auto MetaDataNaked::GetAbandonAssigner()
   const noexcept -> DefinitionData::FMoveAssign {
      return mDefinition->mAbandonAssigner;
   }

   /// Get the reflected disown-constructor                                   
   inline auto MetaDataNaked::GetDisownConstructor()
   const noexcept -> DefinitionData::FCopyConstruct {
      return mDefinition->mDisownConstructor;
   }

   /// Get the reflected disown-assigner                                      
   inline auto MetaDataNaked::GetDisownAssigner()
   const noexcept -> DefinitionData::FCopyAssign {
      return mDefinition->mDisownAssigner;
   }

   /// Get the reflected clone-constructor                                    
   inline auto MetaDataNaked::GetCloneConstructor()
   const noexcept -> DefinitionData::FCopyConstruct {
      return mDefinition->mCloneConstructor;
   }

   /// Get the reflected clone-assigner                                       
   inline auto MetaDataNaked::GetCloneAssigner()
   const noexcept -> DefinitionData::FCopyAssign {
      return mDefinition->mCloneAssigner;
   }

   /// Get the reflected copy-constructor                                     
   inline auto MetaDataNaked::GetCopyConstructor()
   const noexcept -> DefinitionData::FCopyConstruct {
      return mDefinition->mCopyConstructor;
   }

   /// Get the reflected copy-assigner                                        
   inline auto MetaDataNaked::GetCopyAssigner()
   const noexcept -> DefinitionData::FCopyAssign {
      return mDefinition->mCopyAssigner;
   }

   /// Get the reflected comparer                                             
   inline auto MetaDataNaked::GetComparer()
   const noexcept -> DefinitionData::FCompare {
      return mDefinition->mComparer;
   }

   /// Get the reflected hasher                                               
   inline auto MetaDataNaked::GetHasher()
   const noexcept -> DefinitionData::FHash {
      return mDefinition->mHasher;
   }

   /// Check if type has an explicit GetHash() method                         
   inline bool MetaDataNaked::HasGetHashMethod() const noexcept {
      return mDefinition->mHasGetHashMethod;
   }

} // namespace Langulus::RTTI::Inner
