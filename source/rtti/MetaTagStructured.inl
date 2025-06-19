///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "MetaTag.hpp"


namespace Langulus::RTTI::Inner
{

   constexpr MetaDataStructured_16_16::MetaDataStructured_16_16(::std::nullptr_t) noexcept
      : Base {0} {}

   constexpr MetaDataStructured_16_16::MetaDataStructured_16_16(DefinitionData const* definition) noexcept
      : Base {definition ? definition->mID : 0} {}

   constexpr MetaDataStructured_16_16& MetaDataStructured_16_16::operator = (::std::nullptr_t) noexcept {
      Base::operator = (0);
      return *this;
   }

   constexpr MetaDataStructured_16_16& MetaDataStructured_16_16::operator = (DefinitionData const* definition) noexcept {
      Base::operator = (definition ? definition->mID : 0);
      return *this;
   }

   /// Check if type origins match                                            
   /// Disregards all cv-qualifiers, pointers, array extents, etc.            
   ///   @param other - the type to compare against                           
   ///   @return true if types match                                          
   bool MetaDataStructured_16_16::Is(const MetaDataStructured_16_16& other) const noexcept {
      return Instance.GetMetaData(*this)->mOrigin
          == Instance.GetMetaData(other)->mOrigin;
   }

   /// Check if two meta definitions match exactly                            
   ///   @param other - the type to compare against                           
   ///   @return true if types match                                          
   constexpr bool MetaDataStructured_16_16::IsExact(const MetaDataStructured_16_16& other) const noexcept {
      return all == other.all and Base::operator == (other);
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
   auto MetaDataStructured_16_16::GetMinAllocation() const noexcept -> size_t {
      return Instance.GetMetaData(*this)->mAllocationPage;
   }

   /// Get the alignment of the type                                          
   auto MetaDataStructured_16_16::GetAlignment() const noexcept -> size_t {
      return Instance.GetMetaData(*this)->mAlign;
   }

   /// Get the name of the type, the result of NameOf                         
   auto MetaDataStructured_16_16::GetName() const noexcept -> Token {
      return Instance.GetMetaData(*this)->mToken;
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
   auto MetaDataStructured_16_16::GetDestructor() const noexcept -> DefinitionData::FDestroy {
      return Instance.GetMetaData(*this)->mDestructor;
   }

   /// Get the reflected referencer                                           
   auto MetaDataStructured_16_16::GetReferencer() const noexcept -> DefinitionData::FReference {
      return Instance.GetMetaData(*this)->mReferencer;
   }

   /// Get the reflected resolver                                             
   auto MetaDataStructured_16_16::GetResolver() const noexcept -> DefinitionData::FResolve {
      return Instance.GetMetaData(*this)->mResolver;
   }

   /// Get the reflected refer-constructor                                    
   auto MetaDataStructured_16_16::GetReferConstructor() const noexcept -> DefinitionData::FCopyConstruct {
      return Instance.GetMetaData(*this)->mReferConstructor;
   }

   /// Get the reflected refer-assigner                                       
   auto MetaDataStructured_16_16::GetReferAssigner() const noexcept -> DefinitionData::FCopyAssign {
      return Instance.GetMetaData(*this)->mReferAssigner;
   }

   /// Get the reflected move-constructor                                     
   auto MetaDataStructured_16_16::GetMoveConstructor() const noexcept -> DefinitionData::FMoveConstruct {
      return Instance.GetMetaData(*this)->mMoveConstructor;
   }

   /// Get the reflected move-assigner                                        
   auto MetaDataStructured_16_16::GetMoveAssigner() const noexcept -> DefinitionData::FMoveAssign {
      return Instance.GetMetaData(*this)->mMoveAssigner;
   }

   /// Get the reflected abandon-constructor                                  
   auto MetaDataStructured_16_16::GetAbandonConstructor() const noexcept -> DefinitionData::FMoveConstruct {
      return Instance.GetMetaData(*this)->mAbandonConstructor;
   }

   /// Get the reflected abandon-assigner                                     
   auto MetaDataStructured_16_16::GetAbandonAssigner() const noexcept -> DefinitionData::FMoveAssign {
      return Instance.GetMetaData(*this)->mAbandonAssigner;
   }

   /// Get the reflected disown-constructor                                   
   auto MetaDataStructured_16_16::GetDisownConstructor() const noexcept -> DefinitionData::FCopyConstruct {
      return Instance.GetMetaData(*this)->mDisownConstructor;
   }

   /// Get the reflected disown-assigner                                      
   auto MetaDataStructured_16_16::GetDisownAssigner() const noexcept -> DefinitionData::FCopyAssign {
      return Instance.GetMetaData(*this)->mDisownAssigner;
   }

   /// Get the reflected clone-constructor                                    
   auto MetaDataStructured_16_16::GetCloneConstructor() const noexcept -> DefinitionData::FCopyConstruct {
      return Instance.GetMetaData(*this)->mCloneConstructor;
   }

   /// Get the reflected clone-assigner                                       
   auto MetaDataStructured_16_16::GetCloneAssigner() const noexcept -> DefinitionData::FCopyAssign {
      return Instance.GetMetaData(*this)->mCloneAssigner;
   }

   /// Get the reflected copy-constructor                                     
   auto MetaDataStructured_16_16::GetCopyConstructor() const noexcept -> DefinitionData::FCopyConstruct {
      return Instance.GetMetaData(*this)->mCopyConstructor;
   }

   /// Get the reflected copy-assigner                                        
   auto MetaDataStructured_16_16::GetCopyAssigner() const noexcept -> DefinitionData::FCopyAssign {
      return Instance.GetMetaData(*this)->mCopyAssigner;
   }

   /// Get the reflected comparer                                             
   auto MetaDataStructured_16_16::GetComparer() const noexcept -> DefinitionData::FCompare {
      return Instance.GetMetaData(*this)->mComparer;
   }

   /// Get the reflected hasher                                               
   auto MetaDataStructured_16_16::GetHasher() const noexcept -> DefinitionData::FHash {
      return Instance.GetMetaData(*this)->mHasher;
   }

   /// Check if type has an explicit GetHash() method                         
   bool MetaDataStructured_16_16::HasGetHashMethod() const noexcept {
      return Instance.GetMetaData(*this)->mHasGetHashMethod;
   }

} // namespace Langulus::RTTI::Inner