///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "TMany.hpp"
#include "TMap.hpp"
#include "Construct.hpp"
#include <Langulus/Tag.hpp>


namespace Langulus::Anyness
{

   using DMeta = RTTI::DMeta;
   using TMeta = RTTI::TMeta;
   
   ///                                                                        
   ///   Neat - a normalized data container                                   
   ///                                                                        
   ///   Turns messy containers into neatly and consistently ordered ones,    
   /// that are very fast on compare/search/insert/remove, albeit quite a bit 
   /// larger.                                                                
   ///   Neats are extensively used as descriptors in factories, to check     
   /// whether an element with the same signature already exists.             
   ///   Elements that are marked missing are never considered part of the    
   /// descriptor and are filled by the context (i.e. Tags::Parent(?))        
   ///                                                                        
   class Neat {
      using ConstructList = TMany<Construct>;
      using TailList      = TMany<Messy>;
      using Count         = ::std::size_t;
      using Offset        = ::std::size_t;

      // The hash of the container                                      
      // Kept as first member, in order to quickly access it            
      mutable Hash mHash;

      // Tags are ordered first by their tag type, then by their        
      // order of appearance. Duplicate tag types are allowed           
      // Tag contents may or may not also be normalized                 
      TMapUnsorted<TMeta, TailList> mTags;

      // Subconstructs are sorted first by the construct type, and then 
      // by their order of appearance. Their contents may or may not    
      // also be normalized                                             
      TMapUnsorted<DMeta, ConstructList> mConstructs;

      // Any other block type that doesn't fit in the above is sorted   
      // first by the block type, then by the order of appearance       
      // These sub-blocks' contents may or may not be normalized        
      TMapUnsorted<DMeta, TailList> mAnythingElse;

   public:
      using CTTI_Container = Yes;

      ///                                                                     
      ///   Construction                                                      
      ///                                                                     
      constexpr Neat() = default;
      Neat(const Neat&);
      Neat(Neat&&) noexcept;

      template<template<class> class S> requires CT::Intent<S<Neat>>
      Neat(S<Neat>&&);

      template<class A1, class...AN>
      Neat(A1&&, AN&&...) requires RangeInsertable<Many, A1, AN...>;

      ///                                                                     
      ///   Assignment                                                        
      ///                                                                     
      Neat& operator = (const Neat&) = default;
      Neat& operator = (Neat&&) noexcept = default;

      template<template<class> class S> requires CT::Intent<S<Neat>>
      Neat& operator = (S<Neat>&&);

      ///                                                                     
      ///   Comparison                                                        
      ///                                                                     
      bool operator == (const Neat&) const;

      void Clear();
      void Reset();

      ///                                                                     
      ///   Encapsulation                                                     
      ///                                                                     
      Hash GetHash() const;
      bool IsEmpty() const noexcept;
      bool IsMissingDeep() const;
      bool IsExecutable() const noexcept;

      explicit operator bool() const noexcept;

      template<CT::Tag>
      auto GetTags() -> TailList*;

      template<CT::Tag>
      auto GetTraits()      const -> const TailList*;
      auto GetTraits(TMeta)       ->       TailList*;
      auto GetTraits(TMeta) const -> const TailList*;

      template<CT::NotVoid>
      auto GetData() -> TailList*;

      template<CT::NotVoid>
      auto GetData()      const -> const TailList*;
      auto GetData(DMeta)       ->       TailList*;
      auto GetData(DMeta) const -> const TailList*;
      
      template<CT::NotVoid>
      auto GetConstructs() -> ConstructList*;

      template<CT::NotVoid>
      auto FindType()      const -> DMeta;
      auto FindType(DMeta) const -> DMeta;

      template<CT::NotVoid>
      auto GetConstructs()      const -> const ConstructList*;
      auto GetConstructs(DMeta)       ->       ConstructList*;
      auto GetConstructs(DMeta) const -> const ConstructList*;

      template<CT::Tag>
      void SetDefaultTrait(CT::NotVoid auto&&);

      template<CT::Tag...>
      bool ExtractTrait(CT::NotVoid auto&...) const;
      auto ExtractData(CT::NotVoid auto&) const -> Count;
      auto ExtractDataAs(CT::NotVoid auto&) const -> Count;

      template<CT::Tag>
      auto GetTag(Offset = 0)        const -> const Tag<>*;
      auto GetTag(TMeta, Offset = 0) const -> const Tag<>*;

   protected:
      template<CT::Tag>
      bool ExtractTraitInner(CT::NotVoid auto&...) const;
      template<Offset...IDX>
      bool ExtractTraitInner(const TraitList&, ExpandedSequence<IDX...>, CT::NotVoid auto&...) const;
      template<Offset>
      bool ExtractTraitInnerInner(const TraitList&, CT::NotVoid auto&) const;

   public:
      ///                                                                     
      ///   Iteration                                                         
      ///                                                                     
      template<bool MUTABLE = true>
      Count ForEach(auto&&...);
      Count ForEach(auto&&...) const;

      template<bool MUTABLE = true>
      Count ForEachDeep(auto&&...);
      Count ForEachDeep(auto&&...) const;

      template<bool MUTABLE = true>
      Count ForEachTrait(auto&&);
      Count ForEachTrait(auto&&) const;

      template<bool MUTABLE = true>
      Count ForEachConstruct(auto&&);
      Count ForEachConstruct(auto&&) const;

      template<bool MUTABLE = true>
      Count ForEachTail(auto&&);
      Count ForEachTail(auto&&) const;

   protected:
      template<bool MUTABLE = true>
      Count ForEachInner(auto&&);
      Count ForEachInner(auto&&) const;

   public:
      ///                                                                     
      ///   Insertion                                                         
      ///                                                                     
      template<class T1, class...TN>
      Count Insert(T1&&, TN&&...);
      void  Merge(const Neat&);
      Neat& SetTrait(CT::Tag auto&&, Offset = 0);

      Neat& operator <<  (auto&&);
      Neat& operator <<= (auto&&);

   protected:
      Count UnfoldInsert(auto&&);
      void InsertInner(auto&&);

      void AddTrait(CT::Intent auto&&);
      void AddConstruct(CT::Intent auto&&);
      void AddVerb(CT::Intent auto&&);

   public:
      ///                                                                     
      ///   Removal                                                           
      ///                                                                     
      template<CT::NotVoid, bool EMPTY_TOO = false>
      Count RemoveData();
      template<CT::NotVoid>
      Count RemoveConstructs();
      template<CT::Tag, bool EMPTY_TOO = false>
      Count RemoveTag();

      ///                                                                     
      ///   Conversion                                                        
      ///                                                                     
      Count Serialize(CT::Serial auto&) const;
   };

} // namespace Langulus::Anyness
