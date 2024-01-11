///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// Distributed under GNU General Public License v3+                          
/// See LICENSE file, or https://www.gnu.org/licenses                         
///                                                                           
#pragma once
#include "Any.hpp"


namespace Langulus::CT
{

   /// Concept for recognizing arguments, with which a statically typed       
   /// container can be constructed                                           
   template<class T, class...A>
   concept DeepMakable = Inner::UnfoldMakableFrom<T, A...>
        or (sizeof...(A) == 1
           and Block<Desem<FirstOf<A...>>>
           and Inner::SemanticMakableAlt<
               typename SemanticOf<FirstOf<A...>>::template As<T>>
        );

   /// Concept for recognizing argument, with which a statically typed        
   /// container can be assigned                                              
   template<class T, class A>
   concept DeepAssignable = Inner::UnfoldMakableFrom<T, A>
        or (Block<Desem<A>> and Inner::SemanticAssignableAlt<
           typename SemanticOf<A>::template As<T>>);

} // namespace Langulus::CT

namespace Langulus::Anyness
{
   
   ///                                                                        
   ///   TAny                                                                 
   ///                                                                        
   ///   Unlike Any, this one is statically optimized to perform faster, due  
   /// to not being type-erased. In that sense, this container is equivalent  
   /// to std::vector.                                                        
   ///   Don't forget that all Any containers are binary-compatible with each 
   /// other, so after you've asserted, that an Any is of a specific type,    
   /// (by checking result of doing something like pack.IsExact<my type>())   
   /// you can then directly reinterpret_cast that Any to an equivalent       
   /// TAny<of the type you checked for>, essentially converting your         
   /// type-erased container to a statically-optimized equivalent. Anyness    
   /// provides a strong guarantee that this operation is completely safe.    
   ///                                                                        
   template<CT::Data T>
   class TAny : public Any {
      LANGULUS(DEEP) true;
      LANGULUS(POD) false;
      LANGULUS(TYPED) T;
      LANGULUS_BASES(Any);

      static_assert(CT::Inner::Insertable<T>,
         "Contained type is not insertable");
      static_assert(CT::Inner::Allocatable<T>,
         "Contained type is not allocatable");

      ///                                                                     
      ///   Construction                                                      
      ///                                                                     
      constexpr TAny();
      TAny(const TAny&);
      TAny(TAny&&) noexcept;

      template<class T1, class...TAIL>
      requires CT::DeepMakable<T, T1, TAIL...>
      TAny(T1&&, TAIL&&...);

      ~TAny();

      NOD() static TAny From(auto&&, Count = 1);

      template<CT::Data... LIST_T>
      NOD() static TAny Wrap(LIST_T&&...);

      ///                                                                     
      ///   Assignment                                                        
      ///                                                                     
      TAny& operator = (const TAny&);
      TAny& operator = (TAny&&);

      template<class T1> requires CT::DeepAssignable<T, T1>
      TAny& operator = (T1&&);

   public:
      ///                                                                     
      ///   Capsulation                                                       
      ///                                                                     
      NOD() DMeta    GetType() const noexcept;
      NOD() T const* GetRaw() const noexcept;
      NOD() T*       GetRaw()       noexcept;
      NOD() T const* GetRawEnd() const noexcept;
      constexpr void ResetState() noexcept;

      NOD() constexpr bool IsTyped() const noexcept;
      NOD() constexpr bool IsUntyped() const noexcept;
      NOD() constexpr bool IsTypeConstrained() const noexcept;
      NOD() constexpr bool IsDeep() const noexcept;
      NOD() constexpr bool IsSparse() const noexcept;
      NOD() constexpr bool IsDense() const noexcept;
      NOD() constexpr bool IsPOD() const noexcept;
      NOD() constexpr bool IsResolvable() const noexcept;

      NOD() constexpr Size GetStride() const noexcept;
      NOD() constexpr Size GetBytesize() const noexcept;
      NOD() constexpr Count GetCountDeep() const noexcept;
      NOD() constexpr Count GetCountElementsDeep() const noexcept;

      NOD() constexpr bool IsMissingDeep() const;
      NOD() constexpr bool IsConcatable(const CT::Block auto&) const noexcept;
      NOD() constexpr bool IsInsertable(DMeta) const noexcept;
      NOD() constexpr bool IsInsertable() const noexcept;

   //private: IF_LANGULUS_TESTING(public:)
   //   using Any::GetRawSparse;

   public:
      ///                                                                     
      ///   Indexing                                                          
      ///                                                                     
      NOD() T const& Last() const;
      NOD() T&       Last();

      template<CT::Data = T>
      NOD() decltype(auto) Get(Offset) const noexcept;
      template<CT::Data = T>
      NOD() decltype(auto) Get(Offset) noexcept;

      NOD() const T& operator [] (CT::Index auto) const;
      NOD()       T& operator [] (CT::Index auto);

      NOD() decltype(auto) GetHandle(Offset)       IF_UNSAFE(noexcept);
      NOD() decltype(auto) GetHandle(Offset) const IF_UNSAFE(noexcept);

      NOD() Block*       GetBlockDeep(Offset) noexcept;
      NOD() Block const* GetBlockDeep(Offset) const noexcept;

      NOD() Block GetElementDeep(Offset) noexcept;
      NOD() Block GetElementDeep(Offset) const noexcept;

      NOD() TAny Crop(Offset, Count) const;
      NOD() TAny Crop(Offset, Count);

      ///                                                                     
      ///   Iteration                                                         
      ///                                                                     
      using Iterator = Block::Iterator<TAny>;
      using ConstIterator = Block::Iterator<const TAny>;

      NOD() Iterator begin() noexcept;
      NOD() Iterator last() noexcept;
      NOD() ConstIterator begin() const noexcept;
      NOD() ConstIterator last() const noexcept;

      template<bool REVERSE = false>
      Count ForEachElement(auto&&) const;

      template<bool REVERSE = false>
      Count ForEach(auto&&...) const;

      template<bool REVERSE = false, bool SKIP = true>
      Count ForEachDeep(auto&&...) const;

      Count ForEachElementRev(auto&&...) const;

      Count ForEachRev(auto&&...) const;

      template<bool SKIP = true>
      Count ForEachDeepRev(auto&&...) const;

      ///                                                                     
      ///   RTTI                                                              
      ///                                                                     
      NOD() bool CastsToMeta(DMeta) const;
      NOD() bool CastsToMeta(DMeta, Count) const;

      template<CT::Data>
      NOD() bool CastsTo() const;
      template<CT::Data>
      NOD() bool CastsTo(Count) const;

      template<CT::Data, CT::Data...>
      NOD() constexpr bool Is() const noexcept;
      NOD() bool Is(DMeta) const noexcept;

      template<CT::Data, CT::Data...>
      NOD() constexpr bool IsSimilar() const noexcept;
      NOD() bool IsSimilar(DMeta) const noexcept;

      template<CT::Data, CT::Data...>
      NOD() constexpr bool IsExact() const noexcept;
      NOD() bool IsExact(DMeta) const noexcept;

      ///                                                                     
      ///   Comparison                                                        
      ///                                                                     
      template<CT::NotSemantic T1>
      requires (CT::Block<T1> or CT::Inner::Comparable<T, T1>)
      bool operator == (const T1&) const;

      template<bool RESOLVE = true>
      NOD() bool Compare(const CT::Block auto&) const;
      NOD() Hash GetHash() const requires CT::Hashable<T>;

      template<bool REVERSE = false, CT::NotSemantic T1>
      requires CT::Inner::Comparable<T, T1>
      NOD() Index Find(const T1&, Offset = 0) const noexcept;

      template<CT::NotSemantic T1>
      requires CT::Inner::Comparable<T, T1>
      NOD() Iterator FindIt(const T1&);

      template<CT::NotSemantic T1>
      requires CT::Inner::Comparable<T, T1>
      NOD() ConstIterator FindIt(const T1&) const;

      template<bool REVERSE = false>
      NOD() Index FindBlock(const CT::Block auto&, Offset = 0) const noexcept;

      NOD() bool CompareLoose(const CT::Block auto&) const noexcept;
      NOD() Count Matches(const CT::Block auto&) const noexcept;
      NOD() Count MatchesLoose(const CT::Block auto&) const noexcept;

      template<bool ASCEND = false>
      requires CT::Inner::Sortable<T>
      void Sort();

      void Swap(CT::Index auto, CT::Index auto);

      template<bool REVERSE = false>
      Count GatherFrom(const Block&);
      template<bool REVERSE = false>
      Count GatherFrom(const Block&, DataState);

      ///                                                                     
      ///   Memory management                                                 
      ///                                                                     
      void Reserve(Count);

      ///                                                                     
      ///   Insertion                                                         
      ///                                                                     
      template<bool MOVE_ASIDE = true, class T1, class...TAIL>
      requires CT::Inner::UnfoldMakableFrom<T, T1, TAIL...>
      Count Insert(CT::Index auto, T1&&, TAIL&&...);

      template<class FORCE = Any, bool MOVE_ASIDE = true, class T1>
      requires CT::Block<Desem<T1>>
      Count InsertBlock(CT::Index auto, T1&&);

      template<bool MOVE_ASIDE = true, class...A>
      requires ::std::constructible_from<T, A...>
      Conditional<CT::Sparse<T>, T, T&> Emplace(CT::Index auto, A&&...);

      template<bool MOVE_ASIDE = true, class T1, class...TAIL>
      requires CT::Inner::UnfoldMakableFrom<T, T1, TAIL...>
      Count Merge(CT::Index auto, T1&&, TAIL&&...);

      template<class FORCE = Any, bool MOVE_ASIDE = true, class T1>
      requires CT::Block<Desem<T1>>
      Count MergeBlock(CT::Index auto, T1&&);
   
      template<class...A>
      requires ::std::constructible_from<T, A...>
      Count New(Count, A&&...);

      Count New(Count = 1) requires CT::Inner::Defaultable<T>;

      template<CT::Deep T1, bool TRANSFER_OR = true>
      requires CT::CanBeDeepened<T1, TAny>
      T1& Deepen();

      void Null(Count);

      NOD() TAny<T> Extend(Count);

      template<class T1>
      requires CT::Inner::UnfoldMakableFrom<T, T1>
      TAny& operator << (T1&&);

      template<class T1>
      requires CT::Inner::UnfoldMakableFrom<T, T1>
      TAny& operator >> (T1&&);

      template<class T1>
      requires CT::Inner::UnfoldMakableFrom<T, T1>
      TAny& operator <<= (T1&&);

      template<class T1>
      requires CT::Inner::UnfoldMakableFrom<T, T1>
      TAny& operator >>= (T1&&);

   public:
      ///                                                                     
      ///   Removal                                                           
      ///                                                                     
      template<bool REVERSE = false>
      Count Remove(const CT::Data auto&);
      Count RemoveIndex(CT::Index auto, Count = 1);
      Count RemoveIndexDeep(CT::Index auto);
      Iterator RemoveIt(const Iterator&, Count = 1);

      void Trim(Count);
      void Optimize();
      void Clear();
      void Reset();

      ///                                                                     
      ///   Concatenation                                                     
      ///                                                                     
      template<class T1> requires CT::DeepMakable<T, T1>
      NOD() TAny operator + (T1&&) const;

      template<class T1> requires CT::DeepMakable<T, T1>
      TAny& operator += (T1&&);

      ///                                                                     
      ///   Flow                                                              
      ///                                                                     
      // Intentionally undefined, because it requires Langulus::Flow    
      void Run(Flow::Verb&) const;
      // Intentionally undefined, because it requires Langulus::Flow    
      void Run(Flow::Verb&);

   private:
      /// Services graveyard - disallowed interface for typed containers      
      using Any::FromMeta;
      using Any::FromBlock;
      using Any::FromState;
      using Any::From;
      using Any::WrapAs;
      using Any::SetType;
      using Any::MakeTypeConstrained;
      using Any::SmartPush;
   };

} // namespace Langulus::Anyness
