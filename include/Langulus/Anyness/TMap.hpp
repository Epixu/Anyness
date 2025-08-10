///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../../../source/Container.hpp"
#include "../../../source/components/Heap-Movable.hpp"
#include "../../../source/components/Ownership-Stack.hpp"
#include "../../../source/components/DeepOwnership-Heap.hpp"
#include "../../../source/components/Hash-Heap.hpp"
#include "../../../source/components/Indexed-Hash.hpp"
#include "../../../source/components/Insertion.hpp"
#include "../../../source/components/InsertionOperators.hpp"
#include "../../../source/components/Emplacement.hpp"
#include "../../../source/components/Removal.hpp"
#include "../../../source/components/Assignment.hpp"
#include "../../../source/components/Typed-Stack.hpp"
#include "../../../source/components/Count-Stack.hpp"
#include "../../../source/components/Reserve-Stack.hpp"
#include "../../../source/components/Comparison.hpp"
#include "../../../source/components/Iteration-ForEach.hpp"
#include "../../../source/components/Iteration-Range.hpp"
#include "../../../source/components/State-Stack.hpp"
#include "../../../source/states/Sorted.hpp"
#include "../../../source/states/Compressed.hpp"
#include "../../../source/states/Encrypted.hpp"
#include "../../../source/states/Tracked.hpp"
#include "THandle.hpp"
#include "TPair.hpp"
#include "TMany.hpp"
#include <Langulus/Retype.hpp>


namespace Langulus::CT
{

   /// Concept for recognizing arguments, with which a statically typed       
   /// map can be constructed                                                 
   template<class K, class V, class...A>
   concept DeepMapConstructible = UnfoldConstructible<Anyness::TPair<K, V>, A...>
        or (sizeof...(A) == 1 and Map<FirstOf<A...>> and (
               IntentOf<FirstOf<A...>>::Shallow
            or IntentConstructibleAlt<Retype<IntentOf<FirstOf<A...>>, Anyness::TPair<K, V>>>)
        );

   /// Concept for recognizing argument, with which a statically typed        
   /// map can be assigned                                                    
   template<class K, class V, class A>
   concept DeepMapAssignable = UnfoldConstructible<Anyness::TPair<K, V>, A>
        or (Map<A> and (
               IntentOf<A>::Shallow
            or IntentAssignableAlt<Retype<IntentOf<A>, Anyness::TPair<K, V>>>)
        );

} // namespace Langulus::CT

namespace Langulus::Anyness
{
   namespace Inner
   {
      
      ///                                                                     
      template<CT::NotVoid K, CT::NotVoid V>
      using TMapCommon = Container<
         Com::HeapMovable<0>,          // Heap for keys                 
         Com::HeapMovable<1>,          // Heap for values               
         Com::OwnershipStack<0>,       // Keys allocation is referenced 
         Com::OwnershipStack<1>,       // Vals allocation is referenced 
         Com::HashHeap<0>,             // Keys can be hashed            
         Com::HashHeap<1>,             // Values can be hashed          
         Com::DeepOwnershipHeap<0>,    // Sparse keys are referenced    
         Com::DeepOwnershipHeap<1>,    // Sparse vals are referenced    
         Com::IndexedHash<0>,          // Indexed by hashing keys       
         Com::Insertion<0>,            // Allows insertion of keys      
         Com::Insertion<1>,            // Allows insertion of vals      
         Com::InsertionOperators<0>,   // << and >> insertion of keys   
         Com::InsertionOperators<1>,   // << and >> insertion of vals   
         Com::Emplacement<0>,          // Allows emplacement of keys    
         Com::Emplacement<1>,          // Allows emplacement of vals    
         Com::Removal<0>,              // Allows removal of keys        
         Com::Removal<1>,              // Allows removal of vals        
         Com::Assignment<0>,           // Allows assignment             
         Com::Assignment<1>,           // Allows assignment             
         Com::TypedStack<DMeta, K, 0>, // Key type                      
         Com::TypedStack<DMeta, V, 1>, // Value type                    
         Com::CountStack<>,            // Variable count                
         Com::ReserveStack<>,          // Variable capacity             
         Com::Comparison,              // Allows for comparison         
         Com::IterationRange<0>,       // Iterate keys                  
         Com::IterationRange<1>        // Iterate values                
      >;

      ///                                                                     
      template<CT::NotVoid K, CT::NotVoid V>
      using TMapBase = typename TMapCommon<K, V>::template Include<
         Com::StateStack<              // Variable state                
            DefineState::Sorted<>,     // Maybe unsorted                
            DefineState::Compressed<>, // Adds 'compressed' state       
            DefineState::Encrypted<>,  // Adds 'encrypted' state        
            DefineState::Tracked<>     // Adds 'tracked' state          
         >
      >;

      ///                                                                     
      template<CT::NotVoid K, CT::NotVoid V>
      using TMapUnsortedBase = typename TMapCommon<K, V>::template Include<
         Com::StateStack<              // Variable state                
            DefineState::Sorted<State::Disabled>,  // Always unsorted   
            DefineState::Compressed<>, // Adds 'compressed' state       
            DefineState::Encrypted<>,  // Adds 'encrypted' state        
            DefineState::Tracked<>     // Adds 'tracked' state          
         >
      >;
      
      ///                                                                     
      template<CT::NotVoid K, CT::NotVoid V>
      using TMapSortedBase = typename TMapCommon<K, V>::template Include<
         Com::StateStack<              // Variable state                
            DefineState::Sorted<State::Enabled>,   // Always sorted     
            DefineState::Compressed<>, // Adds 'compressed' state       
            DefineState::Encrypted<>,  // Adds 'encrypted' state        
            DefineState::Tracked<>     // Adds 'tracked' state          
         >
      >;

   } // namespace Langulus::Anyness::Inner

   struct Map;


   ///                                                                        
   /// Statically typed map of unspecified state                              
   ///                                                                        
   template<CT::NotVoid K, CT::NotVoid V>
   struct TMap : Inner::TMapBase<K, V> {
      using CTTI_ReflectAs = Map;
      using CTTI_Map       = Yes<>;
      using Base           = Inner::TMapBase<K, V>;

      using KeyDenseMut  = K&;
      using KeyDense     = K const&;
      using KeySparseMut = THandle<K&>;
      using KeySparse    = THandle<K const&>;
      using Key          = Tif<CT::Sparse<K>, KeySparse,    KeyDense>;
      using KeyMut       = Tif<CT::Sparse<K>, KeySparseMut, KeyDenseMut>;

      using ValDenseMut  = V&;
      using ValDense     = V const&;
      using ValSparseMut = THandle<V&>;
      using ValSparse    = THandle<V const&>;
      using Val          = Tif<CT::Sparse<V>, ValSparse,    ValDense>;
      using ValMut       = Tif<CT::Sparse<V>, ValSparseMut, ValDenseMut>;

      using IteratorMut  = typename IterateTogether<const TMany<K>,       TMany<V>>::Iterator;
      using Iterator     = typename IterateTogether<const TMany<K>, const TMany<V>>::Iterator;
      using CountType    = typename Com::CountStack<>::CountType;

      using PairType     = TPair<K const&, V const&>;
      using PairTypeMut  = TPair<K const&, V&>;

      ///                                                                     
      ///   Construction                                                      
      constexpr TMap() noexcept = default;
      constexpr TMap(TMap const&) noexcept = default;
      constexpr TMap(TMap&&) noexcept = default;

      template<class T1, class...TN> requires CT::DeepMapConstructible<K, V, T1, TN...>
      constexpr TMap(T1&&, TN&&...);

      /*template<template<class> class I, CT::Map M> requires CT::Intent<I<M>>
      constexpr TMap(I<M>&& other) noexcept
         : Base {other.template Forward<typename M::Base>()} {}*/

      /*template<CT::Map M1, CT::Map...MN>
      TMap(M1&&, MN&&...) requires CT::PairConstructible<K, V, typename M1::PairType, typename MN::PairType...>;
      
      template<CT::Pair P1, CT::Pair...PN>
      TMap(P1&&, PN&&...) requires CT::PairConstructible<K, V, P1, PN...>;*/
      
      ///                                                                     
      ///   Assignment                                                        
      TMap& operator = (TMap const&) noexcept = default;
      TMap& operator = (TMap&&) noexcept = default;

      template<class A1> requires CT::RangeAssignable<TMap, A1>
      TMap& operator = (A1&&);

      ///                                                                     
      ///   Capsulation                                                       
      Hash GetHash() const;

      ///                                                                     
      ///   Removal                                                           
      auto RemoveKey  (const CT::NoIntent auto&) -> CountType;
      auto RemoveVal  (const CT::NoIntent auto&) -> CountType;
      auto RemovePair (const CT::Pair auto&)     -> CountType;
      void RemoveIt   (IteratorMut&);
   };
   

   ///                                                                        
   /// Unsorted statically typed map                                          
   ///                                                                        
   template<CT::NotVoid K, CT::NotVoid V>
   struct TMapUnsorted : Inner::TMapUnsortedBase<K, V> {
      using CTTI_ReflectAs = Map;
      using CTTI_Map       = Yes<>;
      using Base           = Inner::TMapUnsortedBase<K, V>;

      using KeyDenseMut  = K&;
      using KeyDense     = K const&;
      using KeySparseMut = THandle<K&>;
      using KeySparse    = THandle<K const&>;
      using Key          = Tif<CT::Sparse<K>, KeySparse,    KeyDense>;
      using KeyMut       = Tif<CT::Sparse<K>, KeySparseMut, KeyDenseMut>;

      using ValDenseMut  = V&;
      using ValDense     = V const&;
      using ValSparseMut = THandle<V&>;
      using ValSparse    = THandle<V const&>;
      using Val          = Tif<CT::Sparse<V>, ValSparse,    ValDense>;
      using ValMut       = Tif<CT::Sparse<V>, ValSparseMut, ValDenseMut>;

      using IteratorMut  = typename IterateTogether<const TMany<K>,       TMany<V>>::Iterator;
      using Iterator     = typename IterateTogether<const TMany<K>, const TMany<V>>::Iterator;
      using CountType    = typename Component::CountStack<>::CountType;

      using PairType     = TPair<K const&, V const&>;
      using PairTypeMut  = TPair<K const&, V&>;

      ///                                                                     
      ///   Construction                                                      
      constexpr TMapUnsorted() noexcept = default;
      constexpr TMapUnsorted(TMapUnsorted const&) noexcept = default;
      constexpr TMapUnsorted(TMapUnsorted&&) noexcept = default;

      template<class T1, class...TN> requires CT::DeepMapConstructible<K, V, T1, TN...>
      constexpr TMapUnsorted(T1&&, TN&&...);


      /*template<template<class> class I, CT::Map M> requires CT::Intent<I<M>>
      constexpr TMapUnsorted(I<M>&& other) noexcept
         : Base {FWD(other)} {}*/

      //template<template<class> class I, CT::Map M> requires CT::Intent<I<M>>
      //constexpr TMapUnsorted(I<M>&&) noexcept;

      /*template<CT::Map M1, CT::Map...MN>
      TMapUnsorted(M1&&, MN&&...) requires CT::PairConstructible<K, V, typename M1::PairType, typename MN::PairType...>;
      
      template<CT::Pair P1, CT::Pair...PN>
      TMapUnsorted(P1&&, PN&&...) requires CT::PairConstructible<K, V, P1, PN...>;*/

      ///                                                                     
      ///   Assignment                                                        
      TMapUnsorted& operator = (TMapUnsorted const&) noexcept = default;
      TMapUnsorted& operator = (TMapUnsorted&&) noexcept = default;

      template<class A1> requires CT::RangeAssignable<TMapUnsorted, A1>
      TMapUnsorted& operator = (A1&&);

      ///                                                                     
      ///   Capsulation                                                       
      Hash GetHash() const;

      ///                                                                     
      ///   Removal                                                           
      auto RemoveKey  (const CT::NoIntent auto&) -> CountType;
      auto RemoveVal  (const CT::NoIntent auto&) -> CountType;
      auto RemovePair (const CT::Pair auto&) -> CountType;
      void RemoveIt   (IteratorMut&);
   };
   

   ///                                                                        
   /// Sorted statically typed map                                            
   ///                                                                        
   template<CT::NotVoid K, CT::NotVoid V>
   struct TMapSorted : Inner::TMapSortedBase<K, V> {
      using CTTI_ReflectAs = Map;
      using CTTI_Map       = Yes<>;
      using Base           = Inner::TMapSortedBase<K, V>;

      using KeyDenseMut  = K&;
      using KeyDense     = K const&;
      using KeySparseMut = THandle<K&>;
      using KeySparse    = THandle<K const&>;
      using Key          = Tif<CT::Sparse<K>, KeySparse,    KeyDense>;
      using KeyMut       = Tif<CT::Sparse<K>, KeySparseMut, KeyDenseMut>;

      using ValDenseMut  = V&;
      using ValDense     = V const&;
      using ValSparseMut = THandle<V&>;
      using ValSparse    = THandle<V const&>;
      using Val          = Tif<CT::Sparse<V>, ValSparse,    ValDense>;
      using ValMut       = Tif<CT::Sparse<V>, ValSparseMut, ValDenseMut>;

      using IteratorMut  = typename IterateTogether<const TMany<K>,       TMany<V>>::Iterator;
      using Iterator     = typename IterateTogether<const TMany<K>, const TMany<V>>::Iterator;
      using CountType    = typename Component::CountStack<>::CountType;

      using PairType     = TPair<K const&, V const&>;
      using PairTypeMut  = TPair<K const&, V&>;

      ///                                                                     
      ///   Construction                                                      
      constexpr TMapSorted() noexcept = default;
      constexpr TMapSorted(TMapSorted const&) noexcept = default;
      constexpr TMapSorted(TMapSorted&&) noexcept = default;

      template<class T1, class...TN> requires CT::DeepMapConstructible<K, V, T1, TN...>
      constexpr TMapSorted(T1&&, TN&&...);

      /*template<template<class> class I, CT::Map M> requires CT::Intent<I<M>>
      constexpr TMapSorted(I<M>&& other) noexcept
         : Base {other.template Forward<typename M::Base>()} {}*/

      /*template<CT::Map M1, CT::Map...MN>
      TMapSorted(M1&&, MN&&...) requires CT::PairConstructible<K, V, typename M1::PairType, typename MN::PairType...>;

      template<CT::Pair P1, CT::Pair...PN>
      TMapSorted(P1&&, PN&&...) requires CT::PairConstructible<K, V, P1, PN...>;*/

      ///                                                                     
      ///   Assignment                                                        
      TMapSorted& operator = (TMapSorted const&) noexcept = default;
      TMapSorted& operator = (TMapSorted&&) noexcept = default;

      template<class A1> requires CT::RangeAssignable<TMapSorted, A1>
      TMapSorted& operator = (A1&&);

      ///                                                                     
      ///   Capsulation                                                       
      Hash GetHash() const;

      ///                                                                     
      ///   Removal                                                           
      auto RemoveKey  (const CT::NoIntent auto&) -> CountType;
      auto RemoveVal  (const CT::NoIntent auto&) -> CountType;
      auto RemovePair (const CT::Pair auto&) -> CountType;
      void RemoveIt   (IteratorMut&);
   };

} // namespace Langulus::Anyness
