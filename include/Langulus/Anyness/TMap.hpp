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


namespace Langulus::Anyness
{

   struct Map;


   ///                                                                        
   /// Statically typed map of unspecified state                              
   ///                                                                        
   template<CT::NotVoid K, CT::NotVoid V>
   struct TMap : Container<
      Com::HeapMovable<0>,             // Heap for keys                 
      Com::HeapMovable<1>,             // Heap for values               
      Com::OwnershipStack<0>,          // Keys allocation is referenced 
      Com::OwnershipStack<1>,          // Vals allocation is referenced 
      Com::HashHeap<0>,                // Keys can be hashed            
      Com::HashHeap<1>,                // Values can be hashed          
      Com::DeepOwnershipHeap<0>,       // Sparse keys are referenced    
      Com::DeepOwnershipHeap<1>,       // Sparse vals are referenced    
      Com::IndexedHash<0>,             // Indexed by hashing keys       
      Com::Insertion<0>,               // Allows insertion of keys      
      Com::Insertion<1>,               // Allows insertion of vals      
      Com::InsertionOperators<0>,      // << and >> insertion of keys   
      Com::InsertionOperators<1>,      // << and >> insertion of vals   
      Com::Emplacement<0>,             // Allows emplacement of keys    
      Com::Emplacement<1>,             // Allows emplacement of vals    
      Com::Removal<0>,                 // Allows removal of keys        
      Com::Removal<1>,                 // Allows removal of vals        
      Com::Assignment<0>,              // Allows assignment             
      Com::Assignment<1>,              // Allows assignment             
      Com::TypedStack<DMeta, K, 0>,    // Key type                      
      Com::TypedStack<DMeta, V, 1>,    // Value type                    
      Com::CountStack<>,               // Variable count                
      Com::ReserveStack<>,             // Variable capacity             
      Com::Comparison,                 // Allows for comparison         
      Com::StateStack<                 // Variable state                
         DefineState::Sorted<>,        // Maybe unsorted                
         DefineState::Compressed<>,    // Adds 'compressed' state       
         DefineState::Encrypted<>,     // Adds 'encrypted' state        
         DefineState::Tracked<>        // Adds 'tracked' state          
      >
   > {
      using CTTI_ReflectAs = Map;
      using CTTI_Map       = Yes;

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

      using It           = TIteratorMap<TMap>;
      using CountType    = typename Com::CountStack<>::CountType;

      using PairType     = TPair<K const&, V const&>;
      using PairTypeMut  = TPair<K const&, V&>;

      ///                                                                     
      ///   Construction                                                      
      constexpr TMap() noexcept = default;
      constexpr TMap(const TMap&) noexcept = default;
      constexpr TMap(TMap&&) noexcept = default;

      template<CT::Map M1, CT::Map...MN>
      TMap(M1&&, MN&&...) requires CT::PairConstructible<K, V, typename M1::PairType, typename MN::PairType...>;
      
      template<CT::Pair P1, CT::Pair...PN>
      TMap(P1&&, PN&&...) requires CT::PairConstructible<K, V, P1, PN...>;
      
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
      auto RemoveIt   (const It&) -> It;
   };
   

   ///                                                                        
   /// Unsorted statically typed map                                          
   ///                                                                        
   template<CT::NotVoid K, CT::NotVoid V>
   struct TMapUnsorted : Container<
      Com::HeapMovable<0>,             // Heap for keys                 
      Com::HeapMovable<1>,             // Heap for values               
      Com::OwnershipStack<0>,          // Keys allocation is referenced 
      Com::OwnershipStack<1>,          // Vals allocation is referenced 
      Com::HashHeap<0>,                // Keys can be hashed            
      Com::HashHeap<1>,                // Values can be hashed          
      Com::DeepOwnershipHeap<0>,       // Sparse keys are referenced    
      Com::DeepOwnershipHeap<1>,       // Sparse vals are referenced    
      Com::IndexedHash<0>,             // Indexed by hashing keys       
      Com::Insertion<0>,               // Allows insertion of keys      
      Com::Insertion<1>,               // Allows insertion of vals      
      Com::InsertionOperators<0>,      // << and >> insertion of keys   
      Com::InsertionOperators<1>,      // << and >> insertion of vals   
      Com::Emplacement<0>,             // Allows emplacement of keys    
      Com::Emplacement<1>,             // Allows emplacement of vals    
      Com::Removal<0>,                 // Allows removal of keys        
      Com::Removal<1>,                 // Allows removal of vals        
      Com::Assignment<0>,              // Allows assignment             
      Com::Assignment<1>,              // Allows assignment             
      Com::TypedStack<DMeta, K, 0>,    // Key type                      
      Com::TypedStack<DMeta, V, 1>,    // Value type                    
      Com::CountStack<>,               // Variable count                
      Com::ReserveStack<>,             // Variable capacity             
      Com::Comparison,                 // Allows for comparison         
      Com::StateStack<                 // Variable state                
         DefineState::Sorted<State::Disabled>,  // Always unsorted      
         DefineState::Compressed<>,    // Adds 'compressed' state       
         DefineState::Encrypted<>,     // Adds 'encrypted' state        
         DefineState::Tracked<>        // Adds 'tracked' state          
      >
   > {
      using CTTI_ReflectAs = Map;
      using CTTI_Map       = Yes;

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

      using It           = TIteratorMap<TMapUnsorted>;
      using CountType    = typename Component::CountStack<>::CountType;

      using PairType     = TPair<K const&, V const&>;
      using PairTypeMut  = TPair<K const&, V&>;

      ///                                                                     
      ///   Construction                                                      
      constexpr TMapUnsorted() noexcept = default;
      constexpr TMapUnsorted(const TMapUnsorted&) noexcept = default;
      constexpr TMapUnsorted(TMapUnsorted&&) noexcept = default;

      template<CT::Map M>
      constexpr TMapUnsorted(Copied<M>&&) noexcept;
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
      auto RemoveIt   (const It&) -> It;
   };
   

   ///                                                                        
   /// Sorted statically typed map                                            
   ///                                                                        
   template<CT::NotVoid K, CT::NotVoid V>
   struct TMapSorted : Container<
      Com::HeapMovable<0>,             // Heap for keys                 
      Com::HeapMovable<1>,             // Heap for values               
      Com::OwnershipStack<0>,          // Keys allocation is referenced 
      Com::OwnershipStack<1>,          // Vals allocation is referenced 
      Com::HashHeap<0>,                // Keys can be hashed            
      Com::HashHeap<1>,                // Values can be hashed          
      Com::DeepOwnershipHeap<0>,       // Sparse keys are referenced    
      Com::DeepOwnershipHeap<1>,       // Sparse vals are referenced    
      Com::IndexedHash<0>,             // Indexed by hashing keys       
      Com::Insertion<0>,               // Allows insertion of keys      
      Com::Insertion<1>,               // Allows insertion of vals      
      Com::InsertionOperators<0>,      // << and >> insertion of keys   
      Com::InsertionOperators<1>,      // << and >> insertion of vals   
      Com::Emplacement<0>,             // Allows emplacement of keys    
      Com::Emplacement<1>,             // Allows emplacement of vals    
      Com::Removal<0>,                 // Allows removal of keys        
      Com::Removal<1>,                 // Allows removal of vals        
      Com::Assignment<0>,              // Allows assignment             
      Com::Assignment<1>,              // Allows assignment             
      Com::TypedStack<DMeta, K, 0>,    // Key type                      
      Com::TypedStack<DMeta, V, 1>,    // Value type                    
      Com::CountStack<>,               // Variable count                
      Com::ReserveStack<>,             // Variable capacity             
      Com::Comparison,                 // Allows for comparison         
      Com::StateStack<                 // Variable state                
         DefineState::Sorted<State::Enabled>,   // Always sorted        
         DefineState::Compressed<>,    // Adds 'compressed' state       
         DefineState::Encrypted<>,     // Adds 'encrypted' state        
         DefineState::Tracked<>        // Adds 'tracked' state          
      >
   > {
      using CTTI_ReflectAs = Map;
      using CTTI_Map       = Yes;

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

      using It           = TIteratorMap<TMapSorted>;
      using CountType    = typename Component::CountStack<>::CountType;

      using PairType     = TPair<K const&, V const&>;
      using PairTypeMut  = TPair<K const&, V&>;

      ///                                                                     
      ///   Construction                                                      
      constexpr TMapSorted() noexcept = default;
      constexpr TMapSorted(const TMapSorted&) noexcept = default;
      constexpr TMapSorted(TMapSorted&&) noexcept = default;

      template<CT::Map M1, CT::Map...MN>
      TMapSorted(M1&&, MN&&...) requires CT::PairConstructible<K, V, typename M1::PairType, typename MN::PairType...>;

      template<CT::Pair P1, CT::Pair...PN>
      TMapSorted(P1&&, PN&&...) requires CT::PairConstructible<K, V, P1, PN...>;

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
      auto RemoveIt   (const It&) -> It;
   };

} // namespace Langulus::Anyness
