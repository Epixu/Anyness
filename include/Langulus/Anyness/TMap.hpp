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
#include "../../../source/components/DeepOwnership.hpp"
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


namespace Langulus::Anyness
{

   struct Map;


   ///                                                                        
   /// Statically typed map of unspecified state                              
   ///                                                                        
   template<CT::NotVoid K, CT::NotVoid V>
   struct TMap : Container<
      Component::HeapMovable<0>,       // Heap for keys                 
      Component::HeapMovable<1>,       // Heap for values               
      Component::OwnershipStack<0>,    // Keys allocation is referenced 
      Component::OwnershipStack<1>,    // Vals allocation is referenced 
      Component::HashHeap<0>,          // Keys can be hashed            
      Component::HashHeap<1>,          // Values can be hashed          
      Component::DeepOwnership<0>,     // Sparse keys are referenced    
      Component::DeepOwnership<1>,     // Sparse vals are referenced    
      Component::IndexedHash<0>,       // Indexed by hashing keys       
      Component::Insertion<0>,         // Allows insertion of keys      
      Component::Insertion<1>,         // Allows insertion of vals      
      Component::InsertionOperators<0>,// << and >> insertion of keys   
      Component::InsertionOperators<1>,// << and >> insertion of vals   
      Component::Emplacement<0>,       // Allows emplacement of keys    
      Component::Emplacement<1>,       // Allows emplacement of vals    
      Component::Removal<0>,           // Allows removal of keys        
      Component::Removal<1>,           // Allows removal of vals        
      Component::Assignment<0>,        // Allows assignment             
      Component::Assignment<1>,        // Allows assignment             
      Component::TypedStack<DMeta, K, 0>,       // Key type             
      Component::TypedStack<DMeta, V, 1>,       // Value type           
      Component::CountStack<>,         // Variable count                
      Component::ReserveStack<>,       // Variable capacity             
      Component::Comparison,           // Allows for comparison         
      Component::StateStack<           // Variable state                
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
      using CountType    = typename Component::CountStack<>::CountType;

      ///                                                                     
      ///   Construction                                                      
      constexpr TMap() noexcept = default;
      constexpr TMap(const TMap&) noexcept = default;
      constexpr TMap(TMap&&) noexcept = default;

      template<class A1, class...AN>
      TMap(A1&&, AN&&...) requires CT::RangeInsertable<TMap, A1, AN...>;
      
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
      Component::HeapMovable<0>,       // Heap for keys                 
      Component::HeapMovable<1>,       // Heap for values               
      Component::OwnershipStack<0>,    // Keys allocation is referenced 
      Component::OwnershipStack<1>,    // Vals allocation is referenced 
      Component::HashHeap<0>,          // Keys can be hashed            
      Component::HashHeap<1>,          // Values can be hashed          
      Component::DeepOwnership<0>,     // Sparse keys are referenced    
      Component::DeepOwnership<1>,     // Sparse vals are referenced    
      Component::IndexedHash<0>,       // Indexed by hashing keys       
      Component::Insertion<0>,         // Allows insertion of keys      
      Component::Insertion<1>,         // Allows insertion of vals      
      Component::InsertionOperators<0>,// << and >> insertion of keys   
      Component::InsertionOperators<1>,// << and >> insertion of vals   
      Component::Emplacement<0>,       // Allows emplacement of keys    
      Component::Emplacement<1>,       // Allows emplacement of vals    
      Component::Removal<0>,           // Allows removal of keys        
      Component::Removal<1>,           // Allows removal of vals        
      Component::Assignment<0>,        // Allows assignment             
      Component::Assignment<1>,        // Allows assignment             
      Component::TypedStack<DMeta, K, 0>,       // Key type             
      Component::TypedStack<DMeta, V, 1>,       // Value type           
      Component::CountStack<>,         // Variable count                
      Component::ReserveStack<>,       // Variable capacity             
      Component::Comparison,           // Allows for comparison         
      Component::StateStack<           // Variable state                
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

      ///                                                                     
      ///   Construction                                                      
      constexpr TMapUnsorted() noexcept = default;
      constexpr TMapUnsorted(const TMapUnsorted&) noexcept = default;
      constexpr TMapUnsorted(TMapUnsorted&&) noexcept = default;

      template<class A1, class...AN>
      TMapUnsorted(A1&&, AN&&...) requires CT::RangeInsertable<TMapUnsorted, A1, AN...>;

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
      Component::HeapMovable<0>,       // Heap for keys                 
      Component::HeapMovable<1>,       // Heap for values               
      Component::OwnershipStack<0>,    // Keys allocation is referenced 
      Component::OwnershipStack<1>,    // Vals allocation is referenced 
      Component::HashHeap<0>,          // Keys can be hashed            
      Component::HashHeap<1>,          // Values can be hashed          
      Component::DeepOwnership<0>,     // Sparse keys are referenced    
      Component::DeepOwnership<1>,     // Sparse vals are referenced    
      Component::IndexedHash<0>,       // Indexed by hashing keys       
      Component::Insertion<0>,         // Allows insertion of keys      
      Component::Insertion<1>,         // Allows insertion of vals      
      Component::InsertionOperators<0>,// << and >> insertion of keys   
      Component::InsertionOperators<1>,// << and >> insertion of vals   
      Component::Emplacement<0>,       // Allows emplacement of keys    
      Component::Emplacement<1>,       // Allows emplacement of vals    
      Component::Removal<0>,           // Allows removal of keys        
      Component::Removal<1>,           // Allows removal of vals        
      Component::Assignment<0>,        // Allows assignment             
      Component::Assignment<1>,        // Allows assignment             
      Component::TypedStack<DMeta, K, 0>,       // Key type             
      Component::TypedStack<DMeta, V, 1>,       // Value type           
      Component::CountStack<>,         // Variable count                
      Component::ReserveStack<>,       // Variable capacity             
      Component::Comparison,           // Allows for comparison         
      Component::StateStack<           // Variable state                
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

      ///                                                                     
      ///   Construction                                                      
      constexpr TMapSorted() noexcept = default;
      constexpr TMapSorted(const TMapSorted&) noexcept = default;
      constexpr TMapSorted(TMapSorted&&) noexcept = default;

      template<class A1, class...AN>
      TMapSorted(A1&&, AN&&...) requires CT::RangeInsertable<TMapSorted, A1, AN...>;

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
