///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Map.hpp"
#include "Handle.hpp"
#include "../../../source/components/Typed-Static.hpp"


namespace Langulus::Anyness
{

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
      Component::IndexedHash<>,        // Indexed directly              
      Component::Insertion<>,          // Allows insertion              
      Component::InsertionOperators<>, // << and >> insertion           
      Component::Emplacement,          // Allows emplacement            
      Component::Removal,              // Allows removal                
      Component::Assignment,           // Allows assignment             
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
      using KeySparseMut = Handle<K>;
      using KeySparse    = K;
      using Key          = Tif<CT::Sparse<K>, KeySparse,    KeyDense>;
      using KeyMut       = Tif<CT::Sparse<K>, KeySparseMut, KeyDenseMut>;

      using ValDenseMut  = V&;
      using ValDense     = V const&;
      using ValSparseMut = Handle<V>;
      using ValSparse    = V;
      using Val          = Tif<CT::Sparse<V>, ValSparse,    ValDense>;
      using ValMut       = Tif<CT::Sparse<V>, ValSparseMut, ValDenseMut>;

      ///                                                                     
      ///   Construction                                                      
      constexpr TMap() noexcept = default;
      constexpr TMap(const TMap&) noexcept = default;
      constexpr TMap(TMap&&) noexcept = default;

      template<class A1, class...AN>
      TMap(A1&&, AN&&...) requires CT::RangeInsertable<TMap, A1, AN...>;

      ///                                                                     
      ///   Capsulation                                                       
      Hash GetHash() const;
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
      Component::IndexedHash<>,        // Indexed directly              
      Component::Insertion<>,          // Allows insertion              
      Component::InsertionOperators<>, // << and >> insertion           
      Component::Emplacement,          // Allows emplacement            
      Component::Removal,              // Allows removal                
      Component::Assignment,           // Allows assignment             
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
      using KeySparseMut = Handle<K>;
      using KeySparse    = K;
      using Key          = Tif<CT::Sparse<K>, KeySparse,    KeyDense>;
      using KeyMut       = Tif<CT::Sparse<K>, KeySparseMut, KeyDenseMut>;

      using ValDenseMut  = V&;
      using ValDense     = V const&;
      using ValSparseMut = Handle<V>;
      using ValSparse    = V;
      using Val          = Tif<CT::Sparse<V>, ValSparse,    ValDense>;
      using ValMut       = Tif<CT::Sparse<V>, ValSparseMut, ValDenseMut>;

      ///                                                                     
      ///   Construction                                                      
      constexpr TMapUnsorted() noexcept = default;
      constexpr TMapUnsorted(const TMapUnsorted&) noexcept = default;
      constexpr TMapUnsorted(TMapUnsorted&&) noexcept = default;

      template<class A1, class...AN>
      TMapUnsorted(A1&&, AN&&...) requires CT::RangeInsertable<TMapUnsorted, A1, AN...>;

      ///                                                                     
      ///   Capsulation                                                       
      Hash GetHash() const;
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
      Component::IndexedHash<>,        // Indexed directly              
      Component::Insertion<>,          // Allows insertion              
      Component::InsertionOperators<>, // << and >> insertion           
      Component::Emplacement,          // Allows emplacement            
      Component::Removal,              // Allows removal                
      Component::Assignment,           // Allows assignment             
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
      using KeySparseMut = Handle<K>;
      using KeySparse    = K;
      using Key          = Tif<CT::Sparse<K>, KeySparse,    KeyDense>;
      using KeyMut       = Tif<CT::Sparse<K>, KeySparseMut, KeyDenseMut>;

      using ValDenseMut  = V&;
      using ValDense     = V const&;
      using ValSparseMut = Handle<V>;
      using ValSparse    = V;
      using Val          = Tif<CT::Sparse<V>, ValSparse,    ValDense>;
      using ValMut       = Tif<CT::Sparse<V>, ValSparseMut, ValDenseMut>;

      ///                                                                     
      ///   Construction                                                      
      constexpr TMapSorted() noexcept = default;
      constexpr TMapSorted(const TMapSorted&) noexcept = default;
      constexpr TMapSorted(TMapSorted&&) noexcept = default;

      template<class A1, class...AN>
      TMapSorted(A1&&, AN&&...) requires CT::RangeInsertable<TMapSorted, A1, AN...>;

      ///                                                                     
      ///   Capsulation                                                       
      Hash GetHash() const;
   };

} // namespace Langulus::Anyness
