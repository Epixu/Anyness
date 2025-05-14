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
#include "Handle.hpp"


namespace Langulus::Anyness
{

   ///                                                                        
   /// Type-erased map of unspecified state                                   
   ///                                                                        
   struct Map : Container<
      Component::HeapMovable<0>,       // Heap for keys                 
      Component::HeapMovable<1>,       // Heap for values               
      Component::OwnershipStack<0>,    // Keys allocation is referenced 
      Component::OwnershipStack<1>,    // Vals allocation is referenced 
      Component::DeepOwnership<0>,     // Sparse keys are referenced    
      Component::DeepOwnership<1>,     // Sparse vals are referenced    
      Component::HashHeap<0>,          // Keys can be hashed            
      Component::HashHeap<1>,          // Values can be hashed          
      Component::IndexedHash<0>,       // Indexed by hashing keys       
      Component::Insertion<0>,         // Allows key insertion          
      Component::Insertion<1>,         // Allows val insertion          
      Component::InsertionOperators<0>,// << and >> insertion of keys   
      Component::InsertionOperators<1>,// << and >> insertion of vals   
      Component::Emplacement<0>,       // Allows emplacement of keys    
      Component::Emplacement<1>,       // Allows emplacement of vals    
      Component::Removal<0>,           // Allows removal of keys        
      Component::Removal<1>,           // Allows removal of vals        
      Component::Assignment<0>,        // Allows assignment of keys     
      Component::Assignment<1>,        // Allows assignment of vals     
      Component::TypedStack<DMeta, void, 0>,    // Key type             
      Component::TypedStack<DMeta, void, 1>,    // Value type           
      Component::CountStack<>,         // Variable count                
      Component::ReserveStack<>,       // Variable capacity             
      Component::Comparison,           // Allows for comparison         
      Component::IterationForEach<0>,  // Iterate keys using lambdas    
      Component::IterationForEach<1>,  // Iterate vals using lambdas    
      Component::IterationRange<0>,    // Iterate keys using ranges     
      Component::IterationRange<1>,    // Iterate vals using ranges     
      Component::StateStack<           // Variable state                
         DefineState::Sorted<>,        // Maybe unsorted                
         DefineState::Compressed<>,    // Adds 'compressed' state       
         DefineState::Encrypted<>,     // Adds 'encrypted' state        
         DefineState::Tracked<>        // Adds 'tracked' state          
      >
   > {
      using CTTI_Map = Yes;

      using KeyMut = HandleMut;
      using Key    = Handle;
      using ValMut = HandleMut;
      using Val    = Handle;
      using It     = TIteratorMap<Map>;

      static constexpr bool TypeErased = true;

      ///                                                                     
      ///   Construction                                                      
      constexpr Map() noexcept = default;
      constexpr Map(const Map&) noexcept = default;
      constexpr Map(Map&&) noexcept = default;

      template<class A1, class...AN>
      Map(A1&&, AN&&...) requires CT::RangeInsertable<Map, A1, AN...>;

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
   /// Unsorted type-erased map                                               
   ///                                                                        
   struct MapUnsorted : Container<
      Component::HeapMovable<0>,       // Heap for keys                 
      Component::HeapMovable<1>,       // Heap for values               
      Component::OwnershipStack<0>,    // Keys allocation is referenced 
      Component::OwnershipStack<1>,    // Vals allocation is referenced 
      Component::DeepOwnership<0>,     // Sparse keys are referenced    
      Component::DeepOwnership<1>,     // Sparse vals are referenced    
      Component::HashHeap<0>,          // Keys can be hashed            
      Component::HashHeap<1>,          // Values can be hashed          
      Component::IndexedHash<0>,       // Indexed by hashing keys       
      Component::Insertion<0>,         // Allows key insertion          
      Component::Insertion<1>,         // Allows val insertion          
      Component::InsertionOperators<0>,// << and >> insertion of keys   
      Component::InsertionOperators<1>,// << and >> insertion of vals   
      Component::Emplacement<0>,       // Allows emplacement of keys    
      Component::Emplacement<1>,       // Allows emplacement of vals    
      Component::Removal<0>,           // Allows removal of keys        
      Component::Removal<1>,           // Allows removal of vals        
      Component::Assignment<0>,        // Allows assignment of keys     
      Component::Assignment<1>,        // Allows assignment of vals     
      Component::TypedStack<DMeta, void, 0>,    // Key type             
      Component::TypedStack<DMeta, void, 1>,    // Value type           
      Component::CountStack<>,         // Variable count                
      Component::ReserveStack<>,       // Variable capacity             
      Component::Comparison,           // Allows for comparison         
      Component::IterationForEach<0>,  // Iterate keys using lambdas    
      Component::IterationForEach<1>,  // Iterate vals using lambdas    
      Component::IterationRange<0>,    // Iterate keys using ranges     
      Component::IterationRange<1>,    // Iterate vals using ranges     
      Component::StateStack<           // Variable state                
         DefineState::Sorted<State::Disabled>,  // Always unsorted      
         DefineState::Compressed<>,    // Adds 'compressed' state       
         DefineState::Encrypted<>,     // Adds 'encrypted' state        
         DefineState::Tracked<>        // Adds 'tracked' state          
      >
   > {
      using CTTI_ReflectAs = Map;
      using CTTI_Map       = Yes;

      using KeyMut = HandleMut;
      using Key    = Handle;
      using ValMut = HandleMut;
      using Val    = Handle;
      using It     = TIteratorMap<Map>;

      static constexpr bool TypeErased = true;

      ///                                                                     
      ///   Construction                                                      
      constexpr MapUnsorted() noexcept = default;
      constexpr MapUnsorted(const MapUnsorted&) noexcept = default;
      constexpr MapUnsorted(MapUnsorted&&) noexcept = default;

      template<class A1, class...AN>
      MapUnsorted(A1&&, AN&&...) requires CT::RangeInsertable<MapUnsorted, A1, AN...>;

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
   /// Sorted type-erased map                                                 
   ///                                                                        
   struct MapSorted : Container<
      Component::HeapMovable<0>,       // Heap for keys                 
      Component::HeapMovable<1>,       // Heap for values               
      Component::OwnershipStack<0>,    // Keys allocation is referenced 
      Component::OwnershipStack<1>,    // Vals allocation is referenced 
      Component::DeepOwnership<0>,     // Sparse keys are referenced    
      Component::DeepOwnership<1>,     // Sparse vals are referenced    
      Component::HashHeap<0>,          // Keys can be hashed            
      Component::HashHeap<1>,          // Values can be hashed          
      Component::IndexedHash<0>,       // Indexed by hashing keys       
      Component::Insertion<0>,         // Allows key insertion          
      Component::Insertion<1>,         // Allows val insertion          
      Component::InsertionOperators<0>,// << and >> insertion of keys   
      Component::InsertionOperators<1>,// << and >> insertion of vals   
      Component::Emplacement<0>,       // Allows emplacement of keys    
      Component::Emplacement<1>,       // Allows emplacement of vals    
      Component::Removal<0>,           // Allows removal of keys        
      Component::Removal<1>,           // Allows removal of vals        
      Component::Assignment<0>,        // Allows assignment of keys     
      Component::Assignment<1>,        // Allows assignment of vals     
      Component::TypedStack<DMeta, void, 0>,    // Key type             
      Component::TypedStack<DMeta, void, 1>,    // Value type           
      Component::CountStack<>,         // Variable count                
      Component::ReserveStack<>,       // Variable capacity             
      Component::Comparison,           // Allows for comparison         
      Component::IterationForEach<0>,  // Iterate keys using lambdas    
      Component::IterationForEach<1>,  // Iterate vals using lambdas    
      Component::IterationRange<0>,    // Iterate keys using ranges     
      Component::IterationRange<1>,    // Iterate vals using ranges     
      Component::StateStack<           // Variable state                
         DefineState::Sorted<State::Enabled>,   // Always sorted        
         DefineState::Compressed<>,    // Adds 'compressed' state       
         DefineState::Encrypted<>,     // Adds 'encrypted' state        
         DefineState::Tracked<>        // Adds 'tracked' state          
      >
   > {
      using CTTI_ReflectAs = Map;
      using CTTI_Map       = Yes;

      using KeyMut = HandleMut;
      using Key    = Handle;
      using ValMut = HandleMut;
      using Val    = Handle;
      using It     = TIteratorMap<Map>;

      static constexpr bool TypeErased = true;

      ///                                                                     
      ///   Construction                                                      
      constexpr MapSorted() noexcept = default;
      constexpr MapSorted(const MapSorted&) noexcept = default;
      constexpr MapSorted(MapSorted&&) noexcept = default;

      template<class A1, class...AN>
      MapSorted(A1&&, AN&&...) requires CT::RangeInsertable<MapSorted, A1, AN...>;

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
