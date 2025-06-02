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
#include "Handle.hpp"


namespace Langulus::Anyness
{

   ///                                                                        
   /// Type-erased map of unspecified state                                   
   ///                                                                        
   struct Map : Container<
      Com::HeapMovable<0>,             // Heap for keys                 
      Com::HeapMovable<1>,             // Heap for values               
      Com::OwnershipStack<0>,          // Keys allocation is referenced 
      Com::OwnershipStack<1>,          // Vals allocation is referenced 
      Com::DeepOwnershipHeap<0>,       // Sparse keys are referenced    
      Com::DeepOwnershipHeap<1>,       // Sparse vals are referenced    
      Com::HashHeap<0>,                // Keys can be hashed            
      Com::HashHeap<1>,                // Values can be hashed          
      Com::IndexedHash<0>,             // Indexed by hashing keys       
      Com::Insertion<0>,               // Allows key insertion          
      Com::Insertion<1>,               // Allows val insertion          
      Com::InsertionOperators<0>,      // << and >> insertion of keys   
      Com::InsertionOperators<1>,      // << and >> insertion of vals   
      Com::Emplacement<0>,             // Allows emplacement of keys    
      Com::Emplacement<1>,             // Allows emplacement of vals    
      Com::Removal<0>,                 // Allows removal of keys        
      Com::Removal<1>,                 // Allows removal of vals        
      Com::Assignment<0>,              // Allows assignment of keys     
      Com::Assignment<1>,              // Allows assignment of vals     
      Com::TypedStack<DMeta, void, 0>, // Key type                      
      Com::TypedStack<DMeta, void, 1>, // Value type                    
      Com::CountStack<>,               // Variable count                
      Com::ReserveStack<>,             // Variable capacity             
      Com::Comparison,                 // Allows for comparison         
      Com::IterationForEach<0>,        // Iterate keys using lambdas    
      Com::IterationForEach<1>,        // Iterate vals using lambdas    
      Com::IterationRange<0>,          // Iterate keys using ranges     
      Com::IterationRange<1>,          // Iterate vals using ranges     
      Com::StateStack<                 // Variable state                
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

      using IteratorMut = typename IterateTogether<const Many,       Many>::Iterator;
      using Iterator    = typename IterateTogether<const Many, const Many>::Iterator;

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
      void RemoveIt   (IteratorMut&);
   };
   

   ///                                                                        
   /// Unsorted type-erased map                                               
   ///                                                                        
   struct MapUnsorted : Container<
      Com::HeapMovable<0>,             // Heap for keys                 
      Com::HeapMovable<1>,             // Heap for values               
      Com::OwnershipStack<0>,          // Keys allocation is referenced 
      Com::OwnershipStack<1>,          // Vals allocation is referenced 
      Com::DeepOwnershipHeap<0>,       // Sparse keys are referenced    
      Com::DeepOwnershipHeap<1>,       // Sparse vals are referenced    
      Com::HashHeap<0>,                // Keys can be hashed            
      Com::HashHeap<1>,                // Values can be hashed          
      Com::IndexedHash<0>,             // Indexed by hashing keys       
      Com::Insertion<0>,               // Allows key insertion          
      Com::Insertion<1>,               // Allows val insertion          
      Com::InsertionOperators<0>,      // << and >> insertion of keys   
      Com::InsertionOperators<1>,      // << and >> insertion of vals   
      Com::Emplacement<0>,             // Allows emplacement of keys    
      Com::Emplacement<1>,             // Allows emplacement of vals    
      Com::Removal<0>,                 // Allows removal of keys        
      Com::Removal<1>,                 // Allows removal of vals        
      Com::Assignment<0>,              // Allows assignment of keys     
      Com::Assignment<1>,              // Allows assignment of vals     
      Com::TypedStack<DMeta, void, 0>, // Key type                      
      Com::TypedStack<DMeta, void, 1>, // Value type                    
      Com::CountStack<>,               // Variable count                
      Com::ReserveStack<>,             // Variable capacity             
      Com::Comparison,                 // Allows for comparison         
      Com::IterationForEach<0>,        // Iterate keys using lambdas    
      Com::IterationForEach<1>,        // Iterate vals using lambdas    
      Com::IterationRange<0>,          // Iterate keys using ranges     
      Com::IterationRange<1>,          // Iterate vals using ranges     
      Com::StateStack<                 // Variable state                
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

      using IteratorMut = typename IterateTogether<const Many,       Many>::Iterator;
      using Iterator    = typename IterateTogether<const Many, const Many>::Iterator;

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
      void RemoveIt   (IteratorMut&);
   };
   

   ///                                                                        
   /// Sorted type-erased map                                                 
   ///                                                                        
   struct MapSorted : Container<
      Com::HeapMovable<0>,             // Heap for keys                 
      Com::HeapMovable<1>,             // Heap for values               
      Com::OwnershipStack<0>,          // Keys allocation is referenced 
      Com::OwnershipStack<1>,          // Vals allocation is referenced 
      Com::DeepOwnershipHeap<0>,       // Sparse keys are referenced    
      Com::DeepOwnershipHeap<1>,       // Sparse vals are referenced    
      Com::HashHeap<0>,                // Keys can be hashed            
      Com::HashHeap<1>,                // Values can be hashed          
      Com::IndexedHash<0>,             // Indexed by hashing keys       
      Com::Insertion<0>,               // Allows key insertion          
      Com::Insertion<1>,               // Allows val insertion          
      Com::InsertionOperators<0>,      // << and >> insertion of keys   
      Com::InsertionOperators<1>,      // << and >> insertion of vals   
      Com::Emplacement<0>,             // Allows emplacement of keys    
      Com::Emplacement<1>,             // Allows emplacement of vals    
      Com::Removal<0>,                 // Allows removal of keys        
      Com::Removal<1>,                 // Allows removal of vals        
      Com::Assignment<0>,              // Allows assignment of keys     
      Com::Assignment<1>,              // Allows assignment of vals     
      Com::TypedStack<DMeta, void, 0>, // Key type                      
      Com::TypedStack<DMeta, void, 1>, // Value type                    
      Com::CountStack<>,               // Variable count                
      Com::ReserveStack<>,             // Variable capacity             
      Com::Comparison,                 // Allows for comparison         
      Com::IterationForEach<0>,        // Iterate keys using lambdas    
      Com::IterationForEach<1>,        // Iterate vals using lambdas    
      Com::IterationRange<0>,          // Iterate keys using ranges     
      Com::IterationRange<1>,          // Iterate vals using ranges     
      Com::StateStack<                 // Variable state                
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

      using IteratorMut = typename IterateTogether<const Many,       Many>::Iterator;
      using Iterator    = typename IterateTogether<const Many, const Many>::Iterator;

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
      void RemoveIt   (IteratorMut&);
   };

} // namespace Langulus::Anyness
