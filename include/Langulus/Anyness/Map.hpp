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
#include "../../../source/components/Indexed-Hash.hpp"
#include "../../../source/components/Insertion.hpp"
#include "../../../source/components/InsertionOperators.hpp"
#include "../../../source/components/Emplacement.hpp"
#include "../../../source/components/Removal.hpp"
#include "../../../source/components/Assignment.hpp"
#include "../../../source/components/Typed-Stack.hpp"
#include "../../../source/components/Count-Stack.hpp"
#include "../../../source/components/Reserve-Stack.hpp"
#include "../../../source/components/State-Stack.hpp"
#include "../../../source/components/Comparison.hpp"
#include "../../../source/components/Hash-Heap.hpp"
#include "../../../source/states/Sorted.hpp"
#include "../../../source/states/Compressed.hpp"
#include "../../../source/states/Encrypted.hpp"
#include "../../../source/states/Tracked.hpp"


namespace Langulus::Anyness
{
   namespace Inner
   {

      ///                                                                     
      /// Common type-erased key type                                         
      struct KeyMut : Container<
         Component::HeapMovable<>,
         Component::OwnershipStack<>,
         Component::TypedStack<DMeta>
      > {};
      struct Key : Container<
         Component::HeapMovable<>,
         Component::TypedStack<DMeta>
      > {};

      ///                                                                     
      /// Common type-erased value type                                       
      struct ValMut : Container<
         Component::HeapMovable<>,
         Component::OwnershipStack<>,
         Component::Assignment,
         Component::TypedStack<DMeta>
      > {};
      struct Val : Container<
         Component::HeapMovable<>,
         Component::TypedStack<DMeta>
      > {};

   } // namespace Langulus::Anyness::Inner


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
      Component::IndexedHash<>,        // Indexed directly              
      Component::Insertion<>,          // Allows insertion              
      Component::InsertionOperators<>, // << and >> insertion           
      Component::Emplacement,          // Allows emplacement            
      Component::Removal,              // Allows removal                
      Component::Assignment,           // Allows assignment             
      Component::TypedStack<DMeta, void, 0>,    // Key type             
      Component::TypedStack<DMeta, void, 1>,    // Value type           
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
      using CTTI_Map = Yes;
      using KeyMut = Inner::KeyMut;
      using Key    = Inner::Key;
      using ValMut = Inner::ValMut;
      using Val    = Inner::Val;

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
      Component::IndexedHash<>,        // Indexed directly              
      Component::Insertion<>,          // Allows insertion              
      Component::InsertionOperators<>, // << and >> insertion           
      Component::Emplacement,          // Allows emplacement            
      Component::Removal,              // Allows removal                
      Component::Assignment,           // Allows assignment             
      Component::TypedStack<DMeta, void, 0>,    // Key type             
      Component::TypedStack<DMeta, void, 1>,    // Value type           
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
      using KeyMut = Inner::KeyMut;
      using Key    = Inner::Key;
      using ValMut = Inner::ValMut;
      using Val    = Inner::Val;

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
      Component::IndexedHash<>,        // Indexed directly              
      Component::Insertion<>,          // Allows insertion              
      Component::InsertionOperators<>, // << and >> insertion           
      Component::Emplacement,          // Allows emplacement            
      Component::Removal,              // Allows removal                
      Component::Assignment,           // Allows assignment             
      Component::TypedStack<DMeta, void, 0>,    // Key type             
      Component::TypedStack<DMeta, void, 1>,    // Value type           
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
      using KeyMut = Inner::KeyMut;
      using Key    = Inner::Key;
      using ValMut = Inner::ValMut;
      using Val    = Inner::Val;

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
   };

} // namespace Langulus::Anyness
