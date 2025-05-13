///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Set.hpp"


namespace Langulus::Anyness
{

   ///                                                                        
   /// Statically typed set of unspecified state                              
   ///                                                                        
   template<CT::NotVoid K>
   struct TSet : Container<
      Component::HeapMovable<>,        // Heap for keys                 
      Component::OwnershipStack<>,     // Keys allocation is referenced 
      Component::DeepOwnership<>,      // Sparse keys are referenced    
      Component::HashHeap<>,           // Keys can be hashed            
      Component::IndexedHash<>,        // Indexed by hashing keys       
      Component::Insertion<>,          // Allows insertion              
      Component::InsertionOperators<>, // << and >> insertion           
      Component::Emplacement,          // Allows emplacement            
      Component::Removal,              // Allows removal                
      Component::Assignment,           // Allows assignment             
      Component::TypedStack<DMeta, K>,       // Key type                
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
      using CTTI_ReflectAs = Set;
      using CTTI_Set = Yes;

      constexpr TSet() noexcept = default;
      constexpr TSet(const TSet&) noexcept = default;
      constexpr TSet(TSet&&) noexcept = default;

      template<class A1, class...AN>
      TSet(A1&&, AN&&...) requires CT::RangeInsertable<TSet, A1, AN...>;
   };
   

   ///                                                                        
   /// Unsorted statically typed set                                          
   ///                                                                        
   template<CT::NotVoid K>
   struct TSetUnsorted : Container<
      Component::HeapMovable<>,        // Heap for keys                 
      Component::OwnershipStack<>,     // Keys allocation is referenced 
      Component::DeepOwnership<>,      // Sparse keys are referenced    
      Component::HashHeap<>,           // Keys can be hashed            
      Component::IndexedHash<>,        // Indexed by hashing keys       
      Component::Insertion<>,          // Allows insertion              
      Component::InsertionOperators<>, // << and >> insertion           
      Component::Emplacement,          // Allows emplacement            
      Component::Removal,              // Allows removal                
      Component::Assignment,           // Allows assignment             
      Component::TypedStack<DMeta, K>,       // Key type                
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
      using CTTI_ReflectAs = Set;
      using CTTI_Set = Yes;

      constexpr TSetUnsorted() noexcept = default;
      constexpr TSetUnsorted(const TSetUnsorted&) noexcept = default;
      constexpr TSetUnsorted(TSetUnsorted&&) noexcept = default;

      template<class A1, class...AN>
      TSetUnsorted(A1&&, AN&&...) requires CT::RangeInsertable<TSetUnsorted, A1, AN...>;
   };
   

   ///                                                                        
   /// Sorted statically typed set                                            
   ///                                                                        
   template<CT::NotVoid K>
   struct TSetSorted : Container<
      Component::HeapMovable<>,        // Heap for keys                 
      Component::OwnershipStack<>,     // Keys allocation is referenced 
      Component::DeepOwnership<>,      // Sparse keys are referenced    
      Component::HashHeap<>,           // Keys can be hashed            
      Component::IndexedHash<>,        // Indexed by hashing keys       
      Component::Insertion<>,          // Allows insertion              
      Component::InsertionOperators<>, // << and >> insertion           
      Component::Emplacement,          // Allows emplacement            
      Component::Removal,              // Allows removal                
      Component::Assignment,           // Allows assignment             
      Component::TypedStack<DMeta, K>,       // Key type                
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
      using CTTI_ReflectAs = Set;
      using CTTI_Set = Yes;

      constexpr TSetSorted() noexcept = default;
      constexpr TSetSorted(const TSetSorted&) noexcept = default;
      constexpr TSetSorted(TSetSorted&&) noexcept = default;

      template<class A1, class...AN>
      TSetSorted(A1&&, AN&&...) requires CT::RangeInsertable<TSetSorted, A1, AN...>;
   };

} // namespace Langulus::Anyness
