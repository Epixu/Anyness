///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include <Langulus/Utils/Iterate-Handles.hpp>
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
#include "../../../source/components/State-Stack.hpp"
#include "../../../source/states/Sorted.hpp"
#include "../../../source/states/Compressed.hpp"
#include "../../../source/states/Encrypted.hpp"
#include "../../../source/states/Tracked.hpp"


namespace Langulus::Anyness
{
   ///                                                                        
   /// Type-erased set of unspecified state                                   
   ///                                                                        
   struct Set : Container<
      Com::HeapMovable<>,              // Heap for keys                 
      Com::OwnershipStack<>,           // Keys allocation is referenced 
      Com::DeepOwnershipHeap<>,        // Sparse keys are referenced    
      Com::HashHeap<>,                 // Can be hashed                 
      Com::IndexedHash<>,              // Indexed by hashing keys       
      Com::Insertion<>,                // Allows insertion              
      Com::InsertionOperators<>,       // << and >> insertion           
      Com::Emplacement<>,              // Allows emplacement            
      Com::Removal<>,                  // Allows removal                
      Com::Assignment<>,               // Allows assignment             
      Com::TypedStack<DMeta, void>,    // Key type                      
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
      using CTTI_Set = Yes<>;

      constexpr Set() noexcept = default;
      constexpr Set(const Set&) noexcept = default;
      constexpr Set(Set&&) noexcept = default;

      template<class A1, class...AN>
      Set(A1&&, AN&&...) requires CT::RangeInsertable<Set, A1, AN...>;
   };
   

   ///                                                                        
   /// Unsorted type-erased set                                               
   ///                                                                        
   struct SetUnsorted : Container<
      Com::HeapMovable<>,              // Heap for keys                 
      Com::OwnershipStack<>,           // Keys allocation is referenced 
      Com::DeepOwnershipHeap<>,        // Sparse keys are referenced    
      Com::HashHeap<>,                 // Can be hashed                 
      Com::IndexedHash<>,              // Indexed by hashing keys       
      Com::Insertion<>,                // Allows insertion              
      Com::InsertionOperators<>,       // << and >> insertion           
      Com::Emplacement<>,              // Allows emplacement            
      Com::Removal<>,                  // Allows removal                
      Com::Assignment<>,               // Allows assignment             
      Com::TypedStack<DMeta, void>,    // Key type                      
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
      using CTTI_ReflectAs = Set;
      using CTTI_Set       = Yes<>;

      constexpr SetUnsorted() noexcept = default;
      constexpr SetUnsorted(const SetUnsorted&) noexcept = default;
      constexpr SetUnsorted(SetUnsorted&&) noexcept = default;

      template<class A1, class...AN>
      SetUnsorted(A1&&, AN&&...) requires CT::RangeInsertable<SetUnsorted, A1, AN...>;
   };
   

   ///                                                                        
   /// Sorted type-erased set                                                 
   ///                                                                        
   struct SetSorted : Container<
      Com::HeapMovable<>,              // Heap for keys                 
      Com::OwnershipStack<>,           // Keys allocation is referenced 
      Com::DeepOwnershipHeap<>,        // Sparse keys are referenced    
      Com::HashHeap<>,                 // Can be hashed                 
      Com::IndexedHash<>,              // Indexed by hashing keys       
      Com::Insertion<>,                // Allows insertion              
      Com::InsertionOperators<>,       // << and >> insertion           
      Com::Emplacement<>,              // Allows emplacement            
      Com::Removal<>,                  // Allows removal                
      Com::Assignment<>,               // Allows assignment             
      Com::TypedStack<DMeta, void>,    // Key type                      
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
      using CTTI_ReflectAs = Set;
      using CTTI_Set       = Yes<>;

      constexpr SetSorted() noexcept = default;
      constexpr SetSorted(const SetSorted&) noexcept = default;
      constexpr SetSorted(SetSorted&&) noexcept = default;

      template<class A1, class...AN>
      SetSorted(A1&&, AN&&...) requires CT::RangeInsertable<SetSorted, A1, AN...>;
   };
}
