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
      Component::IndexedHash<>,        // Indexed directly              
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
      Component::IndexedHash<>,        // Indexed directly              
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
      Component::IndexedHash<>,        // Indexed directly              
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
   };

} // namespace Langulus::Anyness
