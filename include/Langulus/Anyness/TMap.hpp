///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Map.hpp"


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
      Component::StateStack<           // Variable state                
         State::Sorted<>,                       // Maybe unsorted       
         State::Compressed<>,          // Adds 'compressed' state       
         State::Encrypted<>,           // Adds 'encrypted' state        
         State::Tracked<>              // Adds 'tracked' state          
      >
   > {
      using CTTI_ReflectAs = Map;
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
      Component::StateStack<           // Variable state                
         State::Sorted<State::Disabled>,        // Always unsorted      
         State::Compressed<>,          // Adds 'compressed' state       
         State::Encrypted<>,           // Adds 'encrypted' state        
         State::Tracked<>              // Adds 'tracked' state          
      >
   > {
      using CTTI_ReflectAs = Map;
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
      Component::StateStack<           // Variable state                
         State::Sorted<State::Enabled>,         // Always sorted        
         State::Compressed<>,          // Adds 'compressed' state       
         State::Encrypted<>,           // Adds 'encrypted' state        
         State::Tracked<>              // Adds 'tracked' state          
      >
   > {
      using CTTI_ReflectAs = Map;
   };

} // namespace Langulus::Anyness
