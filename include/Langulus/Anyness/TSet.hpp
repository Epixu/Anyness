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
      Com::HeapMovable<>,              // Heap for keys                 
      Com::OwnershipStack<>,           // Keys allocation is referenced 
      Com::DeepOwnershipHeap<>,        // Sparse keys are referenced    
      Com::HashHeap<>,                 // Keys can be hashed            
      Com::IndexedHash<>,              // Indexed by hashing keys       
      Com::Insertion<>,                // Allows insertion              
      Com::InsertionOperators<>,       // << and >> insertion           
      Com::Emplacement<>,              // Allows emplacement            
      Com::Removal<>,                  // Allows removal                
      Com::Assignment<>,               // Allows assignment             
      Com::TypedStack<DMeta, K>,       // Key type                      
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
      using CTTI_ReflectAs = Set;
      using CTTI_Set       = Yes<>;

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
      Com::HeapMovable<>,              // Heap for keys                 
      Com::OwnershipStack<>,           // Keys allocation is referenced 
      Com::DeepOwnershipHeap<>,        // Sparse keys are referenced    
      Com::HashHeap<>,                 // Keys can be hashed            
      Com::IndexedHash<>,              // Indexed by hashing keys       
      Com::Insertion<>,                // Allows insertion              
      Com::InsertionOperators<>,       // << and >> insertion           
      Com::Emplacement<>,              // Allows emplacement            
      Com::Removal<>,                  // Allows removal                
      Com::Assignment<>,               // Allows assignment             
      Com::TypedStack<DMeta, K>,       // Key type                      
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
      Com::HeapMovable<>,              // Heap for keys                 
      Com::OwnershipStack<>,           // Keys allocation is referenced 
      Com::DeepOwnershipHeap<>,        // Sparse keys are referenced    
      Com::HashHeap<>,                 // Keys can be hashed            
      Com::IndexedHash<>,              // Indexed by hashing keys       
      Com::Insertion<>,                // Allows insertion              
      Com::InsertionOperators<>,       // << and >> insertion           
      Com::Emplacement<>,              // Allows emplacement            
      Com::Removal<>,                  // Allows removal                
      Com::Assignment<>,               // Allows assignment             
      Com::TypedStack<DMeta, K>,       // Key type                      
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

      constexpr TSetSorted() noexcept = default;
      constexpr TSetSorted(const TSetSorted&) noexcept = default;
      constexpr TSetSorted(TSetSorted&&) noexcept = default;

      template<class A1, class...AN>
      TSetSorted(A1&&, AN&&...) requires CT::RangeInsertable<TSetSorted, A1, AN...>;
   };
}
