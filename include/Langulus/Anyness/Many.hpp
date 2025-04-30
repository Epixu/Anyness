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
#include "../../../source/components/Count-Stack.hpp"
#include "../../../source/components/Contiguous.hpp"
#include "../../../source/components/Indexed-Linear.hpp"
#include "../../../source/components/Typed-Stack.hpp"
#include "../../../source/components/Reserve-Stack.hpp"
#include "../../../source/components/Hash-Stack.hpp"
#include "../../../source/components/Descriptor.hpp"
#include "../../../source/components/State-Stack.hpp"
#include "../../../source/components/Emplacement.hpp"
#include "../../../source/components/Insertion.hpp"
#include "../../../source/components/InsertionOperators.hpp"
#include "../../../source/components/Removal.hpp"
#include "../../../source/components/Assignment.hpp"
#include "../../../source/components/Iteration-ForEach.hpp"
#include "../../../source/components/Comparison.hpp"
#include "../../../source/components/Conversion.hpp"
#include "../../../source/states/Future.hpp"
#include "../../../source/states/Past.hpp"
#include "../../../source/states/Compressed.hpp"
#include "../../../source/states/Encrypted.hpp"
#include "../../../source/states/Or.hpp"
#include "../../../source/states/Tracked.hpp"
#include "../../../source/states/Typed.hpp"
#include "../../../source/rtti/MetaData.hpp"


namespace Langulus::Anyness
{

   struct Many;
   struct ManyView;

   /// A universal type-erased continuous container of variable size          
   struct Many : Container<
      Component::HeapMovable<>,        // Pointer to heap memory        
      Component::OwnershipStack<>,     // Allocation is referenced      
      Component::DeepOwnership<>,      // Referenced indirections       
      Component::Contiguous,           // Heap memory is continuous     
      Component::IndexedLinear<>,      // Indexed directly              
      Component::Insertion<>,          // Allows insertion              
      Component::Emplacement,          // Allows emplacement            
      Component::InsertionOperators<>, // << and >> insertion           
      Component::Removal,              // Allows removal                
      Component::Assignment,           // Allows assignment             
      Component::TypedStack<DMeta>,    // Variable type                 
      Component::CountStack<>,         // Variable count                
      Component::ReserveStack<>,       // Variable capacity             
      Component::HashStack<>,          // Variable hash (cached)        
      Component::Descriptor,           // Descriptor interface          
      Component::IterationForEach,     // ForEach iteration             
      Component::Comparison,           // Allows for comparison         
      Component::Conversion,           // Allows conversion             
      Component::StateStack<           // Variable state                
         DefineState::Typed<>,         // Can be type-constrained       
         DefineState::Future<>,        // Adds a 'missing future' state 
         DefineState::Past<>,          // Adds a 'missing past' state   
         DefineState::Compressed<>,    // Adds 'compressed' state       
         DefineState::Encrypted<>,     // Adds 'encrypted' state        
         DefineState::Or<>,            // Adds 'or' state               
         DefineState::Tracked<>        // Adds 'tracked' state          
      >
   > {
      // View                                                           
      using  ViewType = ManyView;

      // Deep type                                                      
      using  DeepType = Many;

      // Single element selections                                      
      struct PickMut : Container<
         Component::HeapMovable<>,
         Component::OwnershipStack<>,
         Component::Assignment,
         Component::TypedStack<DMeta>
      > {};
      struct Pick : Container<
         Component::HeapMovable<>,
         Component::TypedStack<DMeta>
      > {};

      // Range selections                                               
      struct PickRangeMut : Container<
         Component::HeapMovable<>,
         Component::OwnershipStack<0, false>,
         Component::DeepOwnership<>,
         Component::Contiguous,
         Component::IndexedLinear<>,
         Component::Assignment,
         Component::TypedStack<DMeta>,
         Component::CountStack<>,
         Component::ReserveStack<>
      > {};
      struct PickRange : Container<
         Component::HeapMovable<>,
         Component::OwnershipStack<0, false>,
         Component::Contiguous,
         Component::IndexedLinear<>,
         Component::TypedStack<DMeta>,
         Component::CountStack<>,
         Component::ReserveStack<>
      > {};

      constexpr Many() = default;
      Many(const Many&) noexcept;
      Many(Many&&) noexcept;

      //template<template<class> class I> requires CT::Intent<I<Many>>
      //explicit Many(I<Many>&&) noexcept;

      template<class A1, class...AN>
      Many(A1&&, AN&&...) requires CT::RangeInsertable<Many, A1, AN...>;
   };
   
   using Messy = Many;

   /// A universal type-erased continuous container view of variable size     
   /// Doesn't have ownership, and binary-compatible with the container above 
   struct ManyView : Container<
      Component::HeapMovable<>,        // Pointer to heap memory        
      Component::OwnershipStack<0, false>,   // Pointer to an allocation
      Component::Contiguous,           // Heap memory is continuous     
      Component::IndexedLinear<>,      // Indexed directly              
      Component::TypedStack<DMeta>,    // Variable type                 
      Component::CountStack<>,         // Variable count                
      Component::ReserveStack<>,       // Variable capacity             
      Component::HashStack<>,          // Variable hash (cached)        
      Component::Descriptor,           // Descriptor interface          
      Component::IterationForEach,     // ForEach iteration             
      Component::Comparison,           // Allows for comparison         
      Component::Conversion,           // Allows conversion             
      Component::StateStack<           // Variable state                
         DefineState::Typed<>,         // Can be type-constrained       
         DefineState::Future<>,        // Adds a 'missing future' state 
         DefineState::Past<>,          // Adds a 'missing past' state   
         DefineState::Compressed<>,    // Adds 'compressed' state       
         DefineState::Encrypted<>,     // Adds 'encrypted' state        
         DefineState::Or<>,            // Adds 'or' state               
         DefineState::Tracked<>        // Adds 'tracked' state          
      >
   > {};
   
} // namespace Langulus::Anyness
