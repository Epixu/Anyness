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
#include "../../../source/components/Contiguous.hpp"
#include "../../../source/components/Indexed-Linear.hpp"
#include "../../../source/components/Insertion.hpp"
#include "../../../source/components/InsertionOperators.hpp"
#include "../../../source/components/Emplacement.hpp"
#include "../../../source/components/Removal.hpp"
#include "../../../source/components/Assignment.hpp"
#include "../../../source/components/Typed-Stack.hpp"
#include "../../../source/components/Count-Stack.hpp"
#include "../../../source/components/Reserve-Stack.hpp"
#include "../../../source/components/Hash-Stack.hpp"
#include "../../../source/components/Descriptor.hpp"
#include "../../../source/components/Iteration-ForEach.hpp"
#include "../../../source/components/Iteration-Range.hpp"
#include "../../../source/components/Comparison.hpp"
#include "../../../source/components/Conversion.hpp"
#include "../../../source/components/State-Stack.hpp"
#include "../../../source/states/Typed.hpp"
#include "../../../source/states/Future.hpp"
#include "../../../source/states/Past.hpp"
#include "../../../source/states/Compressed.hpp"
#include "../../../source/states/Encrypted.hpp"
#include "../../../source/states/Or.hpp"
#include "../../../source/states/Tracked.hpp"
#include "../../../source/rtti/MetaData.hpp"
#include "Handle.hpp"


namespace Langulus::Anyness
{

   struct Many;
   struct ManyView;


   ///                                                                        
   /// A universal type-erased contiguous container of variable size          
   /// This is the most universal and feature-complete container, that        
   /// supports all kinds of data states: compression, encryption, linking,   
   /// and so on. If you want to contain a single element, consider using     
   /// Any instead, for a bit shorter and faster representation.              
   ///                                                                        
   struct Many : Container<
      Com::HeapMovable<>,              // Pointer to heap memory        
      Com::OwnershipStack<>,           // Allocation is referenced      
      Com::DeepOwnershipHeap<>,        // Referenced indirections       
      Com::Contiguous,                 // Heap memory is continuous     
      Com::IndexedLinear<>,            // Indexed directly              
      Com::Insertion<>,                // Allows insertion              
      Com::InsertionOperators<>,       // << and >> insertion           
      Com::Emplacement<>,              // Allows emplacement            
      Com::Removal<>,                  // Allows removal                
      Com::Assignment<>,               // Allows assignment             
      Com::TypedStack<DMeta>,          // Variable type                 
      Com::CountStack<>,               // Variable count                
      Com::ReserveStack<>,             // Variable capacity             
      Com::HashStack<>,                // Variable hash (cached)        
      Com::Descriptor,                 // Descriptor interface          
      Com::IterationForEach<>,         // ForEach iteration             
      Com::IterationRange<>,           // Ranged iteration              
      Com::Comparison,                 // Allows for comparison         
      Com::Conversion,                 // Allows conversion             
      Com::StateStack<                 // Variable state                
         DefineState::Typed<>,         // Can be type-constrained       
         DefineState::Future<>,        // Adds a 'missing future' state 
         DefineState::Past<>,          // Adds a 'missing past' state   
         DefineState::Compressed<>,    // Adds 'compressed' state       
         DefineState::Encrypted<>,     // Adds 'encrypted' state        
         DefineState::Or<>,            // Adds 'or' state               
         DefineState::Tracked<>        // Adds 'tracked' state          
      >
   > {
      using ViewType = ManyView;
      using DeepType = Many;
      using PickMut  = HandleMut;
      using Pick     = Handle;

      struct PickRangeMut : Container<
         Com::HeapMovable<>,
         Com::OwnershipStack<0, false>,
         Com::DeepOwnershipHeap<>,
         Com::Contiguous,
         Com::IndexedLinear<>,
         Com::Assignment<>,
         Com::TypedStack<DMeta>,
         Com::CountStack<>,
         Com::ReserveStack<>
      > {};
      struct PickRange : Container<
         Com::HeapMovable<>,
         Com::OwnershipStack<0, false>,
         Com::Contiguous,
         Com::IndexedLinear<>,
         Com::TypedStack<DMeta>,
         Com::CountStack<>,
         Com::ReserveStack<>
      > {};

      ///                                                                     
      /// Construction                                                        
      constexpr Many() noexcept = default;
      constexpr Many(const Many&) noexcept;
      constexpr Many(Many&&) noexcept;

      //template<template<class> class I> requires CT::Intent<I<Many>>
      //explicit Many(I<Many>&&) noexcept;

      template<class A1, class...AN>
      Many(A1&&, AN&&...) requires CT::RangeInsertable<Many, A1, AN...>;
      
      ///                                                                     
      /// Assignment                                                          
      Many& operator = (Many const&) noexcept = default;
      Many& operator = (Many&&) noexcept = default;

      template<class A1> requires CT::RangeAssignable<Many, A1>
      Many& operator = (A1&&);
   };
   
   using Messy = Many;
   
} // namespace Langulus::Anyness

#include "Neat.hpp"