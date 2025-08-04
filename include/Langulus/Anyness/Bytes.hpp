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
#include "../../../source/components/Contiguous.hpp"
#include "../../../source/components/Indexed-Linear.hpp"
#include "../../../source/components/Emplacement.hpp"
#include "../../../source/components/Insertion.hpp"
#include "../../../source/components/InsertionOperators.hpp"
#include "../../../source/components/Concatenate.hpp"
#include "../../../source/components/ConcatenateOperators.hpp"
#include "../../../source/components/Removal.hpp"
#include "../../../source/components/Assignment.hpp"
#include "../../../source/components/Typed-Static.hpp"
#include "../../../source/components/Count-Stack.hpp"
#include "../../../source/components/Reserve-Heap.hpp"
#include "../../../source/components/Hash-Stack.hpp"
#include "../../../source/components/Iteration-Range.hpp"
#include "../../../source/components/Comparison.hpp"
#include "../../../source/components/State-Stack.hpp"
#include "../../../source/states/Compressed.hpp"
#include "../../../source/states/Encrypted.hpp"
#include "../../../source/states/Tracked.hpp"
#include "../../../source/states/Typed.hpp"


namespace Langulus::Anyness
{
   struct Bytes;
   struct BytesView;


   ///                                                                        
   /// A continuous byte container of variable size                           
   ///                                                                        
   struct Bytes : Container<
      Com::HeapMovable<>,                 // Pointer to heap memory     
      Com::OwnershipStack<>,              // Allocation is referenced   
      Com::Contiguous,                    // Heap memory is continuous  
      Com::IndexedLinear<>,               // Indexed directly           
      Com::Emplacement<>,                 // Allows emplacement         
      Com::Insertion<0, Bytes>,           // Serialize + insert         
      Com::InsertionOperators<0, Bytes>,  // << and >> insertion        
      Com::Concatenate,                   // Concatenation              
      Com::ConcatenateOperators,          // + += concatenation         
      Com::Removal<>,                     // Allows removal             
      Com::Assignment<>,                  // Allows assignment          
      Com::TypedStatic<DMeta, Byte>,      // Type-constrained           
      Com::CountStack<>,                  // Variable count             
      Com::ReserveHeap<>,                 // Variable capacity          
      Com::HashStack<>,                   // Variable hash (cached)     
      Com::IterationRange<>,              // Ranged iteration           
      Com::Comparison,                    // Comparisons                
      Com::StateStack<                    // Variable state             
         DefineState::Typed<State::Enabled>, // Always type-constrained 
         DefineState::Compressed<>,       // Adds 'compressed' state    
         DefineState::Encrypted<>,        // Adds 'encrypted' state     
         DefineState::Tracked<>           // Adds 'tracked' state       
      >
   > {
      // View                                                           
      using  ViewType = BytesView;

      // Single element selections                                      
      using  Pick     = Byte const&;
      using  PickMut  = Byte&;

      // Range selections                                               
      struct PickRange : Container<
         Com::HeapMovable<>,
         Com::Contiguous,
         Com::IndexedLinear<>,
         Com::TypedStatic<DMeta, Byte>,
         Com::CountStack<>
      > {};
      struct PickRangeMut : Container<
         Com::HeapMovable<>,
         Com::Contiguous,
         Com::IndexedLinear<>,
         Com::Assignment<>,
         Com::TypedStatic<DMeta, Byte>,
         Com::CountStack<>
      > {};

      constexpr Bytes() noexcept = default;
      constexpr Bytes(const Bytes&) noexcept = default;
      constexpr Bytes(Bytes&&) noexcept = default;

      //template<template<class> class I> requires CT::Intent<I<Bytes>>
      //constexpr Bytes(I<Bytes>&&) noexcept;

      template<class A1>
      constexpr Bytes(A1&&) requires CT::DeepConstructible<Bytes, A1>;
      template<class A1, class...AN>
      constexpr Bytes(A1&&, AN&&...) requires CT::RangeInsertable<Bytes, A1, AN...>;
   };
}

#include "BytesView.hpp"
