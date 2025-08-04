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
   ///                                                                        
   /// A continuous byte container view (without ownership) of variable size, 
   /// that is binary compatible with the container above                     
   ///                                                                        
   struct BytesView : Container<
      Com::HeapMovable<>,              // Pointer to heap memory        
      Com::OwnershipStack<0, false>,   // Allocation is referenced      
      Com::Contiguous,                 // Heap memory is continuous     
      Com::IndexedLinear<>,            // Indexed directly              
      Com::TypedStatic<DMeta, Byte>,   // Type-constrained              
      Com::CountStack<>,               // Variable count                
      Com::ReserveHeap<>,              // Variable capacity             
      Com::HashStack<>,                // Variable hash (cached)        
      Com::IterationRange<>,           // Ranged iteration              
      Com::Comparison,                 // Comparisons                   
      Com::StateStack<                 // Variable state                
         DefineState::Typed<State::Enabled>, // Always type-constrained 
         DefineState::Compressed<>,    // Adds 'compressed' state       
         DefineState::Encrypted<>,     // Adds 'encrypted' state        
         DefineState::Tracked<>        // Adds 'tracked' state          
      >
   > {
      using CTTI_ReflectAs = Bytes;

      constexpr BytesView() noexcept = default;
      constexpr BytesView(const BytesView&) noexcept = default;
      constexpr BytesView(BytesView&&) noexcept = default;

      constexpr BytesView(const CT::Container auto&) noexcept;
   };
}
