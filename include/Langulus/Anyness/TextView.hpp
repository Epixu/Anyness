///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../../../source/Container.hpp"
#include "../../../source/components/Heap-Reference.hpp"
#include "../../../source/components/Ownership-Stack.hpp"
#include "../../../source/components/Contiguous.hpp"
#include "../../../source/components/Indexed-Linear.hpp"
#include "../../../source/components/Typed-Static.hpp"
#include "../../../source/components/Count-Stack.hpp"
#include "../../../source/components/Reserve-Heap.hpp"
#include "../../../source/components/Hash-Stack.hpp"
#include "../../../source/components/State-Stack.hpp"
#include "../../../source/components/Iteration-ForEach.hpp"
#include "../../../source/components/Iteration-Range.hpp"
#include "../../../source/components/Comparison.hpp"
#include "../../../source/components/Conversion.hpp"
#include "../../../source/states/Compressed.hpp"
#include "../../../source/states/Encrypted.hpp"
#include "../../../source/states/Tracked.hpp"
#include "../../../source/states/Typed.hpp"


namespace Langulus::Anyness
{
   struct Text;

   struct TextView : Container<
      Com::HeapReference<>,               // Pointer to heap memory     
      Com::OwnershipStack<0, false>,      // Pointer to an allocation   
      Com::Contiguous,                    // Heap memory is continuous  
      Com::IndexedLinear<>,               // Indexed directly           
      Com::TypedStatic<DMeta, char>,      // Type-constrained           
      Com::CountStack<>,                  // Variable count             
      Com::ReserveHeap<>,                 // Variable capacity          
      Com::HashStack<>,                   // Variable hash (cached)     
      Com::IterationForEach<>,            // ForEach iteration          
      Com::IterationRange<>,              // ForEach iteration          
      Com::Comparison,                    // Allows for comparison      
      Com::Conversion,                    // Allows conversion          
      Com::StateStack<                    // Variable state             
         DefineState::Typed<State::Enabled>, // Always typed            
         DefineState::Compressed<>,       // Adds 'compressed' state    
         DefineState::Encrypted<>,        // Adds 'encrypted' state     
         DefineState::Tracked<>           // Adds 'tracked' state       
      >
   > {
      using CTTI_ReflectAs = Text;

      //TextView() noexcept = default;
      TextView(TextView const&) noexcept = default;
      TextView(TextView&&) noexcept = default;
      explicit TextView(Text const&) noexcept;
   };
}
