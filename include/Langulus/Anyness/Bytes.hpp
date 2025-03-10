///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../../../source/Container.hpp"
#include "../../../source/Allocator.hpp"
#include "../../../source/components/Heap-Movable.hpp"
#include "../../../source/components/Ownership-Stack.hpp"
#include "../../../source/components/Contiguous.hpp"
#include "../../../source/components/Indexed-Linear.hpp"
#include "../../../source/components/Insertion.hpp"
#include "../../../source/components/Emplacement.hpp"
#include "../../../source/components/InsertionOperators.hpp"
#include "../../../source/components/Removal.hpp"
#include "../../../source/components/Assignment.hpp"
#include "../../../source/components/Typed-Static.hpp"
#include "../../../source/components/Count-Stack.hpp"
#include "../../../source/components/Reserve-Heap.hpp"
#include "../../../source/components/Hash-Stack.hpp"
#include "../../../source/components/State-Stack.hpp"
#include "../../../source/states/Compressed.hpp"
#include "../../../source/states/Encrypted.hpp"
#include "../../../source/states/Tracked.hpp"


namespace Langulus::Anyness
{

   using DMeta = RTTI::DMeta;

   /// A continuous byte container of variable size                           
   struct Bytes : Container<
      Component::HeapMovable<>,        // Pointer to heap memory        
      Component::OwnershipStack<>,     // Allocation is referenced      
      Component::Contiguous,           // Heap memory is continuous     
      Component::IndexedLinear<>,      // Indexed directly              
      Component::Insertion,            // Allows insertion              
      Component::Emplacement,          // Allows emplacement            
      Component::InsertionOperators,   // << and >> insertion           
      Component::Removal,              // Allows removal                
      Component::Assignment,           // Allows assignment             
      Component::TypedStatic<DMeta, Byte>, // Type-constrained          
      Component::CountStack<>,         // Variable count                
      Component::ReserveHeap<>,        // Variable capacity             
      Component::HashStack<>,          // Variable hash (cached)        
      Component::StateStack<           // Variable state                
         State::Compressed<>,          // Adds 'compressed' state       
         State::Encrypted<>,           // Adds 'encrypted' state        
         State::Tracked<>              // Adds 'tracked' state          
      >
   > {
      // Single element selections                                      
      using  Pick    = Byte const&;
      using  PickMut = Byte&;

      // Range selections                                               
      struct PickRange : Container<
         Component::HeapMovable<>,
         Component::Contiguous,
         Component::IndexedLinear<>,
         Component::TypedStatic<DMeta, Byte>,
         Component::CountStack<>
      > {};
      struct PickRangeMut : Container<
         Component::HeapMovable<>,
         Component::Contiguous,
         Component::IndexedLinear<>,
         Component::Assignment,
         Component::TypedStatic<DMeta, Byte>,
         Component::CountStack<>
      > {};
   };

} // namespace Langulus::Anyness
