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
#include "../../../source/components/Assignment.hpp"
#include "../../../source/components/Typed-Stack.hpp"
#include "../../../source/components/Count-Static.hpp"
#include "../../../source/components/State-Stack.hpp"
#include "../../../source/components/Conversion.hpp"
#include "../../../source/states/Future.hpp"
#include "../../../source/states/Past.hpp"
#include "../../../source/states/Compressed.hpp"
#include "../../../source/states/Encrypted.hpp"
#include "../../../source/states/Tracked.hpp"
#include "../../../source/states/Typed.hpp"


namespace Langulus::Anyness
{

   /// A universal type-erased container of size 1                            
   struct Any : Container<
      Component::HeapMovable<>,        // Pointer to heap memory        
      Component::OwnershipStack<>,     // Allocation is referenced      
      Component::DeepOwnership<>,      // Sparse elements are referenced
      Component::Assignment,           // Allows assignment             
      Component::Conversion,           // Allows conversion             
      Component::TypedStack<DMeta>,    // Variable type                 
      Component::CountStatic<1>,       // Statically sized to 1         
      Component::StateStack<           // Variable state                
         DefineState::Typed<>,         // Can be type-constrained       
         DefineState::Future<>,        // Adds a 'missing future' state 
         DefineState::Past<>,          // Adds a 'missing past' state   
         DefineState::Compressed<>,    // Adds 'compressed' state       
         DefineState::Encrypted<>,     // Adds 'encrypted' state        
         DefineState::Tracked<>        // Adds 'tracked' state          
      >
   > {};
      
   /// A universal type-erased container view of size 1, that is binary       
   /// compatible with the containers above                                   
   struct AnyView : Container<
      Component::HeapMovable<>,        // Pointer to heap memory        
      Component::OwnershipStack<0, false>,   // Pointer to an allocation
      Component::Conversion,           // Allows conversion             
      Component::TypedStack<DMeta>,    // Variable type                 
      Component::CountStatic<1>,       // Statically sized to 1         
      Component::StateStack<           // Variable state                
         DefineState::Typed<>,         // Can be type-constrained       
         DefineState::Future<>,        // Adds a 'missing future' state 
         DefineState::Past<>,          // Adds a 'missing past' state   
         DefineState::Compressed<>,    // Adds 'compressed' state       
         DefineState::Encrypted<>,     // Adds 'encrypted' state        
         DefineState::Tracked<>        // Adds 'tracked' state          
      >
   > {};
   
} // namespace Langulus::Anyness
