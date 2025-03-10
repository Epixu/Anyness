///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Any.hpp"


namespace Langulus::Anyness
{

   /// A statically typed container of size 1 that is binary compatible with  
   /// the type-erased alternative above                                      
   template<CT::NotVoid T>
   struct TAny : Container<
      Component::HeapMovable<>,        // Pointer to heap memory        
      Component::OwnershipStack<>,     // Allocation is referenced      
      Component::DeepOwnership,        // Sparse elements are referenced
      Component::Assignment,           // Allows assignment             
      Component::TypedStack<DMeta, T>, // Type-constrained              
      Component::CountStatic<1>,       // Statically sized to 1         
      Component::StateStack<           // Variable state                
         State::Future<>,              // Adds a 'missing future' state 
         State::Past<>,                // Adds a 'missing past' state   
         State::Compressed<>,          // Adds 'compressed' state       
         State::Encrypted<>,           // Adds 'encrypted' state        
         State::Tracked<>              // Adds 'tracked' state          
      >
   > {
      using CTTI_ReflectAs = Any;
   };
   
   /// A statically typed container of size 1 that is binary compatible with  
   /// the type-erased alternative above                                      
   template<CT::NotVoid T>
   struct TAnyView : Container<
      Component::HeapMovable<>,        // Pointer to heap memory        
      Component::NoOwnershipStack<>,   // Pointer to an allocation      
      Component::TypedStack<DMeta, T>, // Type-constrained              
      Component::CountStatic<1>,       // Statically sized to 1         
      Component::StateStack<           // Variable state                
         State::Future<>,              // Adds a 'missing future' state 
         State::Past<>,                // Adds a 'missing past' state   
         State::Compressed<>,          // Adds 'compressed' state       
         State::Encrypted<>,           // Adds 'encrypted' state        
         State::Tracked<>              // Adds 'tracked' state          
      >
   > {
      using CTTI_ReflectAs = AnyView;
   };

} // namespace Langulus::Anyness
