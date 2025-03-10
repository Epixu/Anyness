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
#include "../../../source/states/Future.hpp"
#include "../../../source/states/Past.hpp"
#include "../../../source/states/Compressed.hpp"
#include "../../../source/states/Encrypted.hpp"
#include "../../../source/states/Tracked.hpp"


namespace Langulus::Anyness
{

   /// A universal type-erased container of size 1                            
   struct Any : Detail::Container<
      Component::HeapMovable<>,        // Pointer to heap memory        
      Component::OwnershipStack<>,     // Allocation is referenced      
      Component::DeepOwnership,        // Sparse elements are referenced
      Component::Assignment,           // Allows assignment             
      Component::TypedStack<DMeta>,    // Variable type                 
      Component::CountStatic<1>,       // Statically sized to 1         
      Component::StateStack<           // Variable state                
         State::Future<>,              // Adds a 'missing future' state 
         State::Past<>,                // Adds a 'missing past' state   
         State::Compressed<>,          // Adds 'compressed' state       
         State::Encrypted<>,           // Adds 'encrypted' state        
         State::Tracked<>              // Adds 'tracked' state          
      >
   > {};
   
   /// A statically typed container of size 1 that is binary compatible with  
   /// the type-erased alternative above                                      
   template<CT::NotVoid T>
   struct TAny : Detail::Container<
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
   
   /// A universal type-erased container view of size 1, that is binary       
   /// compatible with the containers above                                   
   struct AnyView : Detail::Container<
      Component::HeapMovable<>,        // Pointer to heap memory        
      Component::NoOwnershipStack<>,   // Pointer to an allocation      
      Component::TypedStack<DMeta>,    // Variable type                 
      Component::CountStatic<1>,       // Statically sized to 1         
      Component::StateStack<           // Variable state                
         State::Future<>,              // Adds a 'missing future' state 
         State::Past<>,                // Adds a 'missing past' state   
         State::Compressed<>,          // Adds 'compressed' state       
         State::Encrypted<>,           // Adds 'encrypted' state        
         State::Tracked<>              // Adds 'tracked' state          
      >
   > {};
   
   /// A statically typed container of size 1 that is binary compatible with  
   /// the type-erased alternative above                                      
   template<CT::NotVoid T>
   struct TAnyView : Detail::Container<
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
