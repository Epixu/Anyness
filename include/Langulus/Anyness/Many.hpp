///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../../../source/Container.hpp"
#include "../../../source/components/Heap.hpp"
#include "../../../source/components/Allocation-Stack.hpp"
#include "../../../source/components/Count-Stack.hpp"
#include "../../../source/components/Ownership.hpp"
#include "../../../source/components/DeepOwnership.hpp"
#include "../../../source/components/Continuous.hpp"
#include "../../../source/components/Typed-Stack.hpp"
#include "../../../source/components/Typed-Static.hpp"
#include "../../../source/components/Capacity-Stack.hpp"
#include "../../../source/components/Hash-Stack.hpp"
#include "../../../source/components/Descriptor.hpp"
#include "../../../source/components/State-Stack.hpp"
#include "../../../source/states/Future.hpp"
#include "../../../source/states/Past.hpp"
#include "../../../source/states/Compressed.hpp"
#include "../../../source/states/Encrypted.hpp"
#include "../../../source/states/Or.hpp"
#include "../../../source/states/Tracked.hpp"


namespace Langulus::Anyness
{

   using DMeta = RTTI::DMeta;

   /// A universal type-erased continuous container of variable size          
   struct Many : Detail::Container<
      Component::Heap<>,               // Pointer to heap memory        
      Component::AllocationStack<>,    // Pointer to an allocation      
      Component::Ownership,            // Allocation is referenced      
      Component::DeepOwnership,        // Referenced indirections       
      Component::Continuous,           // Heap memory is continuous     
      Component::TypedStack<DMeta>,    // Variable type                 
      Component::CountStack<>,         // Variable count                
      Component::CapacityStack<>,      // Variable capacity             
      Component::HashStack<>,          // Variable hash (cached)        
      Component::Descriptor,           // Descriptor interface          
      Component::StateStack<           // Variable state                
         State::Future<>,              // Adds a 'missing future' state 
         State::Past<>,                // Adds a 'missing past' state   
         State::Compressed<>,          // Adds 'compressed' state       
         State::Encrypted<>,           // Adds 'encrypted' state        
         State::Or<>,                  // Adds 'or' state               
         State::Tracked<>              // Adds 'tracked' state          
      >
   > {};
   
   /// A statically-typed continuous container of variable size that is       
   /// binary-compatible with the type-erased alternative above               
   template<CT::NotVoid T>
   struct TMany : Detail::Container<
      Component::Heap<T>,              // Pointer to heap memory        
      Component::AllocationStack<>,    // Pointer to an allocation      
      Component::Ownership,            // Allocation is referenced      
      Component::DeepOwnership,        // Referenced indirections       
      Component::Continuous,           // Heap memory is continuous     
      Component::TypedStack<DMeta>,    // Variable type                 
      Component::TypedStatic<T>,       // Statically typed              
      Component::CountStack<>,         // Variable count                
      Component::CapacityStack<>,      // Variable capacity             
      Component::HashStack<>,          // Variable hash (cached)        
      Component::Descriptor,           // Descriptor interface          
      Component::StateStack<           // Variable state                
         State::Future<>,              // Adds a 'missing future' state 
         State::Past<>,                // Adds a 'missing past' state   
         State::Compressed<>,          // Adds 'compressed' state       
         State::Encrypted<>,           // Adds 'encrypted' state        
         State::Or<>,                  // Adds 'or' state               
         State::Tracked<>              // Adds 'tracked' state          
      >
   > {};
   
   /// A universal type-erased continuous container view of variable size     
   /// Doesn't have ownership, and binary-compatible with the container above 
   struct ManyView : Detail::Container<
      Component::Heap<>,               // Pointer to heap memory        
      Component::AllocationStack<>,    // Pointer to an allocation      
      Component::Continuous,           // Heap memory is continuous     
      Component::TypedStack<DMeta>,    // Variable type                 
      Component::CountStack<>,         // Variable count                
      Component::CapacityStack<>,      // Variable capacity             
      Component::HashStack<>,          // Variable hash (cached)        
      Component::Descriptor,           // Descriptor interface          
      Component::StateStack<           // Variable state                
         State::Future<>,              // Adds a 'missing future' state 
         State::Past<>,                // Adds a 'missing past' state   
         State::Compressed<>,          // Adds 'compressed' state       
         State::Encrypted<>,           // Adds 'encrypted' state        
         State::Or<>,                  // Adds 'or' state               
         State::Tracked<>              // Adds 'tracked' state          
      >
   > {};
   
   /// A statically-typed continuous container view of variable size          
   /// Doesn't have ownership, and binary-compatible with the container above 
   template<CT::NotVoid T>
   struct TManyView : Detail::Container<
      Component::Heap<T>,              // Pointer to heap memory        
      Component::AllocationStack<>,    // Pointer to an allocation      
      Component::Continuous,           // Heap memory is continuous     
      Component::TypedStack<DMeta>,    // Variable type                 
      Component::TypedStatic<T>,       // Statically typed              
      Component::CountStack<>,         // Variable count                
      Component::CapacityStack<>,      // Variable capacity             
      Component::HashStack<>,          // Variable hash (cached)        
      Component::Descriptor,           // Descriptor interface          
      Component::StateStack<           // Variable state                
         State::Future<>,              // Adds a 'missing future' state 
         State::Past<>,                // Adds a 'missing past' state   
         State::Compressed<>,          // Adds 'compressed' state       
         State::Encrypted<>,           // Adds 'encrypted' state        
         State::Or<>,                  // Adds 'or' state               
         State::Tracked<>              // Adds 'tracked' state          
      >
   > {};

} // namespace Langulus::Anyness::Detail
