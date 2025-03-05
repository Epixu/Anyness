///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../../../source/Container.hpp"
#include "../../../source/components/Count-Static.hpp"


namespace Langulus::Anyness
{

   /// A universal type-erased container of size 1                            
   struct Any : Detail::Container<
      Component::Heap,                 // Pointer to heap memory        
      Component::AllocationStack,      // Pointer to an allocation      
      Component::Ownership,            // Allocation is referenced      
      Component::DeepOwnership,        // Sparse elements are referenced
      Component::TypedStack<DMeta>,    // Variable type                 
      Component::CountStatic<1>,       // Statically sized to 1         
      Component::StatefulStack,        // Variable state                
      Component::CanBeFuture,          // Adds a 'missing future' state 
      Component::CanBePast,            // Adds a 'missing past' state   
      Component::Compressible,         // Adds 'compressed' state       
      Component::Encryptable,          // Adds 'encrypted' state        
      Component::Trackable             // Adds 'tracked' state          
   > {};
   
   /// A statically typed container of size 1 that is binary compatible with  
   /// the type-erased alternative above                                      
   template<CT::NotVoid T>
   struct TAny : Detail::Container<
      Component::Heap<T>,              // Pointer to heap memory        
      Component::AllocationStack,      // Pointer to an allocation      
      Component::Ownership,            // Allocation is referenced      
      Component::DeepOwnership,        // Sparse elements are referenced
      Component::TypedStack<DMeta>,    // Variable type                 
      Component::TypedStatic<T>,       // Statically typed              
      Component::CountStatic<1>,       // Statically sized to 1         
      Component::StatefulStack,        // Variable state                
      Component::CanBeFuture,          // Adds a 'missing future' state 
      Component::CanBePast,            // Adds a 'missing past' state   
      Component::Compressible,         // Adds 'compressed' state       
      Component::Encryptable,          // Adds 'encrypted' state        
      Component::Trackable             // Adds 'tracked' state          
   > {};

} // namespace Langulus::Anyness
