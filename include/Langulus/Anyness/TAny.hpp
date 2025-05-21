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
      Com::HeapMovable<>,              // Pointer to heap memory        
      Com::OwnershipStack<>,           // Allocation is referenced      
      Com::DeepOwnership,              // Sparse elements are referenced
      Com::Assignment,                 // Allows assignment             
      Com::TypedStack<DMeta, T>,       // Type-constrained              
      Com::CountStatic<1>,             // Statically sized to 1         
      Com::StateStack<                 // Variable state                
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
      Com::HeapMovable<>,              // Pointer to heap memory        
      Com::NoOwnershipStack<>,         // Pointer to an allocation      
      Com::TypedStack<DMeta, T>,       // Type-constrained              
      Com::CountStatic<1>,             // Statically sized to 1         
      Com::StateStack<                 // Variable state                
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
