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
#include "../../../source/components/Count-Stack.hpp"
#include "../../../source/components/Continuous.hpp"
#include "../../../source/components/Indexed-Static.hpp"
#include "../../../source/components/Typed-Static.hpp"
#include "../../../source/components/Typed-Stack.hpp"
#include "../../../source/components/Capacity-Stack.hpp"
#include "../../../source/components/Hash-Stack.hpp"
#include "../../../source/components/Descriptor.hpp"
#include "../../../source/components/State-Stack.hpp"
#include "../../../source/components/Insertion.hpp"
#include "../../../source/components/Emplacement.hpp"
#include "../../../source/components/InsertionOperators.hpp"
#include "../../../source/components/Removal.hpp"
#include "../../../source/components/Assignment.hpp"
#include "../../../source/states/Future.hpp"
#include "../../../source/states/Past.hpp"
#include "../../../source/states/Compressed.hpp"
#include "../../../source/states/Encrypted.hpp"
#include "../../../source/states/Or.hpp"
#include "../../../source/states/Tracked.hpp"
#include "../../../source/rtti/MetaData.hpp"


namespace Langulus::Anyness
{

   using DMeta = RTTI::DMeta;

   /// A universal type-erased continuous container of variable size          
   struct Many : Detail::Container<
      Component::HeapMovable<>,        // Pointer to heap memory        
      Component::OwnershipStack<>,     // Allocation is referenced      
      Component::DeepOwnership,        // Referenced indirections       
      Component::Continuous,           // Heap memory is continuous     
      Component::IndexedStatic<>,      // Indexed directly              
      Component::Insertion,            // Allows insertion              
      Component::Emplacement,          // Allows emplacement            
      Component::InsertionOperators,   // << and >> insertion           
      Component::Removal,              // Allows removal                
      Component::Assignment,           // Allows assignment             
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
      Component::HeapMovable<>,        // Pointer to heap memory        
      Component::OwnershipStack<>,     // Allocation is referenced      
      Component::DeepOwnership,        // Referenced indirections       
      Component::Continuous,           // Heap memory is continuous     
      Component::IndexedStatic<>,      // Indexed directly              
      Component::Insertion,            // Allows insertion              
      Component::Emplacement,          // Allows emplacement            
      Component::InsertionOperators,   // << and >> insertion           
      Component::Removal,              // Allows removal                
      Component::Assignment,           // Allows assignment             
      Component::TypedStack<DMeta, T>, // Type-constrained              
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
   > {
      using CTTI_ReflectAs = Many;

      // Single element selections                                      
      using  PickDenseMut  = T&;
      using  PickDense     = T const&;
      struct PickSparseMut : Detail::Container<
         Component::HeapMovable<>,
         Component::OwnershipStack<>,
         Component::Assignment,
         Component::TypedStatic<DMeta, T>
      > {};
      using  PickSparse = T;
      using  Pick       = ::std::conditional_t<CT::Sparse<T>, PickSparse,    PickDense>;
      using  PickMut    = ::std::conditional_t<CT::Sparse<T>, PickSparseMut, PickDenseMut>;

      // Range selections                                               
      struct PickRangeDenseMut : Detail::Container<
         Component::HeapMovable<>,
         Component::Continuous,
         Component::IndexedStatic<>,
         Component::Assignment,
         Component::TypedStatic<DMeta, T>,
         Component::CountStack<>
      > {};
      using  PickRangeDense = PickRangeDenseMut;
      struct PickRangeSparseMut : Detail::Container<
         Component::HeapMovable<>,
         Component::NoOwnershipStack<>,
         Component::DeepOwnership,
         Component::Continuous,
         Component::IndexedStatic<>,
         Component::Assignment,
         Component::TypedStatic<DMeta, T>,
         Component::CountStack<>,
         Component::CapacityStack<>
      > {};
      using  PickRangeSparse = PickRangeSparseMut;
      using  PickRange       = ::std::conditional_t<CT::Sparse<T>, PickRangeSparse,    PickRangeDense>;
      using  PickRangeMut    = ::std::conditional_t<CT::Sparse<T>, PickRangeSparseMut, PickRangeDenseMut>;
   };
   
   /// A universal type-erased continuous container view of variable size     
   /// Doesn't have ownership, and binary-compatible with the container above 
   struct ManyView : Detail::Container<
      Component::HeapMovable<>,        // Pointer to heap memory        
      Component::NoOwnershipStack<>,   // Pointer to an allocation      
      Component::Continuous,           // Heap memory is continuous     
      Component::IndexedStatic<>,      // Indexed directly              
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
      Component::HeapMovable<>,        // Pointer to heap memory        
      Component::NoOwnershipStack<>,   // Pointer to an allocation      
      Component::Continuous,           // Heap memory is continuous     
      Component::IndexedStatic<>,      // Indexed directly              
      Component::TypedStack<DMeta, T>, // Type-constrained              
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
   > {
      using CTTI_ReflectAs = ManyView;
   };

} // namespace Langulus::Anyness
