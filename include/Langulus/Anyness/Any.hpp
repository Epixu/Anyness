///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../../../source/Container.hpp"
#include "../../../source/components/Typed-Stack.hpp"
#include "../../../source/components/Heap-Movable.hpp"
#include "../../../source/components/Ownership-Stack.hpp"
#include "../../../source/components/Count-Static.hpp"
#include "../../../source/components/DeepOwnership-Heap.hpp"
#include "../../../source/components/Hash-Emergent.hpp"
#include "../../../source/components/Emplacement.hpp"
#include "../../../source/components/Assignment.hpp"
#include "../../../source/components/Removal.hpp"
#include "../../../source/components/Conversion.hpp"
#include "../../../source/components/State-Stack.hpp"
#include "../../../source/states/Typed.hpp"
#include "../../../source/states/Future.hpp"
#include "../../../source/states/Past.hpp"
#include "../../../source/states/Compressed.hpp"
#include "../../../source/states/Encrypted.hpp"
#include "../../../source/states/Tracked.hpp"


namespace Langulus::Anyness::Inner
{
   using AnyBase = Container<
      Com::TypedStack<DMeta>,          // Type-erased                   
      Com::HeapMovable<>,              // Pointer to heap memory        
      Com::OwnershipStack<>,           // Allocation is referenced      
      Com::CountStatic<1u>,            // Statically sized to 1         
      Com::DeepOwnershipHeap<>,        // Sparse elements are referenced
      Com::HashEmergent<>,             // Hash is retrieved from item   
      Com::Emplacement<>,              // Allows emplacement            
      Com::Assignment<>,               // Allows assignment             
      Com::Removal<>,                  // Allows clear/reset            
      Com::Comparison<>,               // Allows comparisons            
      Com::StateStack<                 // Variable state                
         DefineState::Typed<>,         // Can be type-constrained       
         DefineState::Future<>,        // Adds a 'missing future' state 
         DefineState::Past<>,          // Adds a 'missing past' state   
         DefineState::Compressed<>,    // Adds 'compressed' state       
         DefineState::Encrypted<>,     // Adds 'encrypted' state        
         DefineState::Tracked<>        // Adds 'tracked' state          
      >
   >;
}

namespace Langulus::Anyness
{
   ///                                                                        
   /// A universal type-erased container of size 1                            
   /// This is the most universal and feature-complete container, that        
   /// supports all kinds of data states: compression, encryption, linking,   
   /// and so on. For a slightly smaller and faster representation, consider  
   /// using Own or Ref instead. If you want to contain a number of similar   
   /// elements use Many instead.                                             
   struct Any : Inner::AnyBase {
      using Base = Inner::AnyBase;
      //using Base::Base;
      using Base::operator =;
      using Com::Assignment<>::operator =;
      using Base::operator ==;

      // Single element selections                                      
      using Pick    = Handle;
      using PickMut = HandleMut;

      /// Construction that emplaces A in the container                       
      template<class A>
      constexpr Any(A&& argument) {
         if constexpr (CT::ContainsOne<A>)
            Base::ConstructFrom(FWD(argument));
         else {
            this->GetType();
            this->AllocateFresh(this->RequestSize(1));
            this->EmplaceWithIntent(IntentOf<A&&> {FWD(argument)});
         }
      }
   };  
}
