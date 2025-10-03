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
#include "../../../source/components/Reserve-Emergent.hpp"
#include "../../../source/components/OwnershipDeep-Heap.hpp"
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
#include "Handle.hpp"


namespace Langulus::Anyness::Inner
{
   using AnyBase = Container<
      Com::TypedStack<DMeta>,          // Type-erased                   
      Com::HeapMovable<>,              // Pointer to heap memory        
      Com::OwnershipStack<>,           // Allocation is referenced      
      Com::CountStatic<1u>,            // Statically sized to 1         
      Com::ReserveEmergent<>,          // Reserve derived from alloc    
      Com::OwnershipDeepHeap<>,        // Sparse elements are referenced
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
   /// A universal type-erased container of size 1.                           
   /// This is the most universal and feature-complete container, that        
   /// supports all kinds of data states: compression, encryption, linking,   
   /// and so on. For a slightly smaller and faster representation, consider  
   /// using Own or Ref instead. If you want to contain a number of similar   
   /// elements use Many instead.                                             
   struct Any : Inner::AnyBase {
      using Base::operator ==;
      //using Com::OwnershipDeepHeap<>::DestroyElement;
      using DefineState::Typed<>::IsTypeConstrained;

      using Pick          = Handle;
      using PickMut       = HandleMut;
      using HandleType    = Handle;
      using HandleMutType = HandleMut;
      using DeepType      = Any;

      constexpr Any() noexcept { this->ConstructDefault(); }
      constexpr Any(Any const& other)     : Any {Refer {other}} {}
      constexpr Any(Any&& other) noexcept : Any {Move  {other}} {}
      constexpr ~Any() noexcept { this->Destroy(); }

      /// Construction that emplaces A in the container                       
      template<class A>
      constexpr Any(A&& argument) {
         if constexpr (CT::ContainsOne<A>)
            this->ConstructFrom(FWD(argument));
         else {
            this->SetType<Decvq<Deref<A>>>();
            this->AllocateFresh(this->RequestHeap(1));
            this->ResetState();
            this->EmplaceWithIntent(FWDIntent(argument));
         }
      }

      /// Assignment                                                          
      constexpr Any& operator = (Any const& other) {
         return operator = (Refer {other});
      }
      constexpr Any& operator = (Any&& other) noexcept {
         return operator = (Move {other});
      }
      
      template<class A>
      constexpr Any& operator = (A&& argument) {
         if constexpr (CT::ContainsOne<A>) {
            LglsAssumeUser(not IsDeep(), "Ambiguous use of assignment "
               "- you should use either AssignFrom (if you want to overwrite "
               "the container itself) or Assign (if you want to overwrite the "
               "first item) in order to clearly state your intent. "
               "AssignFrom will be used by default"
            );
            this->AssignFrom(FWD(argument));
         }
         else Com::Assignment<>::operator = (FWD(argument));
         return *this;
      }
   };
}
