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
   /// A type-erased container of size 1.                                     
   /// This is the most universal and feature-complete container, that        
   /// supports all kinds of data states: compression, encryption, linking,   
   /// and so on. For a slightly smaller and faster representation, consider  
   /// using Own or Ref instead. If you want to contain a number of similar   
   /// elements use Many instead.                                             
   struct Any : Inner::AnyBase {
      using CTTI_Deep   = Yes<>;
      using CTTI_MapsTo = Text;

      using Base = Inner::AnyBase;
      using DefineState::Typed<>::IsTypeConstrained;
      using DefineState::Typed<>::EnableTypeConstrained;

      using Pick          = Handle;
      using PickMut       = HandleMut;
      using HandleType    = Handle;
      using HandleMutType = HandleMut;
      using DeepType      = Any;

      constexpr Any() noexcept {
         this->ConstructDefault();
      }
      constexpr Any(Any const& other) {
         this->ConstructFrom(Refer(other));
      }
      constexpr Any(Any&& other) noexcept  {
         this->ConstructFrom(Move(other));
      }
      constexpr ~Any() noexcept {
         this->Destroy();
      }

      /// Construction that either absorbs the provided container, or         
      /// emplaces A in the container                                         
      template<class A>
      constexpr Any(A&& argument) {
         if constexpr (CT::ContainsOne<A>) {
            LglsAssumeUser((Same<Deint<A>, Any>),
               "Ambiguous use of construction "
               "- you should use tag-dispatch with first argument either Absorb "
               "(if you want to overwrite the container itself) or Piecewise "
               "(if you want to overwrite the first item) in order to clearly "
               "state your intent. Absorb will be used by default!"
            );
            this->ConstructFrom(FWD(argument));
         }
         else {
            this->SetType<Decvq<Deref<Deint<A>>>>();
            this->AllocateFresh(this->RequestHeap(1));
            this->ResetState();
            this->EmplaceWithIntent(FWDIntent(argument));
         }
      }
      
      /// Construction that absorbs the provided container                    
      template<class A>
      constexpr Any(Inner::Absorb, A&& argument) {
         this->ConstructFrom(FWD(argument));
      }
      
      /// Construction that emplaces A inside                                 
      template<class A>
      constexpr Any(Inner::Piecewise, A&& argument) {
         this->SetType<Decvq<Deref<Deint<A>>>>();
         this->AllocateFresh(this->RequestHeap(1));
         this->ResetState();
         this->EmplaceWithIntent(FWDIntent(argument));
      }
      
      /// Assignment                                                          
      constexpr Any& operator = (Any const& other) {
         return this->AssignFrom(Refer(other));
      }
      constexpr Any& operator = (Any&& other) noexcept {
         return this->AssignFrom(Move(other));
      }
      
      template<class A>
      constexpr Any& operator = (A&& argument) {
         if constexpr (CT::ContainsOne<A>) {
            LglsAssumeUser((Same<Deint<A>, Any>),
               "Ambiguous use of assignment "
               "- you should use either AssignFrom (if you want to overwrite "
               "the container itself) or Assign (if you want to overwrite the "
               "first item) in order to clearly state your intent. "
               "AssignFrom will be used by default!"
            );
            return this->AssignFrom(FWD(argument));
         }
         else return this->Assign(FWD(argument));
      }

      /// Three-way comparison                                                
      constexpr Compared operator <=> (Any const& other) const noexcept {
         return this->Compare(other);
      }

      template<class A>
      constexpr Compared operator <=> (const A& argument) const has_assumptions {
         if constexpr (CT::ContainsOne<A>) {
            LglsAssumeUser((Same<Deint<A>, Any>),
               "Ambiguous use of three-way comparison "
               "- you should use either Compare (if you want to compare "
               "containers) or CompareOne (if you want to compare the "
               "first item) in order to clearly state your intent. "
               "Compare will be used by default!"
            );
            return this->Compare(argument);
         }
         else return this->CompareOne(argument);
      }

      /// Equality comparison                                                 
      constexpr bool operator == (Any const& other) const noexcept {
         return this->CompareEqual(other);
      }

      template<class A>
      constexpr bool operator == (const A& argument) const has_assumptions {
         if constexpr (CT::ContainsOne<A>) {
            LglsAssumeUser((Same<Deint<A>, Any>),
               "Ambiguous use of equality comparison "
               "- you should use either CompareEqual (if you want to compare "
               "containers) or CompareOneEqual (if you want to compare the "
               "first item) in order to clearly state your intent. "
               "Compare will be used by default!"
            );
            return this->CompareEqual(argument);
         }
         else return this->CompareOneEqual(argument);
      }
   };
}

namespace Langulus::CTTI
{
   /// Convert Any -> Text                                                    
   template<>
   struct Converter<Anyness::Any, Anyness::Text> {
      static constexpr void Convert(Anyness::Any const& from, Anyness::Text& to);
      static constexpr auto Convert(Anyness::Any const& from) -> Anyness::Text;
   };
}
