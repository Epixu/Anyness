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
#include "../../../source/components/Count-Stack.hpp"
#include "../../../source/components/Reserve-Emergent.hpp"
#include "../../../source/components/OwnershipDeep-Heap.hpp"
#include "../../../source/components/Hash-Stack.hpp"
#include "../../../source/components/Emplacement.hpp"
#include "../../../source/components/IndexedLinear.hpp"
#include "../../../source/components/Insertion.hpp"
#include "../../../source/components/InsertionOperators.hpp"
#include "../../../source/components/Merging.hpp"
#include "../../../source/components/MergingOperators.hpp"
#include "../../../source/components/Assignment.hpp"
#include "../../../source/components/Removal.hpp"
#include "../../../source/components/Conversion.hpp"
#include "../../../source/components/Comparison.hpp"
#include "../../../source/components/Iteration-ForEach.hpp"
#include "../../../source/components/Iteration-Range.hpp"
#include "../../../source/components/State-Stack.hpp"
#include "../../../source/states/Typed.hpp"
#include "../../../source/states/Future.hpp"
#include "../../../source/states/Past.hpp"
#include "../../../source/states/Compressed.hpp"
#include "../../../source/states/Encrypted.hpp"
#include "../../../source/states/Or.hpp"
#include "../../../source/states/Tracked.hpp"
#include "Handle.hpp"


namespace Langulus::Anyness::Inner
{
   using ManyBase = Container<
      Com::TypedStack<DMeta>,          // Type-erased                   
      Com::HeapMovable<>,              // Pointer to heap memory        
      Com::CountStack<>,               // Dynamically sized             
      Com::ReserveEmergent<>,          // Reserve derived from alloc    
      Com::OwnershipStack<>,           // Allocation is referenced      
      Com::OwnershipDeepHeap<>,        // Sparse elements are referenced
      Com::HashStack<>,                // Hash can be cached            
      Com::IndexedLinear<>,            // Indexed directly              
      Com::Insertion<>,                // Allows insertion              
      Com::InsertionOperators<>,       // << and >> insertion           
      Com::Merging<>,                  // Allows merging                
      Com::MergingOperators<>,         // <<= and >>= merging           
      Com::Emplacement<>,              // Allows emplacement            
      Com::Assignment<>,               // Allows assignment             
      Com::Removal<>,                  // Allows clear/reset            
      Com::Conversion,                 // Allows conversions            
      Com::Comparison<>,               // Allows comparisons            
      Com::IterationForEach<>,         // ForEach iteration             
      Com::IterationRange<>,           // Ranged iteration              
      Com::StateStack<                 // Variable state                
         DefineState::Typed<>,         // Can be type-constrained       
         DefineState::Future<>,        // Adds a 'missing future' state 
         DefineState::Past<>,          // Adds a 'missing past' state   
         DefineState::Compressed<>,    // Adds 'compressed' state       
         DefineState::Encrypted<>,     // Adds 'encrypted' state        
         DefineState::Or<>,            // Adds 'or' state               
         DefineState::Tracked<>        // Adds 'tracked' state          
      >
   >;
}

namespace Langulus::Anyness
{
   ///                                                                        
   ///   A universal type-erased contiguous container of variable size        
   ///                                                                        
   ///   This is the most universal and feature-complete container, that      
   /// supports all kinds of data states: branching, compression, encryption, 
   /// linking, and so on. If you want to contain a single element, consider  
   /// using Any instead, for a bit shorter and faster representation.        
   struct Many : Inner::ManyBase {
      using CTTI_Deep     = Yes<>;
      using CTTI_MapsTo   = Text;

      using Base          = Inner::ManyBase;
      using Pick          = Handle;
      using PickMut       = HandleMut;
      using HandleType    = Handle;
      using HandleMutType = HandleMut;
      using DeepType      = Many;

      using DefineState::Typed<>::IsTypeConstrained;
      using DefineState::Typed<>::EnableTypeConstrained;

      constexpr Many() noexcept {
         this->ConstructDefault();
      }
      constexpr Many(Many const& other) {
         this->Absorb(Refer(other));
      }
      constexpr Many(Many&& other) noexcept  {
         this->Absorb(Move(other));
      }
      constexpr ~Many() noexcept {
         this->Destroy();
      }

      /// Construction that either absorbs the provided containers, or        
      /// emplaces all A in the container                                     
      template<class A1, class...AN>
      constexpr Many(A1&& a1, AN&&...an) {
         if constexpr (sizeof...(AN) == 0) {
            if constexpr (CT::Deep<Deint<A1>> and CT::Dense<Deint<A1>>) {
               LglsAssumeUser((Same<Deint<A1>, Many>),
                  "Ambiguous use of construction "
                  "- you should use tag-dispatch with first argument either Absorb "
                  "(if you want to overwrite the container itself) or Piecewise "
                  "(if you want to overwrite the first item) in order to clearly "
                  "state your intent. Absorb will be used by default!"
               );
               this->Absorb(LglsFwd(a1));
            }
            else this->EmplaceConstruct(LglsFwd(a1));
         }
         else this->Insert(LglsFwd(a1), LglsFwd(an)...);
      }
      
      /// Construction that absorbs the provided containers                   
      template<class A1, class...AN>
      constexpr Many(Inner::Absorb, A1&& a1, AN&&...an) {
         if constexpr (sizeof...(AN) == 0)
            this->Absorb(LglsFwd(a1));
         else
            this->Concat(LglsFwd(a1), LglsFwd(an)...);
      }
      
      /// Construction that emplaces all arguments inside                     
      template<class A1, class...AN>
      constexpr Many(Inner::Piecewise, A1&& a1, AN&&...an) {
         if constexpr (sizeof...(AN) == 0)
            this->EmplaceConstruct(LglsFwd(a1));
         else
            this->Insert(LglsFwd(a1), LglsFwd(an)...);
      }
      
      /// Assignment                                                          
      constexpr Many& operator = (Many const& other) {
         return this->AssignAbsorb(Refer(other));
      }
      constexpr Many& operator = (Many&& other) noexcept {
         return this->AssignAbsorb(Move(other));
      }
      
      template<class A>
      constexpr Many& operator = (A&& argument) {
         if constexpr (CT::Deep<Deint<A>> and CT::Dense<Deint<A>>) {
            LglsAssumeUser((Same<Deint<A>, Many>),
               "Ambiguous use of assignment "
               "- you should use either AssignAbsorb (if you want to overwrite "
               "the container itself) or Assign (if you want to overwrite the "
               "first item) in order to clearly state your intent. "
               "AssignAbsorb will be used by default!"
            );
            return this->AssignAbsorb(LglsFwd(argument));
         }
         else return this->Assign(LglsFwd(argument));
      }

      using Com::Comparison<>::operator <=>;
      using Com::Comparison<>::operator ==;
   };
}

namespace Langulus::CTTI
{
   /// Convert Many -> Text                                                   
   template<>
   struct Converter<Anyness::Many, Anyness::Text> {
      static constexpr auto Convert(Anyness::Many const& from) -> Anyness::Text;
   };
}