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
#include "../../../source/components/Count-Stack.hpp"
#include "../../../source/components/Reserve-Stack.hpp"
#include "../../../source/components/Ownership-Stack.hpp"
#include "../../../source/components/OwnershipDeep-Heap.hpp"
#include "../../../source/components/Hash-Heap.hpp"
#include "../../../source/components/IndexedHash-Heap.hpp"
#include "../../../source/components/Insertion.hpp"
#include "../../../source/components/InsertionOperators.hpp"
#include "../../../source/components/Emplacement.hpp"
#include "../../../source/components/Removal.hpp"
#include "../../../source/components/Conversion.hpp"
#include "../../../source/components/Comparison.hpp"
#include "../../../source/components/Iteration-ForEach.hpp"
#include "../../../source/components/Iteration-Range.hpp"
#include "../../../source/components/State-Stack.hpp"
#include "../../../source/states/Sorted.hpp"
#include "../../../source/states/Compressed.hpp"
#include "../../../source/states/Encrypted.hpp"
#include "../../../source/states/Tracked.hpp"


namespace Langulus::Anyness::Inner
{
   template<State::StateValue SORT>
   using SetBase = Container<
      Com::TypedStack<DMeta>,          // Type-erased                   
      Com::HeapMovable<>,              // Pointer to heap memory        
      Com::CountStack<>,               // Dynamically sized             
      Com::ReserveEmergent<>,          // Reserve derived from alloc    
      Com::OwnershipStack<>,           // Allocation is referenced      
      Com::OwnershipDeepHeap<>,        // Sparse elements are referenced
      Com::HashHeap<>,                 // Hash can be cached            
      Com::IndexedHashHeap<>,          // Indexed by hash table         
      Com::Insertion<>,                // Allows insertion              
      Com::InsertionOperators<>,       // << and >> insertion           
      Com::Emplacement<>,              // Allows emplacement            
      Com::Assignment<>,               // Allows assignment             
      Com::Removal<>,                  // Allows clear/reset            
      Com::Conversion,                 // Allows conversions            
      Com::Comparison<>,               // Allows comparisons            
      Com::IterationForEach<>,         // ForEach iteration             
      Com::IterationRange<>,           // Ranged iteration              
      Com::StateStack<                 // Variable state                
         DefineState::Typed<>,         // Can be type-constrained       
         DefineState::Sorted<SORT>,    // Maybe unsorted                
         DefineState::Compressed<>,    // Adds 'compressed' state       
         DefineState::Encrypted<>,     // Adds 'encrypted' state        
         DefineState::Tracked<>        // Adds 'tracked' state          
      >
   >;

   ///                                                                        
   /// A universal type-erased non-contiguous set of variable size            
   ///                                                                        
   template<State::StateValue SORTED = State::Variable>
   struct Set : SetBase<SORTED> {
      using CTTI_Set      = Yes<>;
      using CTTI_Deep     = Yes<>;
      using CTTI_MapsTo   = Text;

      using Base          = SetBase<SORTED>;
      using Pick          = Handle;
      using PickMut       = HandleMut;
      using HandleType    = Handle;
      using HandleMutType = HandleMut;
      using DeepType      = Many;

      using DefineState::Typed<>::IsTypeConstrained;
      using DefineState::Typed<>::EnableTypeConstrained;

      constexpr Set() noexcept {
         this->ConstructDefault();
      }
      constexpr Set(Set const& other) {
         this->Absorb(Refer(other));
      }
      constexpr Set(Set&& other) noexcept  {
         this->Absorb(Move(other));
      }
      constexpr ~Set() noexcept {
         this->Destroy();
      }

      /// Construction that either absorbs the provided containers, or        
      /// emplaces all A in the container                                     
      template<class A1, class...AN>
      constexpr Set(A1&& a1, AN&&...an) {
         if constexpr (sizeof...(AN) == 0) {
            if constexpr (CT::Deep<Deint<A1>> and CT::Dense<Deint<A1>>) {
               LglsAssumeUser((Same<Deint<A1>, Set>),
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
         else {
            this->ConstructDefault();
            this->Insert(LglsFwd(a1), LglsFwd(an)...);
         }
      }
      
      /// Construction that absorbs the provided containers                   
      template<class A1, class...AN>
      constexpr Set(Inner::Absorb, A1&& a1, AN&&...an) {
         if constexpr (sizeof...(AN) == 0)
            this->Absorb(LglsFwd(a1));
         else {
            this->ConstructDefault();
            this->Concat(LglsFwd(a1), LglsFwd(an)...);
         }
      }
      
      /// Construction that emplaces all arguments inside                     
      template<class A1, class...AN>
      constexpr Set(Inner::Piecewise, A1&& a1, AN&&...an) {
         if constexpr (sizeof...(AN) == 0)
            this->EmplaceConstruct(LglsFwd(a1));
         else {
            this->ConstructDefault();
            this->Insert(LglsFwd(a1), LglsFwd(an)...);
         }
      }
      
      /// Assignment                                                          
      constexpr Set& operator = (Set const& other) {
         return this->AssignAbsorb(Refer(other));
      }
      constexpr Set& operator = (Set&& other) noexcept {
         return this->AssignAbsorb(Move(other));
      }
      
      template<class A>
      constexpr Set& operator = (A&& argument) {
         if constexpr (CT::Deep<Deint<A>> and CT::Dense<Deint<A>>) {
            LglsAssumeUser((Same<Deint<A>, Set>),
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

namespace Langulus::Anyness
{
   using Set         = Inner::Set<State::Variable>;
   using SetSorted   = Inner::Set<State::Enabled>;
   using SetUnsorted = Inner::Set<State::Disabled>;
}

namespace Langulus::CTTI
{
   /// Convert Set -> Text                                                    
   template<Anyness::State::StateValue SORT>
   struct Converter<Anyness::Inner::Set<SORT>, Anyness::Text> {
      static constexpr auto Convert(Anyness::Inner::Set<SORT> const& from) -> Anyness::Text;
   };
}