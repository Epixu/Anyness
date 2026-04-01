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
#include "../../../source/components/Heap-Reuse.hpp"
#include "../../../source/components/Count-Stack.hpp"
#include "../../../source/components/Reserve-Stack.hpp"
#include "../../../source/components/Ownership-Stack.hpp"
#include "../../../source/components/OwnershipDeep-Heap.hpp"
#include "../../../source/components/Hash-Heap.hpp"
#include "../../../source/components/IndexedHash-Stack.hpp"
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
#include "../../../source/states/Sorted.hpp"
#include "../../../source/states/Compressed.hpp"
#include "../../../source/states/Encrypted.hpp"
#include "../../../source/states/Tracked.hpp"


namespace Langulus::Anyness::Inner
{
   template<State::StateValue SORT>
   using MapBase = Container<
      Com::TypedStack<DMeta, void, false, 0>,  // Type-erased keys      
      Com::TypedStack<DMeta, void, false, 1>,  // Type-erased values    
      Com::HeapMovable<0, 8, 2>,       // Pointer to key & value memory 
      Com::HeapReuse<1, 0>,            // Reuses HeapMovable<0>         
      Com::CountStack<0>,              // Dynamically sized             
      Com::CountReuse<1, 0>,           // Reuses CountStack<0>          
      Com::ReserveStack<0>,            // Reserve kept as member        
      Com::ReserveReuse<1, 0>,         // Reuses ReserveStack<0>        
      Com::IndexedHashStack<0>,        // Indexed by hash table         
      Com::IndexedHashReuse<1, 0>,     // Reuses IndexedHashStack<0>    
      Com::OwnershipStack<0>,          // Allocation is referenced      
      Com::OwnershipReuse<1, 0>,       // Reuses OwnershipStack<0>      
      Com::OwnershipDeepHeap<0>,       // Sparse keys are referenced    
      Com::OwnershipDeepHeap<1>,       // Sparse values are referenced  
      Com::HashHeap<0>,                // Hash can be cached            
      Com::HashReuse<1, 0>,            // Reuses HashHeap<0>            
      Com::Merging<0>,                 // Allows merging keys           
      Com::Insertion<1>,               // Allows inserting values       
      Com::Assignment<1>,              // Allows assignment of values   
      Com::Removal<0>,                 // Allows clear/reset of keys    
      Com::Removal<1>,                 // Allows clear/reset of values  
      Com::Conversion<0>,              // Allows conversions of keys    
      Com::Conversion<1>,              // Allows conversions of values  
      Com::Comparison<0>,              // Allows comparisons of keys    
      Com::Comparison<1>,              // Allows comparisons of values  
      Com::IterationForEach<0>,        // ForEach iteration of keys     
      Com::IterationForEach<1>,        // ForEach iteration of values   
      Com::IterationRange<0>,          // Ranged iteration of keys      
      Com::IterationRange<1>,          // Ranged iteration of values    
      Com::StateStack<                 // Variable state                
         DefineState::Typed<>,         // Can be type-constrained       
         DefineState::Sorted<SORT>,    // Maybe unsorted                
         DefineState::Compressed<>,    // Adds 'compressed' state       
         DefineState::Encrypted<>,     // Adds 'encrypted' state        
         DefineState::Tracked<>        // Adds 'tracked' state          
      >
   >;

   ///                                                                        
   /// A universal type-erased non-contiguous map of variable size.           
   /// Emplacement is disabled for maps, because keys aren't allowed to       
   /// change in-place. This also means that they are only const-iteratable.  
   /// Values, on the other hand, are mutable.                                
   template<State::StateValue SORTED = State::Variable>
   struct Map : MapBase<SORTED> {
      using CTTI_Map      = Yes<>;
      using CTTI_Deep     = Yes<>;
      using CTTI_MapsTo   = Text;

      using Base          = MapBase<SORTED>;
      using DeepType      = Many;

      using DefineState::Typed<>::IsTypeConstrained;
      using DefineState::Typed<>::EnableTypeConstrained;

      constexpr Map() noexcept {
         this->ConstructDefault();
      }
      constexpr Map(Map const& other) {
         this->Absorb(Refer(other));
      }
      constexpr Map(Map&& other) noexcept  {
         this->Absorb(Move(other));
      }
      constexpr ~Map() noexcept {
         this->Destroy();
      }

      /// Construction that either absorbs the provided containers, or        
      /// emplaces all A in the container                                     
      template<class A1, class...AN>
      constexpr Map(A1&& a1, AN&&...an) {
         if constexpr (sizeof...(AN) == 0) {
            if constexpr (CT::Map<A1>) {
               LglsAssumeUser((Same<Deint<A1>, Map>),
                  "Ambiguous use of construction "
                  "- you should use tag-dispatch with first argument either Absorb "
                  "(if you want to overwrite the container itself) or Piecewise "
                  "(if you want to overwrite the first item) in order to clearly "
                  "state your intent. Absorb will be used by default!"
               );
               this->Absorb(LglsFwd(a1));
            }
            else {
               this->ConstructDefault();
               this->Merge(LglsFwd(a1));
            }
         }
         else {
            this->ConstructDefault();
            this->Insert(LglsFwd(a1), LglsFwd(an)...);
         }
      }
      
      /// Construction that absorbs the provided containers                   
      template<class A1, class...AN>
      constexpr Map(Inner::Absorb, A1&& a1, AN&&...an) {
         if constexpr (sizeof...(AN) == 0)
            this->Absorb(LglsFwd(a1));
         else {
            this->ConstructDefault();
            this->Concat(LglsFwd(a1), LglsFwd(an)...);
         }
      }
      
      /// Construction that emplaces all arguments inside                     
      template<class A1, class...AN>
      constexpr Map(Inner::Piecewise, A1&& a1, AN&&...an) {
         this->ConstructDefault();
         this->Merge(LglsFwd(a1), LglsFwd(an)...);
      }
      
      /// Assignment                                                          
      constexpr Map& operator = (Map const& other) {
         return this->AssignAbsorb(Refer(other));
      }
      constexpr Map& operator = (Map&& other) noexcept {
         return this->AssignAbsorb(Move(other));
      }
      
      template<class A>
      constexpr Map& operator = (A&& argument) {
         if constexpr (CT::Map<A>) {
            LglsAssumeUser((Same<Deint<A>, Map>),
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
   using Map         = Inner::Map<State::Variable>;
   using MapSorted   = Inner::Map<State::Enabled>;
   using MapUnsorted = Inner::Map<State::Disabled>;
}

namespace Langulus::CTTI
{
   /// Convert Map -> Text                                                    
   template<Anyness::State::StateValue SORT>
   struct Converter<Anyness::Inner::Map<SORT>, Anyness::Text> {
      static constexpr auto Convert(Anyness::Inner::Map<SORT> const& from) -> Anyness::Text;
   };
}