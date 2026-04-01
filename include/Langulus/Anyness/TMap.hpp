///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Map.hpp"


namespace Langulus::Anyness::Inner
{
   template<CT::NotVoid K, CT::NotVoid V, State::StateValue SORT>
   using TMapBase = Container<
      Com::TypedStack<DMeta, K, true, 0>,  // Type-constrained keys     
      Com::TypedStack<DMeta, V, true, 1>,  // Type-constrained values   
      Com::HeapMovable<0, 8, 2, K*>,   // Pointer to key & value memory 
      Com::HeapReuse<1, 0, V*>,        // Reuses HeapMovable<0>         
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
         DefineState::Typed<State::Enabled>, // Always type-constrained 
         DefineState::Sorted<SORT>,    // Maybe unsorted                
         DefineState::Compressed<>,    // Adds 'compressed' state       
         DefineState::Encrypted<>,     // Adds 'encrypted' state        
         DefineState::Tracked<>        // Adds 'tracked' state          
      >
   >;
}

namespace Langulus::Anyness
{
   ///                                                                        
   /// A statically-typed non-contiguous map of variable size that is         
   /// binary-compatible with the type-erased alternative `Map`.              
   /// Emplacement is disabled for maps, because keys aren't allowed to       
   /// change in-place. This also means that they are only const-iteratable.  
   /// Values, on the other hand, are mutable.                                
   template<CT::NotVoid K, CT::NotVoid V, State::StateValue SORT = State::Variable>
   struct TMap : Inner::TMapBase<K, V, SORT> {
      using CTTI_Map       = Yes<>;
      using CTTI_ReflectAs = Map;
      using CTTI_Deep      = Yes<>;
      using CTTI_MapsTo    = Text;

      using Base           = Inner::TMapBase<K, V, SORT>;
      using DeepType       = Any;

      constexpr TMap() noexcept {
         this->ConstructDefault();
      }
      constexpr TMap(TMap const& other) {
         this->Absorb(Refer(other));
      }
      constexpr TMap(TMap&& other) noexcept {
         this->Absorb(Move(other));
      }
      constexpr ~TMap() noexcept {
         this->Destroy();
      }
      
      /// Construction that either absorbs the provided containers, or        
      /// emplaces all A in the container                                     
      template<class A1, class...AN>
      constexpr TMap(A1&& a1, AN&&...an) {
         if constexpr (sizeof...(AN) == 0) {
            if constexpr (SameAsOneOf<Deint<A1>, 
               TMap<K, V, State::Variable>, TMap<K, V, State::Enabled>, TMap<K, V, State::Disabled>,
               Map, MapSorted, MapUnsorted>
            ) {
               LglsAssumeUser((not SameAsOneOf<K,
                  TMap<K, V, State::Variable>, TMap<K, V, State::Enabled>, TMap<K, V, State::Disabled>,
                  Map, MapSorted, MapUnsorted>),
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
      
      /// Construction that absorbs the provided container                    
      template<class A1, class...AN>
      constexpr TMap(Inner::Absorb, A1&& a1, AN&&...an) {
         if constexpr (sizeof...(AN) == 0)
            this->Absorb(LglsFwd(a1));
         else {
            this->ConstructDefault();
            this->Concat(LglsFwd(a1), LglsFwd(an)...);
         }
      }
      
      /// Construction that emplaces all arguments inside                     
      template<class A1, class...AN>
      constexpr TMap(Inner::Piecewise, A1&& a1, AN&&...an) {
         this->ConstructDefault();
         this->Merge(LglsFwd(a1), LglsFwd(an)...);
      }

      /// Assignment                                                          
      constexpr TMap& operator = (TMap const& other) {
         return this->AssignAbsorb(Refer(other));
      }
      constexpr TMap& operator = (TMap&& other) noexcept {
         return this->AssignAbsorb(Move(other));
      }

      template<class A>
      constexpr TMap& operator = (A&& argument) {
         if constexpr (SameAsOneOf<Deint<A>,
            TMap<T, State::Variable>, TMap<T, State::Enabled>, TMap<T, State::Disabled>,
            Map, MapSorted, MapUnsorted>
         ) {
            LglsAssumeUser((not SameAsOneOf<K,
               TMap<K, V, State::Variable>, TMap<K, V, State::Enabled>, TMap<T, State::Disabled>,
               Map, MapSorted, MapUnsorted>),
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

   template<CT::NotVoid K, CT::NotVoid V>
   using TMapSorted = TMap<K, V, State::Enabled>;

   template<CT::NotVoid K, CT::NotVoid V>
   using TMapUnsorted = TMap<K, V, State::Disabled>;
}

namespace Langulus::CTTI
{
   /// Convert TMap -> Text                                                   
   template<CT::NotVoid K, CT::NotVoid V, Anyness::State::StateValue SORT>
   struct Converter<Anyness::TMap<K, V, SORT>, Anyness::Text> {
      static constexpr auto Convert(Anyness::TMap<K, V, SORT> const&) -> Anyness::Text;
   };
}
