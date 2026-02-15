///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Set.hpp"


namespace Langulus::Anyness::Inner
{
   template<CT::NotVoid T, State::StateValue SORT>
   using TSetBase = Container<
      Com::TypedStack<DMeta, T>,       // Type-constrained              
      Com::HeapMovable<0, T*>,         // Pointer to heap memory        
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
   /// A statically-typed non-contiguous set of variable size that is         
   /// binary-compatible with the type-erased alternative `Set`.              
   template<CT::NotVoid T, State::StateValue SORT = State::Variable>
   struct TSet : Inner::TSetBase<T, SORT> {
      using CTTI_Set       = Yes<>;
      using CTTI_ReflectAs = Many;
      using CTTI_Deep      = Yes<>;
      using CTTI_MapsTo    = Text;

      using Base           = Inner::TSetBase<T, SORT>;
      using Pick           = ConstAll<T&>;
      using PickMut        = Tif<CT::Sparse<T>, THandle<T&>, T&>;
      using HandleType     = THandle<ConstAll<T&>>;
      using HandleMutType  = THandle<T&>;
      using DeepType       = Any;

      constexpr TSet() noexcept {
         this->ConstructDefault();
      }
      constexpr TSet(TSet const& other) {
         this->Absorb(Refer(other));
      }
      constexpr TSet(TSet&& other) noexcept {
         this->Absorb(Move(other));
      }
      constexpr ~TSet() noexcept {
         this->Destroy();
      }
      
      /// Construction that either absorbs the provided containers, or        
      /// emplaces all A in the container                                     
      template<class A1, class...AN>
      constexpr TSet(A1&& a1, AN&&...an) {
         if constexpr (sizeof...(AN) == 0) {
            if constexpr (SameAsOneOf<Deint<A1>, 
               TSet<T, State::Variable>, TSet<T, State::Enabled>, TSet<T, State::Disabled>,
               Set, SetSorted, SetUnsorted>
            ) {
               LglsAssumeUser((not SameAsOneOf<T,
                  TSet<T, State::Variable>, TSet<T, State::Enabled>, TSet<T, State::Disabled>,
                  Set, SetSorted, SetUnsorted>),
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
      
      /// Construction that absorbs the provided container                    
      template<class A1, class...AN>
      constexpr TSet(Inner::Absorb, A1&& a1, AN&&...an) {
         if constexpr (sizeof...(AN) == 0)
            this->Absorb(LglsFwd(a1));
         else {
            this->ConstructDefault();
            this->Concat(LglsFwd(a1), LglsFwd(an)...);
         }
      }
      
      /// Construction that emplaces all arguments inside                     
      template<class A1, class...AN>
      constexpr TSet(Inner::Piecewise, A1&& a1, AN&&...an) {
         if constexpr (sizeof...(AN) == 0)
            this->EmplaceConstruct(LglsFwd(a1));
         else {
            this->ConstructDefault();
            this->Insert(LglsFwd(a1), LglsFwd(an)...);
         }
      }

      /// Assignment                                                          
      constexpr TSet& operator = (TSet const& other) {
         return this->AssignAbsorb(Refer(other));
      }
      constexpr TSet& operator = (TSet&& other) noexcept {
         return this->AssignAbsorb(Move(other));
      }

      template<class A>
      constexpr TSet& operator = (A&& argument) {
         if constexpr (SameAsOneOf<Deint<A>,
            TSet<T, State::Variable>, TSet<T, State::Enabled>, TSet<T, State::Disabled>,
            Set, SetSorted, SetUnsorted>
         ) {
            LglsAssumeUser((not SameAsOneOf<T,
               TSet<T, State::Variable>, TSet<T, State::Enabled>, TSet<T, State::Disabled>,
               Set, SetSorted, SetUnsorted>),
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
      using Com::IterationRange<>::begin;
      using Com::IterationRange<>::end;
      using Com::IterationRange<>::rbegin;
      using Com::IterationRange<>::rend;
   };

   template<CT::NotVoid T>
   using TSetSorted = TSet<T, State::Enabled>;

   template<CT::NotVoid T>
   using TSetUnsorted = TSet<T, State::Disabled>;
}

namespace Langulus::CTTI
{
   /// Convert TSet -> Text                                                   
   template<CT::NotVoid T, Anyness::State::StateValue SORT>
   struct Converter<Anyness::TSet<T, SORT>, Anyness::Text> {
      static constexpr auto Convert(Anyness::TSet<T, SORT> const&) -> Anyness::Text;
   };
}
