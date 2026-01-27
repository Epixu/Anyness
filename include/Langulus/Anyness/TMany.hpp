///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Many.hpp"


namespace Langulus::Anyness::Inner
{
   template<CT::NotVoid T>
   using TManyBase = Container<
      Com::TypedStack<DMeta, T>,       // Type-constrained              
      Com::HeapMovable<>,              // Pointer to heap memory        
      Com::OwnershipStack<>,           // Allocation is referenced      
      Com::CountStack<>,               // Dynamically sized             
      Com::ReserveEmergent<>,          // Reserve derived from alloc    
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
         DefineState::Typed<State::Enabled>, // Always type-constrained 
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
   /// A statically-typed contiguous container of variable size that is       
   /// binary-compatible with the type-erased alternative `Many`.             
   template<CT::NotVoid T>
   struct TMany : Inner::TManyBase<T> {
      using CTTI_ReflectAs = Many;
      using CTTI_Deep      = Yes<>;
      using CTTI_MapsTo    = Text;

      using Base = Inner::TManyBase<T>;
      using Com::TypedStack<DMeta, T>::IsTypeConstrained;

      using Pick          = T const&;
      using PickMut       = THandle<T&>;
      using HandleType    = THandle<T const&>;
      using HandleMutType = THandle<T&>;
      using DeepType      = Any;

      constexpr TMany() noexcept {
         this->ConstructDefault();
      }
      constexpr TMany(TMany const& other) {
         this->Absorb(Refer(other));
      }
      constexpr TMany(TMany&& other) noexcept {
         this->Absorb(Move(other));
      }
      constexpr ~TMany() noexcept {
         this->Destroy();
      }
      
      /// Construction that either absorbs the provided container, or         
      /// emplaces T in the container, using A... as constructor arguments    
      template<class...A>
      constexpr TMany(A&&...arguments) {
         if constexpr (sizeof...(A) == 1 and CT::Container<A...>) {
            LglsAssumeUser(
               ((Same<Deint<A>, TMany> or Same<TypeOf<Deint<A>>, T>) and ...),
               "Ambiguous use of construction "
               "- you should use tag-dispatch with first argument either Absorb "
               "(if you want to overwrite the container itself) or Piecewise "
               "(if you want to overwrite the first item) in order to clearly "
               "state your intent. Absorb will be used by default!"
            );
            this->Absorb(FWD(arguments)...);
         }
         else this->EmplaceConstruct(FWD(arguments)...);
      }
      
      /// Construction that absorbs the provided container                    
      constexpr TMany(Inner::Absorb, auto&& argument) {
         this->Absorb(FWD(argument));
      }
      
      /// Emplaces T inside, using A... as constructor arguments              
      constexpr TMany(Inner::Piecewise, auto&&...arguments) {
         this->EmplaceConstruct(FWD(arguments)...);
      }

      /// Assignment                                                          
      constexpr TMany& operator = (TMany const& other) {
         return this->AssignAbsorb(Refer(other));
      }
      constexpr TMany& operator = (TMany&& other) noexcept {
         return this->AssignAbsorb(Move(other));
      }

      template<class A>
      constexpr TMany& operator = (A&& argument) {
         if constexpr (CT::Container<A>) {
            LglsAssumeUser(
               (Same<Deint<A>, TMany> or Same<TypeOf<Deint<A>>, T>),
               "Ambiguous use of assignment "
               "- you should use either AssignAbsorb (if you want to overwrite "
               "the container itself) or Assign (if you want to overwrite the "
               "first item) in order to clearly state your intent. "
               "AssignAbsorb will be used by default!"
            );
            return this->AssignAbsorb(FWD(argument));
         }
         else return this->Assign(FWD(argument));
      }
      
      using Com::Comparison<>::operator <=>;
      using Com::Comparison<>::operator ==;
   };
}

namespace Langulus::CTTI
{
   /// Convert TMany -> Text                                                  
   template<class T>
   struct Converter<Anyness::TMany<T>, Anyness::Text> {
      static constexpr void Convert(Anyness::TMany<T> const&, Anyness::Text&);
      static constexpr auto Convert(Anyness::TMany<T> const&) -> Anyness::Text;
   };
}
