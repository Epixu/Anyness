///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Any.hpp"
#include "THandle.hpp"


namespace Langulus::Anyness::Inner
{
   template<CT::NotVoid T>
   using TAnyBase = Container<
      Com::TypedStack<DMeta, T>,       // Type-constrained              
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
         DefineState::Typed<State::Enabled>, // Always type-constrained 
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
   /// A statically-typed container of size 1 that is binary-compatible with  
   /// the type-erased alternative `Any`.                                     
   template<CT::NotVoid T>
   struct TAny : Inner::TAnyBase<T> {
      using CTTI_ReflectAs = Any;
      using CTTI_Deep      = Yes<>;
      using CTTI_MapsTo    = Text;

      using Base = Inner::TAnyBase<T>;
      using Com::TypedStack<DMeta, T>::IsTypeConstrained;

      using Pick          = T const&;
      using PickMut       = THandle<T&>;
      using HandleType    = THandle<T const&>;
      using HandleMutType = THandle<T&>;
      using DeepType      = Any;

      constexpr TAny() noexcept {
         this->ConstructDefault();
      }
      constexpr TAny(TAny const& other) {
         this->ConstructFrom(Refer(other));
      }
      constexpr TAny(TAny&& other) noexcept {
         this->ConstructFrom(Move(other));
      }
      constexpr ~TAny() noexcept {
         this->Destroy();
      }

      /// Construction that either absorbs the provided container, or         
      /// emplaces T in the container, using A... as constructor arguments    
      template<class...A>
      constexpr TAny(A&&...arguments) {
         if constexpr (sizeof...(A) == 1 and CT::ContainsOne<A...>) {
            LglsAssumeUser(
               ((Same<Deint<A>, TAny> or Same<TypeOf<Deint<A>>, T>) and ...),
               "Ambiguous use of construction "
               "- you should use tag-dispatch with first argument either Absorb "
               "(if you want to overwrite the container itself) or Piecewise "
               "(if you want to overwrite the first item) in order to clearly "
               "state your intent. Absorb will be used by default!"
            );
            this->ConstructFrom(FWD(arguments)...);
         }
         else {
            this->GetType();
            this->AllocateFresh(this->RequestHeap(1));
            this->ResetState();
            this->EmplaceConstruct(FWD(arguments)...);
         }
      }
      
      /// Construction that absorbs the provided container                    
      template<class A>
      constexpr TAny(Inner::Absorb, A&& argument) {
         this->ConstructFrom(FWD(argument));
      }
      
      /// Emplaces T inside, using A... as constructor arguments              
      template<class...A>
      constexpr TAny(Inner::Piecewise, A&&...arguments) {
         this->GetType();
         this->AllocateFresh(this->RequestHeap(1));
         this->ResetState();
         this->EmplaceConstruct(FWD(arguments)...);
      }

      /// Assignment                                                          
      constexpr TAny& operator = (TAny const& other) {
         return this->AssignFrom(Refer(other));
      }
      constexpr TAny& operator = (TAny&& other) noexcept {
         return this->AssignFrom(Move(other));
      }

      template<class A>
      constexpr TAny& operator = (A&& argument) {
         if constexpr (CT::ContainsOne<A>) {
            LglsAssumeUser(
               (Same<Deint<A>, TAny> or Same<TypeOf<Deint<A>>, T>),
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
      constexpr auto operator <=> (TAny const& other) const noexcept
      -> ::std::partial_ordering {
         return this->Compare(other);
      }

      template<class A>
      constexpr auto operator <=> (const A& argument) const has_assumptions
      -> decltype(Fake<T>() <=> argument) {
         if constexpr (CT::ContainsOne<A>) {
            LglsAssumeUser(
               (Same<Deint<A>, TAny> or Same<TypeOf<Deint<A>>, T>),
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
      constexpr bool operator == (TAny const& other) const noexcept {
         return this->CompareEqual(other);
      }

      template<class A>
      constexpr bool operator == (const A& argument) const has_assumptions {
         if constexpr (CT::ContainsOne<A>) {
            LglsAssumeUser(
               (Same<Deint<A>, TAny> or Same<TypeOf<Deint<A>>, T>),
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
   /// Convert TAny -> Text                                                   
   template<class T>
   struct Converter<Anyness::TAny<T>, Anyness::Text> {
      static constexpr void Convert(Anyness::TAny<T> const&, Anyness::Text&);
      static constexpr auto Convert(Anyness::TAny<T> const&) -> Anyness::Text;
   };
}
