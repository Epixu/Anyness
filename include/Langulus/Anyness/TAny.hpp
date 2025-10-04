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
      using CTTI_Deep = Yes<>;

      using Base = Inner::TAnyBase<T>;
      using Base::operator ==;
      using Com::TypedStack<DMeta, T>::IsTypeConstrained;

      using Pick          = T const&;
      using PickMut       = THandle<T&>;
      using HandleType    = THandle<T const&>;
      using HandleMutType = THandle<T&>;
      using DeepType      = Any;

      constexpr TAny() noexcept { this->ConstructDefault(); }
      constexpr TAny(TAny const& other)     : TAny {Refer {other}} {}
      constexpr TAny(TAny&& other) noexcept : TAny {Move  {other}} {}
      constexpr ~TAny() noexcept { this->Destroy(); }

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
            
            if constexpr (sizeof...(A) == 1) {
               using A1 = typename Types<A...>::First;
               if constexpr (CT::Intent<A1> and Same<TypeOf<A1>, T>)
                  IntentNew(this->GetRaw(), FWD(arguments)...);
               else if constexpr (Same<A1, T>)
                  IntentNew(this->GetRaw(), IntentOfT<A1&&> {FWD(arguments)...});
               else
                  new (this->GetRaw()) T {FWD(arguments)...};
            }
            else new (this->GetRaw()) T {FWD(arguments)...};
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
            
         if constexpr (sizeof...(A) == 1) {
            using A1 = typename Types<A...>::First;
            if constexpr (CT::Intent<A1> and Same<TypeOf<A1>, T>)
               IntentNew(this->GetRaw(), FWD(arguments)...);
            else if constexpr (Same<A1, T>)
               IntentNew(this->GetRaw(), IntentOfT<A1&&> {FWD(arguments)...});
            else
               new (this->GetRaw()) T {FWD(arguments)...};
         }
         else new (this->GetRaw()) T {FWD(arguments)...};
      }

      /// Assignment                                                          
      constexpr TAny& operator = (TAny const& other) {
         this->AssignFrom(Refer(other));
         return *this;
      }
      constexpr TAny& operator = (TAny&& other) noexcept {
         this->AssignFrom(Move(other));
         return *this;
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
            this->AssignFrom(FWD(argument));
         }
         else Com::Assignment<>::operator = (FWD(argument));
         return *this;
      }
   };
   
   /// A statically typed container of size 1 that is binary compatible with  
   /// the type-erased alternative above                                      
   /*template<CT::NotVoid T>
   struct TAnyView : Container<
      Com::TypedStack<DMeta, T>,       // Type-constrained              
      Com::HeapMovable<>,              // Pointer to heap memory        
      Com::OwnershipStack<0, false>,   // Pointer to an allocation      
      Com::CountStatic<1>,             // Statically sized to 1         
      Com::StateStack<                 // Variable state                
         DefineState::Future<>,        // Adds a 'missing future' state 
         DefineState::Past<>,          // Adds a 'missing past' state   
         DefineState::Compressed<>,    // Adds 'compressed' state       
         DefineState::Encrypted<>,     // Adds 'encrypted' state        
         DefineState::Tracked<>        // Adds 'tracked' state          
      >
   > {
      using CTTI_ReflectAs = AnyView;
   };*/
}
