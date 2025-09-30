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
      Com::OwnershipDeepHeap<>,        // Sparse elements are referenced
      Com::HashEmergent<>,             // Hash is retrieved from item   
      Com::Emplacement<>,              // Allows emplacement            
      Com::Assignment<>,               // Allows assignment             
      Com::Removal<>,                  // Allows clear/reset            
      Com::Comparison<>,               // Allows comparisons            
      Com::StateStack<                 // Variable state                
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
   /// A statically-typed container of size 1 that is binary-compatible with  
   /// the type-erased alternative `Any`.                                     
   template<CT::NotVoid T>
   struct TAny : Inner::TAnyBase<T> {
      using CTTI_ReflectAs = Any;
      using Base = Inner::TAnyBase<T>;
      //using Base::Base;
      //using Base::operator =;
      //using Com::Assignment<>::operator =;
      using Base::operator ==;
      using Com::OwnershipDeepHeap<>::DestroyElement;
      using Com::TypedStack<DMeta, T>::IsTypeConstrained;

      // Single element selections                                      
      using Pick    = T const&;
      using PickMut = THandle<T&>;

      constexpr TAny() { this->ConstructDefault(); }
      constexpr TAny(TAny const& other) : TAny {Refer {other}} {}
      constexpr TAny(TAny&& other)      : TAny {Move  {other}} {}

      /// Construction that emplaces T in the container                       
      template<class...A>
      constexpr TAny(A&&...arguments) {
         if constexpr (sizeof...(A) == 0)
            this->ConstructDefault();
         else if constexpr (sizeof...(A) == 1 and CT::ContainsOne<A...>)
            this->ConstructFrom(FWD(arguments)...);
         else {
            // Emplace                                                  
            this->GetType();
            this->AllocateFresh(this->RequestHeap(1));
            if constexpr (sizeof...(A) == 1) {
               using A1 = typename Types<A...>::First;
               if constexpr (CT::Intent<A1> and CT::Similar<TypeOf<A1>, T>)
                  IntentNew(this->GetRaw(), FWD(arguments)...);
               else if constexpr (CT::Similar<A1, T>)
                  IntentNew(this->GetRaw(), IntentOfT<A1&&> {FWD(arguments)...});
               else
                  new (this->GetRaw()) T {FWD(arguments)...};
            }
            else new (this->GetRaw()) T {FWD(arguments)...};
         }
      }

      /// Assignment                                                          
      template<class A>
      constexpr TAny& operator = (A&& argument) {
         if constexpr (CT::ContainsOne<A>)
            this->AssignFrom(FWD(argument));
         else
            Com::Assignment<>::operator = (FWD(argument));
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
