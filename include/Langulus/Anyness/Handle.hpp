///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../../../source/components/Typed-Stack.hpp"
#include "../../../source/components/Heap-Reference.hpp"
#include "../../../source/components/Count-Static.hpp"
#include "../../../source/components/Reserve-Emergent.hpp"
#include "../../../source/components/OwnershipDeep-Stack.hpp"
#include "../../../source/components/Assignment.hpp"
#include "../../../source/components/Emplacement.hpp"
#include "../../../source/components/Comparison.hpp"
#include "../../../source/components/Iteration-Operators.hpp"


namespace Langulus::Anyness
{
   struct HandleDisowned;
   
   ///                                                                        
   /// A type-erased mutable handle with ownership.                           
   /// It refers to a picked element inside a type-erased container.          
   /// Handles can never be empty.                                            
   ///   @attention handles are never (de)referenced upon construction and    
   ///      destruction - only on reassignment                                
   struct HandleMut : Container<
      Com::TypedStack<DMeta, void, true>,
      Com::HeapReference<>,
      Com::CountStatic<1u>,
      Com::ReserveEmergent<>,
      Com::OwnershipDeepStack<>,
      Com::Assignment<>,
      Com::Emplacement<>,
      Com::Comparison<>,
      Com::IterationOperators<>
   > {
      using CTTI_Handle    = Yes<>;
      using CTTI_ReflectAs = void;
      using HandleMutType  = HandleMut;
      using DeepType       = HandleDisowned;

      /// Handles can't be default- or piecewise-initialized                  
      HandleMut() = delete;
      HandleMut(Inner::Piecewise, auto&&) = delete;

      constexpr HandleMut(HandleMut const& other) {
         this->Absorb(Refer(other));
      }
      constexpr HandleMut(HandleMut&& other) noexcept {
         this->Absorb(Move(other));
      }

      constexpr HandleMut(void* ptr, EntryPtr entry, DMeta type) noexcept {
         this->SetHeapInner(ptr);
         this->SetEntriesInner(entry);
         this->SetTypeInner(type);
      }

      /*auto GetAllocation() const noexcept -> Allocation const* {
         return *this->GetEntriesInner();
      }*/
   };
   

   ///                                                                        
   /// A type-erased mutable handle without ownership.                        
   /// It refers to a picked element inside a type-erased container.          
   /// Handles can never be empty.                                            
   struct HandleDisownedMut : Container<
      Com::TypedStack<DMeta, void, true>,
      Com::HeapReference<>,
      Com::CountStatic<1u>,
      Com::Assignment<>,
      Com::Emplacement<>,
      Com::Comparison<>,
      Com::IterationOperators<>
   > {
      using CTTI_Handle    = Yes<>;
      using CTTI_ReflectAs = void;
      using HandleMutType  = HandleDisownedMut;
      using DeepType       = HandleDisowned;

      /// Handles can't be default- or piecewise-initialized                  
      HandleDisownedMut() = delete;
      HandleDisownedMut(Inner::Piecewise, auto&&) = delete;

      constexpr HandleDisownedMut(HandleDisownedMut const& other) {
         this->Absorb(Refer(other));
      }
      constexpr HandleDisownedMut(HandleDisownedMut&& other) noexcept {
         this->Absorb(Move(other));
      }

      constexpr HandleDisownedMut(void* ptr, DMeta type) noexcept {
         this->SetHeapInner(ptr);
         this->SetTypeInner(type);
      }
   };
   

   ///                                                                        
   /// A type-erased immutable handle with ownership.                         
   /// It refers to a picked element inside a type-erased container.          
   /// Handles can never be empty.                                            
   ///   @attention handles are never (de)referenced upon construction and    
   ///      destruction - only on reassignment. Since this handle is not      
   ///      mutable, this isn't possible either, however the handle still     
   ///      carries ownership information, so that it can be used on demand   
   ///      instead of sought from the memory manager every time.             
   struct Handle : Container<
      Com::TypedStack<DMeta, void, true>,
      Com::HeapReference<>,
      Com::CountStatic<1u>,
      Com::OwnershipDeepStack<>,
      Com::Comparison<>,
      Com::IterationOperators<>
   > {
      using CTTI_Handle    = Yes<>;
      using CTTI_ReflectAs = void;
      using HandleType     = Handle;
      using DeepType       = HandleDisowned;

      /// Handles can't be default- or piecewise-initialized                  
      Handle() = delete;
      Handle(Inner::Piecewise, auto&&) = delete;

      constexpr Handle(Handle const& other) {
         this->Absorb(Refer(other));
      }
      constexpr Handle(Handle&& other) noexcept {
         this->Absorb(Move(other));
      }

      constexpr Handle(void* ptr, EntryPtr entry, DMeta type) noexcept {
         this->SetHeapInner(ptr);
         this->SetEntriesInner(entry);
         this->SetTypeInner(type);
      }

      /*auto GetAllocation() const noexcept -> Allocation const* {
         return *this->GetEntriesInner();
      }*/
   };
   

   ///                                                                        
   /// A type-erased immutable handle without ownership.                      
   /// It refers to a picked element inside a type-erased container.          
   /// Handles can never be empty.                                            
   struct HandleDisowned : Container<
      Com::TypedStack<DMeta, void, true>,
      Com::HeapReference<>,
      Com::CountStatic<1u>,
      Com::Comparison<>,
      Com::IterationOperators<>
   > {
      using CTTI_Handle    = Yes<>;
      using CTTI_ReflectAs = void;
      using HandleType     = HandleDisowned;
      using DeepType       = HandleDisowned;

      /// Handles can't be default- or piecewise-initialized                  
      HandleDisowned() = delete;
      HandleDisowned(Inner::Piecewise, auto&&) = delete;

      /// Refer constructor                                                   
      constexpr HandleDisowned(HandleDisowned const& other) {
         this->Absorb(Refer(other));
      }

      /// Move constructor                                                    
      constexpr HandleDisowned(HandleDisowned&& other) noexcept {
         this->Absorb(Move(other));
      }

      /// Construction that absorbs the provided container                    
      template<CT::Container C>
      constexpr HandleDisowned(C&& argument) {
         this->Absorb(FWD(argument));
      }
      template<CT::Container C>
      constexpr HandleDisowned(Inner::Absorb, C&& argument) {
         this->Absorb(FWD(argument));
      }

      /// Manual constructor for some niche uses, like iterators              
      constexpr HandleDisowned(void* ptr, DMeta type) noexcept {
         this->SetHeapInner(ptr);
         this->SetTypeInner(type);
      }
   };
}
