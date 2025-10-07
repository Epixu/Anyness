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
   ///                                                                        
   /// A type-erased mutable handle with ownership.                           
   /// It refers to a picked element inside a type-erased container.          
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

      HandleMut() = delete;
      
      constexpr HandleMut(HandleMut const& other) {
         this->ConstructFrom(Refer(other));
      }
      constexpr HandleMut(HandleMut&& other) noexcept {
         this->ConstructFrom(Move(other));
      }

      constexpr HandleMut(void* ptr, EntryPtr entry, DMeta type) noexcept {
         this->SetHeapInner(ptr);
         this->SetEntriesInner(entry);
         this->SetTypeInner(type);
      }
   };
   

   ///                                                                        
   /// A type-erased mutable handle without ownership.                        
   /// It refers to a picked element inside a type-erased container.          
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

      HandleDisownedMut() = delete;
      
      constexpr HandleDisownedMut(HandleDisownedMut const& other) {
         this->ConstructFrom(Refer(other));
      }
      constexpr HandleDisownedMut(HandleDisownedMut&& other) noexcept {
         this->ConstructFrom(Move(other));
      }

      constexpr HandleDisownedMut(void* ptr, DMeta type) noexcept {
         this->SetHeapInner(ptr);
         this->SetTypeInner(type);
      }
   };
   

   ///                                                                        
   /// A type-erased immutable handle with ownership.                         
   /// It refers to a picked element inside a type-erased container.          
   ///   @attention handles are never (de)referenced upon construction and    
   ///      destruction - only on reassignment. Since this handle is not      
   ///      mutable, this isn't possible either, however the handle still     
   ///      carries ownership information, so that it can be used on demand   
   ///      instead of sought from the memory manager every time              
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

      Handle() = delete;
      
      constexpr Handle(Handle const& other) {
         this->ConstructFrom(Refer(other));
      }
      constexpr Handle(Handle&& other) noexcept {
         this->ConstructFrom(Move(other));
      }

      constexpr Handle(void* ptr, EntryPtr entry, DMeta type) noexcept {
         this->SetHeapInner(ptr);
         this->SetEntriesInner(entry);
         this->SetTypeInner(type);
      }
   };
   

   ///                                                                        
   /// A type-erased immutable handle without ownership.                      
   /// It refers to a picked element inside a type-erased container.          
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

      HandleDisowned() = delete;
      
      constexpr HandleDisowned(HandleDisowned const& other) {
         this->ConstructFrom(Refer(other));
      }
      constexpr HandleDisowned(HandleDisowned&& other) noexcept {
         this->ConstructFrom(Move(other));
      }

      constexpr HandleDisowned(void* ptr, DMeta type) noexcept {
         this->SetHeapInner(ptr);
         this->SetTypeInner(type);
      }
   };
}
