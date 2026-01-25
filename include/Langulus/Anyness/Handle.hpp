///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../../../source/components/Typed-Stack.hpp"
#include "../../../source/components/Typed-Static.hpp"
#include "../../../source/components/Heap-Reference.hpp"
#include "../../../source/components/Count-Static.hpp"
#include "../../../source/components/Reserve-Emergent.hpp"
#include "../../../source/components/OwnershipDeep-Stack.hpp"
#include "../../../source/components/Assignment.hpp"
#include "../../../source/components/Emplacement.hpp"
#include "../../../source/components/Comparison.hpp"
#include "../../../source/components/Iteration-Operators.hpp"
#include "../../../source/components/Stack.hpp"


namespace Langulus::Anyness
{
   namespace Inner
   {
      //TODO define the type-erased bases for handles here as well??

      /// Statically typed handle to a dense element held inside a container  
      template<CT::Reference T> requires CT::Dense<T>
      using THandleEmbeddedDense = Container<
         Com::TypedStatic<DMeta, Deref<T>>,
         Com::HeapReference<>,
         Com::OwnershipStack<0, false>,
         Com::CountStatic<1u>,
         Com::ReserveEmergent<>,
         Com::Assignment<>,
         Com::Emplacement<>,
         Com::Comparison<>,
         Com::IterationOperators<>
      >;
      
      /// Statically typed handle to a sparse element held inside a container 
      template<CT::Reference T> requires CT::Sparse<T>
      using THandleEmbeddedSparse = Container<
         Com::TypedStatic<DMeta, Deref<T>>,
         Com::HeapReference<>,
         Com::CountStatic<1u>,
         Com::OwnershipDeepStack<>,
         Com::Assignment<>,
         Com::Emplacement<>,
         Com::Comparison<>,
         Com::IterationOperators<>
      >;
      
      /// Statically typed handle to a disowned element held inside container 
      template<CT::Reference T>
      using THandleDisownedEmbedded = Container<
         Com::TypedStatic<DMeta, Deref<T>>,
         Com::HeapReference<>,
         Com::CountStatic<1u>,
         Com::Assignment<>,
         Com::Emplacement<>,
         Com::Comparison<>,
         Com::IterationOperators<>
      >;
      
      /// Statically typed handle to a local dense value                      
      /// (isomorphic to TOwn)                                                
      //TODO inherit TOwn from this?
      template<CT::NotReference T> requires CT::Dense<T>
      using THandleLocalDense = Container<
         Com::TypedStatic<DMeta, T>,
         Com::Stack<T>,
         Com::Assignment<>,
         Com::Comparison<>
      >;
      
      /// Statically typed handle to a local sparse value                     
      /// (isomorphic to TRef)                                                
      //TODO inherit TRef from this?
      template<CT::NotReference T> requires CT::Sparse<T>
      using THandleLocalSparse = Container<
         Com::TypedStatic<DMeta, Deptr<T>>,
         Com::HeapMovable<>,
         Com::OwnershipStack<0, false>,
         Com::CountStatic<1u>,
         Com::Emplacement<>,
         Com::Assignment<>,
         Com::Comparison<>
      >;
   }

   
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





   
   ///                                                                        
   /// When T is a dense reference, then element is embedded inside container 
   /// Handles can never be empty.                                            
   ///   @attention memory is never (de)referenced upon construction and      
   ///      destruction - only on reassignment                                
   ///   @tparam T the contained type                                         
   template<CT::Reference T> requires CT::Dense<T>
   struct THandle<T> : Inner::THandleEmbeddedDense<T> {
      using CTTI_Handle    = Yes<>;
      using CTTI_Typed     = Deref<T>;
      using CTTI_ReflectAs = void;
      using HandleMutType  = THandle<DecvqAll<T>>;
      using Denser         = THandle;
      using DeepType       = HandleDisowned;

      /// Handles can't be default- or piecewise-initialized                  
      THandle() = delete;
      THandle(Inner::Piecewise, auto&&) = delete;

      constexpr THandle(THandle const& other) {
         this->Absorb(Refer(other));
      }
      constexpr THandle(THandle&& other) noexcept {
         this->Absorb(Move(other));
      }
      constexpr ~THandle() noexcept {
         this->Destroy();
      }

      constexpr THandle(T ptr, AllocationPtr alloc) noexcept {
         this->SetHeapInner(&ptr);
         this->SetAllocationInner(alloc);
      }
      constexpr THandle(Deref<T>* ptr, AllocationPtr alloc) noexcept {
         this->SetHeapInner(ptr);
         this->SetAllocationInner(alloc);
      }
   };
   
   template<CT::Reference T> requires CT::Sparse<T>
   struct THandle<T> : Inner::THandleEmbeddedSparse<T> {
      using CTTI_Handle    = Yes<>;
      using CTTI_Typed     = Deref<T>;
      using CTTI_ReflectAs = void;
      using HandleMutType  = THandle<DecvqAll<T>>;
      using Denser         = THandle<Deptr<T>&>;
      using DeepType       = HandleDisowned;

      /// Handles can't be default- or piecewise-initialized                  
      THandle() = delete;
      THandle(Inner::Piecewise, auto&&) = delete;

      constexpr THandle(THandle const& other) {
         this->Absorb(Refer(other));
      }
      constexpr THandle(THandle&& other) noexcept {
         this->Absorb(Move(other));
      }
      constexpr ~THandle() noexcept {
         this->Destroy();
      }

      constexpr THandle(Deref<T>* ptr, EntryPtr entry) noexcept {
         this->SetHeapInner(ptr);
         this->SetEntriesInner(entry);
      }
   };
   

   ///                                                                        
   /// When T is a dense reference, then element is embedded inside container.
   /// This handle never propagates or modifies ownership.                    
   /// Handles can never be empty.                                            
   ///   @tparam T the contained type                                         
   template<CT::Reference T>
   struct THandleDisowned<T> : Inner::THandleDisownedEmbedded<T> {
      using CTTI_Handle    = Yes<>;
      using CTTI_Typed     = Deref<T>;
      using CTTI_ReflectAs = void;
      using HandleMutType  = THandleDisowned<DecvqAll<T>>;
      using Denser         = THandle<Deptr<T>&>;
      using DeepType       = HandleDisowned;

      /// Handles can't be default- or piecewise-initialized                  
      THandleDisowned() = delete;
      THandleDisowned(Inner::Piecewise, auto&&) = delete;

      constexpr THandleDisowned(THandleDisowned const& other) {
         this->Absorb(Refer(other));
      }
      constexpr THandleDisowned(THandleDisowned&& other) noexcept {
         this->Absorb(Move(other));
      }
      constexpr ~THandleDisowned() noexcept {
         this->Destroy();
      }
   };
   

   ///                                                                        
   /// When T is not a reference, then it is not embedded.                    
   /// Such dense handles are isomorphic to TOwn<T> - data is on the stack.   
   /// Handles can never be empty.                                            
   ///   @tparam T the contained type                                         
   template<CT::NotReference T> requires CT::Dense<T>
   struct THandle<T> : Inner::THandleLocalDense<T> {
      using CTTI_Handle    = Yes<>;
      using CTTI_ReflectAs = void;
      using HandleMutType  = THandle<DecvqAll<T>>;
      using Denser         = THandle;
      using DeepType       = HandleDisowned;

      /// Handles can't be default- or piecewise-initialized                  
      THandle() = delete;
      THandle(Inner::Piecewise, auto&&) = delete;

      constexpr THandle(THandle const& other) {
         this->Absorb(Refer(other));
      }
      constexpr THandle(THandle&& other) noexcept {
         this->Absorb(Move(other));
      }
      constexpr ~THandle() noexcept {
         this->Destroy();
      }
   };
   

   ///                                                                        
   /// When T is not a reference, then it is not embedded.                    
   /// Such sparse handles are isomorphic to TRef<T>.                         
   /// Handles can never be empty.                                            
   ///   @tparam T the contained type                                         
   template<CT::NotReference T> requires CT::Sparse<T>
   struct THandle<T> : Inner::THandleLocalSparse<T> {
      using CTTI_Handle    = Yes<>;
      using CTTI_ReflectAs = void;
      using HandleMutType  = THandle<DecvqAll<T>>;
      using Denser         = THandle<Deptr<T>>;
      using DeepType       = HandleDisowned;

      /// Handles can't be default- or piecewise-initialized                  
      THandle() = delete;
      THandle(Inner::Piecewise, auto&&) = delete;

      constexpr THandle(THandle const& other) {
         this->Absorb(Refer(other));
      }
      constexpr THandle(THandle&& other) noexcept {
         this->Absorb(Move(other));
      }
      constexpr ~THandle() noexcept {
         this->Destroy();
      }
   };
}
