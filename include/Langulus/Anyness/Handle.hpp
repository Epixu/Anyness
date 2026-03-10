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
#include "../../../source/components/OwnershipDeep-Heap.hpp"
#include "../../../source/components/OwnershipDeep-Reference.hpp"
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
      template<CT::Reference T> requires (CT::Dense<T> and CT::NotSheddable<T>)
      using THandleEmbeddedDense = Container<
         Com::TypedStatic<DMeta, Deref<T>>,
         Com::HeapReference<0, Deref<T>*>,
         Com::CountStatic<1u>,
         Com::ReserveEmergent<>,
         Com::OwnershipStack<0, false>,
         Com::Assignment<>,
         Com::Emplacement<>,
         Com::Comparison<>,
         Com::IterationOperators<>
      >;
      
      /// Statically typed handle to a sparse element held inside a container 
      template<CT::Reference T> requires (CT::Sparse<T> and CT::NotSheddable<T>)
      using THandleEmbeddedSparse = Container<
         Com::TypedStatic<DMeta, Deref<T>>,
         Com::HeapReference<0, Deref<T>*>,
         Com::CountStatic<1u>,
         Com::OwnershipDeepReference<>,
         Com::Assignment<>,
         Com::Emplacement<>,
         Com::Comparison<>,
         Com::IterationOperators<>
      >;
      
      /// Statically typed handle to a disowned element held inside container 
      template<CT::Reference T> requires CT::NotSheddable<T>
      using THandleDisownedEmbedded = Container<
         Com::TypedStatic<DMeta, Deref<T>>,
         Com::HeapReference<0, Deref<T>*>,
         Com::CountStatic<1u>,
         Com::Assignment<>,
         Com::Emplacement<>,
         Com::Comparison<>,
         Com::IterationOperators<>
      >;
      
      /// Statically typed handle to a local dense value                      
      /// (isomorphic to TOwn)                                                
      //TODO inherit TOwn from this?
      template<CT::NotReference T> requires (CT::Dense<T> and CT::NotSheddable<T>)
      using THandleLocalDense = Container<
         Com::TypedStatic<DMeta, T>,
         Com::Stack<T>,
         Com::CountStatic<1u>,
         Com::Assignment<>,
         Com::Emplacement<>,
         Com::Comparison<>
      >;
      
      /// Statically typed handle to a local sparse value.                    
      ///   @attention this handle is local and has strong ownership!         
      template<CT::NotReference T> requires (CT::Sparse<T> and CT::NotSheddable<T>)
      using THandleLocalSparse = Container<
         Com::TypedStatic<DMeta, T>,
         Com::HeapMovable<0, 0, 0, T*>,
         Com::CountStatic<1u>,
         Com::ReserveEmergent<>,
         Com::OwnershipStack<>,
         Com::OwnershipDeepHeap<>,
         Com::Emplacement<>,
         Com::Assignment<>,
         Com::Comparison<>
      >;
   }

   
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
      Com::OwnershipDeepReference<>,
      Com::Assignment<>,
      Com::Emplacement<>,
      Com::Comparison<>,
      Com::IterationOperators<>
   > {
      using CTTI_Handle    = Yes<>;
      using CTTI_ReflectAs = void;
      using HandleMutType  = HandleMut;
      using DeepType       = HandleDisowned;

      /// Handles can't be piecewise-initialized                              
      HandleMut(Inner::Piecewise, auto&&) = delete;

      constexpr HandleMut() noexcept {
         this->ConstructDefault();
      }

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

      /// Assignment                                                          
      constexpr HandleMut& operator = (HandleMut const& other) {
         return this->AssignAbsorb(Refer(other));
      }
      constexpr HandleMut& operator = (HandleMut&& other) noexcept {
         return this->AssignAbsorb(Move(other));
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
      using DeepType       = HandleDisowned;

      /// Handles can't be piecewise-initialized                              
      HandleDisownedMut(Inner::Piecewise, auto&&) = delete;

      constexpr HandleDisownedMut() noexcept {
         this->ConstructDefault();
      }

      constexpr HandleDisownedMut(HandleDisownedMut const& other) {
         this->Absorb(Disown(other));
      }

      constexpr HandleDisownedMut(HandleDisownedMut&& other) noexcept {
         this->Absorb(Disown(other));
      }

      constexpr HandleDisownedMut(void* ptr, DMeta type) noexcept {
         this->SetHeapInner(ptr);
         this->SetTypeInner(type);
      }

      /// Assignment                                                          
      constexpr HandleDisownedMut& operator = (HandleDisownedMut const& other) {
         return this->AssignAbsorb(Disown(other));
      }
      constexpr HandleDisownedMut& operator = (HandleDisownedMut&& other) noexcept {
         return this->AssignAbsorb(Disown(other));
      }
   };
   

   ///                                                                        
   /// A type-erased immutable handle with ownership.                         
   /// It refers to a picked element inside a type-erased container.          
   ///   @attention handles are never (de)referenced upon construction and    
   ///      destruction - only on reassignment. Since this handle is not      
   ///      mutable, this isn't possible either, however the handle still     
   ///      carries ownership information, so that it can be used on demand   
   ///      instead of sought from the memory manager every time.             
   struct Handle : Container<
      Com::TypedStack<DMeta, void, true>,
      Com::HeapReference<>,
      Com::CountStatic<1u>,
      Com::OwnershipDeepReference<>,
      Com::Comparison<>,
      Com::IterationOperators<>
   > {
      using CTTI_Handle    = Yes<>;
      using CTTI_ReflectAs = void;
      using HandleType     = Handle;
      using DeepType       = HandleDisowned;

      /// Handles can't be piecewise-initialized                              
      Handle(Inner::Piecewise, auto&&) = delete;

      constexpr Handle() noexcept {
         this->ConstructDefault();
      }

      constexpr Handle(Handle const& other) {
         this->Absorb(Refer(other));
      }

      constexpr Handle(Handle&& other) noexcept {
         this->Absorb(Move(other));
      }

      constexpr Handle(void const* ptr, EntryPtr entry, DMeta type) noexcept {
         this->SetHeapInner(ptr);
         this->SetEntriesInner(entry);
         this->SetTypeInner(type);
      }

      /// Assignment                                                          
      constexpr Handle& operator = (Handle const& other) {
         return this->AssignAbsorb(Refer(other));
      }
      constexpr Handle& operator = (Handle&& other) noexcept {
         return this->AssignAbsorb(Move(other));
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
      using DeepType       = HandleDisowned;

      /// Handles can't be piecewise-initialized                              
      HandleDisowned(Inner::Piecewise, auto&&) = delete;

      constexpr HandleDisowned() noexcept {
         this->ConstructDefault();
      }

      /// Refer constructor                                                   
      constexpr HandleDisowned(HandleDisowned const& other) {
         this->Absorb(Disown(other));
      }

      /// Move constructor                                                    
      constexpr HandleDisowned(HandleDisowned&& other) noexcept {
         this->Absorb(Disown(other));
      }

      /// Construction that absorbs the provided container                    
      template<CT::Container C>
      constexpr HandleDisowned(C&& argument) {
         this->Absorb(Disown(argument));
      }

      template<CT::Container C>
      constexpr HandleDisowned(Inner::Absorb, C&& argument) {
         this->Absorb(Disown(argument));
      }

      /// Manual constructor for some niche uses, like iterators              
      constexpr HandleDisowned(void const* ptr, DMeta type) noexcept {
         this->SetHeapInner(ptr);
         this->SetTypeInner(type);
      }

      /// Assignment                                                          
      constexpr HandleDisowned& operator = (HandleDisowned const& other) {
         return this->AssignAbsorb(Disown(other));
      }
      constexpr HandleDisowned& operator = (HandleDisowned&& other) noexcept {
         return this->AssignAbsorb(Disown(other));
      }
   };





   
   ///                                                                        
   /// When T is a reference, then element is embedded inside container       
   ///   @attention memory is never (de)referenced upon construction and      
   ///      destruction - only on reassignment                                
   ///   @tparam T the contained type                                         
   template<CT::Reference T> requires (CT::Dense<T> and CT::NotSheddable<T>)
   struct THandle<T> : Inner::THandleEmbeddedDense<T> {
      using CTTI_Handle    = Yes<>;
      using CTTI_Typed     = Deref<T>;
      using CTTI_ReflectAs = void;
      using HandleMutType  = THandle<DecvqAll<T>>;
      using Denser         = THandle;
      using DeepType       = HandleDisowned;

      /// Handles can't be piecewise-initialized                              
      THandle(Inner::Piecewise, auto&&) = delete;

      constexpr THandle() noexcept {
         this->ConstructDefault();
      }

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

      /// Assignment                                                          
      constexpr THandle& operator = (THandle const& other) {
         return this->AssignAbsorb(Refer(other));
      }
      constexpr THandle& operator = (THandle&& other) noexcept {
         return this->AssignAbsorb(Move(other));
      }
   };
   
   template<CT::Reference T> requires (CT::Sparse<T> and CT::NotSheddable<T>)
   struct THandle<T> : Inner::THandleEmbeddedSparse<T> {
      using CTTI_Handle    = Yes<>;
      using CTTI_Typed     = Deref<T>;
      using CTTI_ReflectAs = void;
      using HandleMutType  = THandle<DecvqAll<T>>;
      using Denser         = THandle<Deptr<T>&>;
      using DeepType       = HandleDisowned;

      /// Handles can't be piecewise-initialized                              
      THandle(Inner::Piecewise, auto&&) = delete;

      constexpr THandle() noexcept {
         this->ConstructDefault();
      }

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

      /// Assignment                                                          
      constexpr THandle& operator = (THandle const& other) {
         return this->AssignAbsorb(Refer(other));
      }
      constexpr THandle& operator = (THandle&& other) noexcept {
         return this->AssignAbsorb(Move(other));
      }
   };
   

   ///                                                                        
   /// When T is a reference, then element is embedded inside container.      
   /// This handle never propagates or modifies ownership.                    
   ///   @tparam T the contained type                                         
   template<CT::Reference T> requires CT::NotSheddable<T>
   struct THandleDisowned<T> : Inner::THandleDisownedEmbedded<T> {
      using CTTI_Handle    = Yes<>;
      using CTTI_Typed     = Deref<T>;
      using CTTI_ReflectAs = void;
      using HandleMutType  = THandleDisowned<DecvqAll<T>>;
      using Denser         = THandle<Deptr<T>&>;
      using DeepType       = HandleDisowned;

      /// Handles can't be piecewise-initialized                              
      THandleDisowned(Inner::Piecewise, auto&&) = delete;

      constexpr THandleDisowned() noexcept {
         this->ConstructDefault();
      }

      constexpr THandleDisowned(THandleDisowned const& other) {
         this->Absorb(Disown(other));
      }

      constexpr THandleDisowned(THandleDisowned&& other) noexcept {
         this->Absorb(Disown(other));
      }

      constexpr ~THandleDisowned() noexcept {
         this->Destroy();
      }

      /// Assignment                                                          
      constexpr THandleDisowned& operator = (THandleDisowned const& other) {
         return this->AssignAbsorb(Disown(other));
      }
      constexpr THandleDisowned& operator = (THandleDisowned&& other) noexcept {
         return this->AssignAbsorb(Disown(other));
      }
   };
   

   ///                                                                        
   /// When T is not a reference, then it is not embedded.                    
   /// Such dense handles are similar to TOwn<T> - data is on the stack.      
   ///   @tparam T the contained type                                         
   template<CT::NotReference T> requires (CT::Dense<T> and CT::NotSheddable<T>)
   struct THandle<T> : Inner::THandleLocalDense<T> {
      using CTTI_Handle    = Yes<>;
      using CTTI_ReflectAs = void;
      using HandleMutType  = THandle<DecvqAll<T>>;
      using Denser         = THandle;
      using DeepType       = HandleDisowned;
      using Base           = typename Inner::THandleLocalDense<T>::Base;

      constexpr THandle(Inner::Piecewise, auto&& a)
      requires requires { T{LglsFwd(a)}; }
         : Base {Stackwise, LglsFwd(a)} {}

      constexpr THandle(Inner::Piecewise, CT::Intent auto&& a)
      requires (not requires { T{LglsFwd(a)}; })
         : Base {Stackwise, DeintCast(a)} {}

      constexpr THandle() noexcept {
         this->ConstructDefault();
      }

      constexpr THandle(THandle const& other) {
         this->Absorb(Refer(other));
      }

      constexpr THandle(THandle&& other) noexcept {
         this->Absorb(Move(other));
      }

      constexpr ~THandle() noexcept {
         this->Destroy();
      }

      /// Assignment                                                          
      constexpr THandle& operator = (THandle const& other) {
         return this->AssignAbsorb(Refer(other));
      }
      constexpr THandle& operator = (THandle&& other) noexcept {
         return this->AssignAbsorb(Move(other));
      }
   };
   

   ///                                                                        
   /// When T is not a reference, then it is not embedded.                    
   /// Such sparse handles are similar to TRef<Deptr<T>>.                     
   ///   @attention such handles are local and have strong ownership! This    
   ///      means that they need to be cleared of their allocation upon move  
   ///      or abandon!                                                       
   ///   @tparam T the contained sparse type                                  
   template<CT::NotReference T> requires (CT::Sparse<T> and CT::NotSheddable<T>)
   struct THandle<T> : Inner::THandleLocalSparse<T> {
      using CTTI_Handle    = Yes<>;
      using CTTI_ReflectAs = void;
      using HandleMutType  = THandle<DecvqAll<T>>;
      using Denser         = THandle<Deptr<T>>;
      using DeepType       = HandleDisowned;
      using Base           = typename Inner::THandleLocalSparse<T>::Base;

      constexpr THandle() noexcept {
         this->ConstructDefault();
      }

      constexpr THandle(THandle const& other) {
         this->Absorb(Refer(other));
      }

      constexpr THandle(THandle&& other) noexcept {
         this->Absorb(Move(other));
      }

      /// Piecewise constructor                                               
      template<class A>
      THandle(Inner::Piecewise, A&& pointer) {
         if (DeintCast(pointer)) {
            this->EmplaceConstruct(LglsFwd(pointer));

            /*this->SetHeapInner(DeintCast(pointer));
            if constexpr (not CT::Disowned<A>)
               this->FindAllocationInner();*/
         }
         else this->ConstructDefault();
      }

      constexpr ~THandle() noexcept {
         this->Destroy();
      }

      /// Assignment                                                          
      constexpr THandle& operator = (THandle const& other) {
         return this->AssignAbsorb(Refer(other));
      }
      constexpr THandle& operator = (THandle&& other) noexcept {
         return this->AssignAbsorb(Move(other));
      }
   };
}
