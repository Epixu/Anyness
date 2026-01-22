///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../../../source/components/Typed-Static.hpp"
#include "../../../source/components/Heap-Reference.hpp"
#include "../../../source/components/OwnershipDeep-Stack.hpp"
#include "../../../source/components/Count-Static.hpp"
#include "../../../source/components/Reserve-Emergent.hpp"
#include "../../../source/components/Stack.hpp"
#include "../../../source/components/Assignment.hpp"
#include "../../../source/components/Emplacement.hpp"
#include "../../../source/components/Comparison.hpp"
#include "../../../source/components/Iteration-Operators.hpp"


namespace Langulus::Anyness
{
   namespace Inner
   {
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
      template<CT::NotReference T> requires CT::Dense<T>
      using THandleLocalDense = Container<
         Com::TypedStatic<DMeta, T>,
         Com::Stack<T>,
         Com::Assignment<>,
         Com::Comparison<>
      >;
      
      /// Statically typed handle to a local sparse value                     
      /// (isomorphic to TRef)                                                
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

   template<class T> struct THandle;
   template<class T> struct THandleDisowned;


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

      auto GetAllocation() const noexcept -> Allocation const* {
         return *this->GetEntriesInner();
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
