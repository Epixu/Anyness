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
      
      template<CT::NotReference T> requires CT::Dense<T>
      using THandleLocalDense = Container<
         Com::TypedStatic<DMeta, T>,         // Statically typed        
         Com::Stack<T>,                      // Element on the stack    
         Com::Assignment<>,                  // Allows for reassignment 
         Com::Comparison<>                   // Can be compared         
      >;
      
      template<CT::NotReference T> requires CT::Sparse<T>
      using THandleLocalSparse = Container<
         Com::TypedStatic<DMeta, Deptr<T>>,  // Statically typed        
         Com::HeapMovable<>,                 // Data on the heap        
         Com::OwnershipStack<0, false>,      // Allocation is referenced
         Com::CountStatic<1u>,               // Statically sized        
         Com::Emplacement<>,                 // Can be emplaced         
         Com::Assignment<>,                  // Can be reassigned       
         Com::Comparison<>                   // Can be compared         
      >;
   }

   template<class T> struct THandle;
   template<class T> struct THandleDisowned;


   ///                                                                        
   /// When T is a dense reference, then element is embedded inside container 
   ///   @attention memory is never (de)referenced upon construction and      
   ///      destruction - only on reassignment                                
   ///   @tparam T - the contained type                                       
   template<CT::Reference T> requires CT::Dense<T>
   struct THandle<T> : Inner::THandleEmbeddedDense<T> {
      using CTTI_Handle    = Yes<>;
      using CTTI_Typed     = Deref<T>;
      using CTTI_ReflectAs = void;
      using Base = Inner::THandleEmbeddedDense<T>;
      using Base::Base;

      THandle() = delete;
      
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
      using Base = Inner::THandleEmbeddedSparse<T>;
      using Base::Base;

      THandle() = delete;
      
      constexpr THandle(Deref<T>* ptr, EntryPtr entry) noexcept {
         this->SetHeapInner(ptr);
         this->SetEntriesInner(entry);
      }
   };
   

   ///                                                                        
   /// When T is a dense reference, then element is embedded inside container 
   /// This handle never propagates or modifies ownership                     
   ///   @tparam T - the contained type                                       
   template<CT::Reference T>
   struct THandleDisowned<T> : Inner::THandleDisownedEmbedded<T> {
      using CTTI_Handle    = Yes<>;
      using CTTI_Typed     = Deref<T>;
      using CTTI_ReflectAs = void;
      using Base = Inner::THandleDisownedEmbedded<T>;
      using Base::Base;

      THandleDisowned() = delete;
   };
   

   ///                                                                        
   /// When T is not a reference, then it is not embedded                     
   /// Such dense handles are isomorphic to TOwn<T> - data is on the stack    
   ///   @tparam T - the contained type                                       
   template<CT::NotReference T> requires CT::Dense<T>
   struct THandle<T> : Inner::THandleLocalDense<T> {
      using CTTI_Handle    = Yes<>;
      using CTTI_ReflectAs = void;
      using Base = Inner::THandleLocalDense<T>;
      using Base::Base;

      THandle() = delete;
   };
   

   ///                                                                        
   /// When T is not a reference, then it is not embedded                     
   /// Such sparse handles are isomorphic to TRef<T>                          
   ///   @tparam T - the contained type                                       
   template<CT::NotReference T> requires CT::Sparse<T>
   struct THandle<T> : Inner::THandleLocalSparse<T> {
      using CTTI_Handle    = Yes<>;
      using CTTI_ReflectAs = void;
      using Base = Inner::THandleLocalSparse<T>;
      using Base::Base;
      
      THandle() = delete;
   };
}
