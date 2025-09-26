///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../../../source/Container.hpp"
#include "../../../source/components/Typed-Static.hpp"
#include "../../../source/components/Heap-Movable.hpp"
#include "../../../source/components/Ownership-Stack.hpp"
#include "../../../source/components/Count-Static.hpp"
#include "../../../source/components/Emplacement.hpp"
#include "../../../source/components/Assignment.hpp"
#include "../../../source/components/Comparison.hpp"


namespace Langulus::Anyness::Inner
{
   template<class T>
   using TRefBase = Container<
      Com::TypedStatic<DMeta, T>,         // Statically typed          
      Com::HeapMovable<>,                 // Data on the heap          
      Com::OwnershipStack<>,              // Allocation is referenced  
      Com::CountStatic<1u>,               // Statically sized          
      Com::Emplacement<>,                 // Can be emplaced           
      Com::Assignment<>,                  // Can be reassigned         
      Com::Comparison<>                   // Can be compared           
   >;
}

namespace Langulus::Anyness
{
   ///                                                                        
   ///   A statically typed shared pointer                                    
   ///                                                                        
   ///   Works fine with packed pointers as well. Has deep ownership, but no  
   /// states are applied. You can use TAny instead if you want any           
   /// combination of encryption, compression and linking.                    
   ///                                                                        
   template<class T>
   struct TRef : Inner::TRefBase<T> {
      using Base = Inner::TRefBase<T>;
      using Base::Base;
      using Base::operator =;
      using Base::operator ==;

      // Single element selections                                      
      using Pick    = T;
      using PickMut = T;

      constexpr TRef() noexcept { this->ConstructDefault(); }
      constexpr TRef(nullptr_t) noexcept : TRef{} {}
      constexpr TRef(T* pointer) noexcept {
         if (pointer) {
            Base::SetHeapInner(pointer);
            Base::FindAllocationInner();
         }
         else this->ConstructDefault();
      }

      constexpr bool operator == (nullptr_t) const noexcept {
         return this->IsEmpty();
      }

      constexpr bool operator == (T* rhs) const noexcept {
         if (rhs == nullptr)
            return this->IsEmpty();
         return this->GetRaw() == rhs;
      }

      constexpr TRef& operator = (T* other) noexcept {
         if (other == Base::GetRaw())
            return *this;
         
         if (other) {
            Base::Free();
            Base::SetHeapInner(other);
            Base::FindAllocationInner();
         }
         else this->AssignDefault();
         return *this;
      }
   };
}
