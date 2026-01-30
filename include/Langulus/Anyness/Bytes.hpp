///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../../../source/Container.hpp"
#include "../../../source/components/Heap-Movable.hpp"
#include "../../../source/components/Ownership-Stack.hpp"
#include "../../../source/components/IndexedLinear.hpp"
#include "../../../source/components/Emplacement.hpp"
#include "../../../source/components/Insertion.hpp"
#include "../../../source/components/InsertionOperators.hpp"
#include "../../../source/components/Removal.hpp"
#include "../../../source/components/Assignment.hpp"
#include "../../../source/components/Typed-Static.hpp"
#include "../../../source/components/Count-Stack.hpp"
#include "../../../source/components/Reserve-Emergent.hpp"
#include "../../../source/components/Hash-Stack.hpp"
#include "../../../source/components/Iteration-Range.hpp"
#include "../../../source/components/Comparison.hpp"
#include <Langulus/Utils/Byte.hpp>


namespace Langulus::Anyness
{
   struct Bytes;

   namespace Inner
   {
      using BytesBase = Container<
         Com::TypedStatic<DMeta, Byte>,   // Type-constrained           
         Com::HeapMovable<>,              // Pointer to heap memory     
         Com::CountStack<>,               // Variable count             
         Com::ReserveEmergent<>,          // Capacity derived from alloc
         Com::OwnershipStack<>,           // Allocation is referenced   
         Com::HashStack<>,                // Variable hash (cached)     
         Com::Insertion<0, Bytes>,        // Serialize + insert         
         Com::InsertionOperators<0, Bytes>,// << and >> insertion       
         Com::Removal<>,                  // Allows removal             
         Com::Assignment<>,               // Allows assignment          
         Com::Comparison<>,               // Allows for comparison      
         Com::Conversion,                 // Allows conversion          
         Com::IndexedLinear<>,            // Indexed directly           
         Com::IterationForEach<>,         // ForEach iteration          
         Com::IterationRange<>            // Range iteration            
      >;
   }
   
   ///                                                                        
   /// A continuous byte container of variable size                           
   struct Bytes : Inner::BytesBase {
      using CountType = Base::CountType;

      // Single element selections                                      
      using Pick    = Byte const&;
      using PickMut = Byte&;

      constexpr Bytes() noexcept {
         this->ConstructDefault();
      }
      constexpr Bytes(Bytes const& other) {
         this->Absorb(Refer {other});
      }
      constexpr Bytes(Bytes&& other) noexcept {
         this->Absorb(Move {other});
      }
      constexpr ~Bytes() noexcept {
         this->Destroy();
      }
      
      /// Construction from any kind of other bytes with intent               
      template<template<class> class I> requires CT::Intent<I<Bytes>>
      constexpr Bytes(I<Bytes>&& bytes) {
         this->Absorb(LglsFwd(bytes));
      }
      
      /// Assignment                                                          
      constexpr Bytes& operator = (Bytes const& other) {
         return this->AssignAbsorb(Refer {other});
      }
      constexpr Bytes& operator = (Bytes&& other) noexcept {
         return this->AssignAbsorb(Move {other});
      }
      
      /// Comparison                                                          
      constexpr auto operator <=> (Bytes const& other) const noexcept -> ::std::partial_ordering {
         return this->Compare(other);
      }
      constexpr bool operator == (Bytes const& other) const noexcept {
         return this->CompareEqual(other);
      }
   };
}
