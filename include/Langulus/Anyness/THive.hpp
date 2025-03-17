///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../../../source/Container.hpp"
#include "../../../source/components/Heap-Immovable.hpp"
#include "../../../source/components/Heap-Movable.hpp"
#include "../../../source/components/Ownership-Stack.hpp"
#include "../../../source/components/DeepOwnership.hpp"
#include "../../../source/components/Emplacement.hpp"
#include "../../../source/components/Removal.hpp"
#include "../../../source/components/Assignment.hpp"
#include "../../../source/components/Typed-Static.hpp"
#include "../../../source/components/Count-Stack.hpp"
#include "../../../source/components/Reserve-Heap.hpp"


namespace Langulus::Anyness
{

   using DMeta = RTTI::DMeta;
      
   /// A statically-typed non-continuous container of variable size that      
   /// guarantees elements will never move in memory                          
   template<CT::NotVoid T>
   struct THive : Container<
      Component::HeapImmovable<>,      // Immovable heap memory         
      Component::OwnershipStack<>,     // Allocation is referenced      
      Component::DeepOwnership<>,      // Referenced indirections       
      Component::Emplacement,          // Allows emplacement            
      Component::Removal,              // Allows removal                
      Component::TypedStatic<DMeta, T>,// Statically typed              
      Component::CountStack<>,         // Variable count                
      Component::ReserveHeap<>         // Variable capacity             
   > {
      // Single element selections                                      
      using  PickDenseMut  = T&;
      using  PickDense     = T const&;
      struct PickSparseMut : Container<
         Component::HeapMovable<>,
         Component::OwnershipStack<>,
         Component::Assignment,
         Component::TypedStatic<DMeta, T>
      > {};
      using  PickSparse = T;
      using  Pick       = Tif<CT::Sparse<T>, PickSparse,    PickDense>;
      using  PickMut    = Tif<CT::Sparse<T>, PickSparseMut, PickDenseMut>;
   };

} // namespace Langulus::Anyness
