///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include <Langulus/Utils/Iterate-Handles.hpp>
#include "../../../source/Container.hpp"
#include "../../../source/components/Heap-Immovable.hpp"
#include "../../../source/components/Ownership-Stack.hpp"
#include "../../../source/components/DeepOwnership-Heap.hpp"
#include "../../../source/components/Emplacement.hpp"
#include "../../../source/components/Removal.hpp"
#include "../../../source/components/Typed-Static.hpp"
#include "../../../source/components/Count-Stack.hpp"
#include "../../../source/components/Reserve-Heap.hpp"
#include "THandle.hpp"


namespace Langulus::Anyness
{

   ///                                                                        
   /// A statically-typed non-continuous container of variable size that      
   /// guarantees elements will never move from the memory they were first    
   /// instantiated in                                                        
   ///                                                                        
   template<CT::NotVoid T>
   struct THive : Container<
      Com::HeapImmovable<>,            // Immovable heap memory         
      Com::OwnershipStack<>,           // Allocation is referenced      
      Com::DeepOwnershipHeap<>,        // Referenced indirections       
      Com::Emplacement<>,              // Allows emplacement            
      Com::Removal<>,                  // Allows removal                
      Com::TypedStatic<DMeta, T>,      // Statically typed              
      Com::CountStack<>,               // Variable count                
      Com::ReserveHeap<>               // Variable capacity             
   > {
      using PickDenseMut  = T&;
      using PickDense     = T const&;
      using PickSparseMut = THandle<T&>;
      using PickSparse    = THandle<T const&>;
      using Pick          = Tif<CT::Sparse<T>, PickSparse,    PickDense>;
      using PickMut       = Tif<CT::Sparse<T>, PickSparseMut, PickDenseMut>;
   };

} // namespace Langulus::Anyness
