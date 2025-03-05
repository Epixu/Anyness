///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../../../source/Container.hpp"
#include "../../../source/components/Heap.hpp"
#include "../../../source/components/Allocation-Stack.hpp"
#include "../../../source/components/Count-Static.hpp"
#include "../../../source/components/Ownership.hpp"
#include "../../../source/components/DeepOwnership.hpp"
#include "../../../source/components/Typed-Static.hpp"


namespace Langulus::Anyness
{

   /// A statically typed shared pointer                                      
   template<CT::NotVoid T>
   struct Ref : Detail::Container<
      Component::Heap<T>,              // Element on the heap           
      Component::AllocationStack<>,    // Pointer to an allocation      
      Component::Ownership,            // Allocation is referenced      
      Component::DeepOwnership,        // Referenced indirections       
      Component::TypedStatic<T>,       // Statically typed              
      Component::CountStatic<1>        // Statically sized to 1         
   > {};

} // namespace Langulus::Anyness
