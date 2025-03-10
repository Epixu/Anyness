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
#include "../../../source/components/DeepOwnership.hpp"
#include "../../../source/components/Typed-Static.hpp"


namespace Langulus::Anyness
{

   ///                                                                        
   /// A statically typed shared pointer                                      
   ///                                                                        
   template<CT::NotVoid T>
   struct Ref : Detail::Container<
      Component::HeapMovable<>,        // Data on the heap              
      Component::OwnershipStack<>,     // Allocation is referenced      
      Component::DeepOwnership,        // Referenced indirections       
      Component::TypedStatic<DMeta, T> // Statically typed              
   > {};

} // namespace Langulus::Anyness
