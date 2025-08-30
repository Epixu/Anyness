///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../../../source/Container.hpp"
#include "../../../source/components/Heap-Reference.hpp"
#include "../../../source/components/Ownership-Stack.hpp"
#include "../../../source/components/Indexed-Linear.hpp"
#include "../../../source/components/Typed-Static.hpp"
#include "../../../source/components/Count-Stack.hpp"
#include "../../../source/components/Hash-Stack.hpp"
#include "../../../source/components/Iteration-ForEach.hpp"
#include "../../../source/components/Iteration-Range.hpp"
#include "../../../source/components/Comparison.hpp"
#include "../../../source/components/Conversion.hpp"


namespace Langulus::Anyness
{
   namespace Inner
   {
      template<class T>
      using TViewBase = Container<
         Com::TypedStatic<DMeta, T>,         // Type-constrained        
         Com::HeapReference<>,               // Pointer to heap memory  
         Com::OwnershipStack<0, false>,      // Pointer to an allocation
         Com::CountStack<>,                  // Variable count          
         Com::HashEmergent<>,                // Emergent hash           
         Com::Comparison,                    // Allows for comparisons  
         Com::Conversion,                    // Allows conversions      
         Com::IndexedLinear<>,               // Indexed directly        
         Com::IterationForEach<>,            // ForEach iteration       
         Com::IterationRange<>,              // Ranged iteration        
         // Assignment is allowed only if T is mutable                  
         Com::Assignment<>
      >;
   }

   ///                                                                        
   /// A lightweight container view of variable size.                         
   /// If T is constant, the view becomes immutable.                          
   template<class T>
   struct TView : Inner::TViewBase<T> {
      // Single element selections                                      
      using Pick    = T const&;
      using PickMut = T&;

      using Base = Inner::TViewBase<T>;
      using Base::Base;
      using Base::operator =;
      using Base::operator ==;
   };
}
