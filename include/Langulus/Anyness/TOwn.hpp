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
#include "../../../source/components/Stack.hpp"
#include "../../../source/components/Count-Static.hpp"
#include "../../../source/components/Emplacement.hpp"
#include "../../../source/components/Assignment.hpp"
#include "../../../source/components/Comparison.hpp"


namespace Langulus::Anyness::Inner
{
   template<CT::NotVoid T>
   using TOwnBase = Container<
      Com::TypedStatic<DMeta, T>,         // Statically typed           
      Com::Stack<T>,                      // Element on the stack       
      Com::CountStatic<1u>,               // Statically sized           
      Com::Emplacement<>,                 // Can be emplaced            
      Com::Assignment<>,                  // Can be reassigned          
      Com::Comparison<>                   // Can be compared            
   >;
}

namespace Langulus::Anyness
{
   ///                                                                        
   /// A statically typed stack-based container of size 1.                    
   /// Mainly serves to transfer values and/or pointers on move.              
   template<CT::NotVoid T>
   struct TOwn : Inner::TOwnBase<T> {
      using Base = Inner::TOwnBase<T>;
      using Base::Base;
      using Base::operator =;
      using Com::Assignment<>::operator =;
      using Base::operator ==;

      constexpr TOwn() noexcept { this->ConstructDefault(); }
      constexpr TOwn(const T& source)
         : Base {typename Base::Stackwise {}, source} {}
      constexpr TOwn(T&& source) noexcept
         : Base {typename Base::Stackwise {}, FWD(source)} {}
      constexpr ~TOwn() {}
      
      constexpr bool operator == (const T& rhs) const noexcept {
         return this->GetStackInner() == rhs;
      }
   };
}
