///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Concatenate.hpp"


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Adds operators for concatenation (+ and +=)                            
   ///   @tparam ID - heap we're inserting to                                 
   template<unsigned ID = 0>
   struct ConcatenateOperators {
      using CTTI_Component = Yes<>;
      static constexpr int ComponentPrecedence = 3000;

      /// Concatenate another container at the back, resulting in a new one   
      template<CT::Container C>
      C operator + (this C const& lhs, CT::Container auto&& rhs) {
         if (lhs.IsEmpty())
            return C {FWD(rhs)};

         C shallowCopy = lhs;
         shallowCopy.Concat(FWD(rhs));
         return shallowCopy;
      }

      /// Concatenate another container at the back, reusing this one         
      template<CT::Container C>
      C& operator += (this C& self, CT::Container auto&& rhs) {
         self.Concat(FWD(rhs));
         return self;
      }
   };
}
