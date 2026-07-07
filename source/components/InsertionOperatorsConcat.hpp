///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Insertion.hpp"


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Adds operators for concatenation (+ and +=)                            
   ///   @tparam ID, SHARED operators that share the same insertion behavior. 
   ///   @attention this relies on Com::Insertion being present               
   template<Cid ID, Cid...SHARED>
   struct InsertionOperatorsConcat {
      using CTTI_Component = Yes<>;
      using CTTI_ReflectAs = void;
      using Id             = Values<ID, SHARED...>;

      static constexpr int ComponentPrecedence = 3000;

      /// Copy `lhs` and push `rhs` to the back                               
      template<CT::Container C>
      C operator + (this C const& lhs, CT::NotContainer auto&& rhs) {
         C temp {Copy {lhs}};
         temp.Insert(LglsFwd(rhs));
         return temp;
      }

      /// Insert `rhs` at the back                                            
      template<CT::Container C>
      C& operator += (this C& lhs, CT::NotContainer auto&& rhs) {
         lhs.Insert(LglsFwd(rhs));
         return lhs;
      }

      /// Concatenate another container at the back, resulting in a new one   
      template<CT::Container C>
      C operator + (this C const& lhs, CT::Container auto&& rhs) {
         if (lhs.IsEmpty())
            return C {LglsFwd(rhs)};

         C shallowCopy = lhs;
         shallowCopy.Concat(LglsFwd(rhs));
         return shallowCopy;
      }

      /// Concatenate another container at the back, reusing this one         
      template<CT::Container C>
      C& operator += (this C& self, CT::Container auto&& rhs) {
         self.Concat(LglsFwd(rhs));
         return self;
      }
   };
}
