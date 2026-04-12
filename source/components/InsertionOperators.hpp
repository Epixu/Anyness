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
   /// Adds operators for front (>>) and back (<<) insertion                  
   ///   @tparam ID heap we're inserting to                                   
   ///   @tparam AS type to serialize as before inserting. Useful for byte    
   ///      and text containers. Use void to insert without serialization     
   template<Cid ID, class AS, Cid...SHARED>
   struct InsertionOperators {
      using CTTI_Component = Yes<>;

      static constexpr Cid Id = ID;
      static constexpr int ComponentPrecedence = 3000;

      /// Push back                                                           
      template<CT::Container C, class A>
      C& operator << (this C&, A&&) requires CT::RangeInsertable<C, A>;

      /// Push front                                                          
      template<CT::Container C, class A>
      C& operator >> (this C&, A&&) requires CT::RangeInsertable<C, A>;

      /// Copy left side and push back rhs                                    
      template<CT::Container C>
      C operator + (this C const& lhs, CT::NotContainer auto&& rhs) {
         return C {Copy {lhs}} << LglsFwd(rhs);
      }

      /// Same as push back operator (<<)                                     
      template<CT::Container C>
      C& operator += (this C& self, CT::NotContainer auto&& rhs) {
         return self << LglsFwd(rhs);
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
