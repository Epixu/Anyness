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
      using CTTI_ReflectAs = void;

      static constexpr Cid Id = ID;
      static constexpr int ComponentPrecedence = 3000;

      /// Push back                                                           
      template<CT::Container C, class A>
      C& operator << (this C&, A&&) requires CT::RangeInsertable<C, A>;

      /// Push front                                                          
      template<CT::Container C, class A>
      C& operator >> (this C&, A&&) requires CT::RangeInsertable<C, A>;
   };
}
