///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Merging.hpp"


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Adds operators for front (>>=) and back (<<=) merge                    
   ///   @tparam ID, SHARED operators that share the same insertion behavior. 
   ///   @attention this relies on Com::Merging being present                 
   template<Cid ID, Cid...SHARED>
   struct MergingOperators {
      using CTTI_Component = Yes<>;
      using CTTI_ReflectAs = void;
      using Id             = Values<ID, SHARED...>;

      static constexpr int ComponentPrecedence = 3000;

      /// Merge back                                                          
      template<CT::ContainsMany C, class A>
      C& operator <<= (this C& lhs, A&& rhs) {
         lhs.Merge(LglsFwd(rhs));
         return lhs;
      }

      /// Merge front                                                         
      template<CT::ContainsMany C, class A>
      C& operator >>= (this C& lhs, A&& rhs) {
         lhs.MergeAt(Index::Front, LglsFwd(rhs));
         return lhs;
      }
   };
}
