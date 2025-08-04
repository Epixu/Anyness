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
   ///                                                                        
   struct ConcatenateOperators {
      using CTTI_Component = Yes<>;

      /// Push back                                                           
      template<CT::Container C>
      C operator + (this C&, CT::Container auto&&);

      /// Push front                                                          
      template<CT::Container C>
      C& operator += (this C&, CT::Container auto&&);
   };
}
