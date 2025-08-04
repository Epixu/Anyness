///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Marks a container as contiguous                                        
   /// Allows for a plethora of batch optimizations                           
   ///                                                                        
   struct Contiguous {
      using CTTI_Component  = Yes<>;
      using CTTI_Contiguous = Yes<>;
   };
}
