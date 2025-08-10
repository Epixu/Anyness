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
   /// Adds a variable to a container                                         
   /// Increases the container's bytesize                                     
   ///   @tparam T - type of the variable                                     
   ///   @tparam ID - multiple variables are supported                        
   ///   @attention same IDs serve to identify heap components as well, so    
   ///      make sure they don't overlap                                      
   ///                                                                        
   template<CT::NotVoid T, unsigned ID = 0>
   struct Stack {
      using CTTI_Component = Yes<>;
      static constexpr int ComponentPrecedence = -2000;

   protected:
      T mStack;
   };
}
