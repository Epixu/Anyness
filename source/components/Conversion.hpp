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
   /// Implements conversion for containers                                   
   ///                                                                        
   struct Conversion {
      using CTTI_Component = Yes<>;

   private:
      template<CT::Container C>
      using Count = typename C::CountType;
      template<CT::Container C>
      using At = typename C::IndexType;

   public:
      template<CT::Container C, CT::NotContainer TO>
      bool ConvertTo(this const C&, TO&);

      template<CT::Container C, CT::Container TO>
      bool ConvertTo(this const C&, TO&);
   };
}
