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
   /// Implements removal for containers                                      
   ///   @tparam ID - heap we're removing from                                
   template<unsigned ID = 0>
   struct Removal {
      using CTTI_Component = Yes<>;

   private:
      template<CT::Container C>
      using Count = typename C::CountType;
      template<CT::Container C>
      using Iterator = typename C::Iterator;

   public:
      template<bool REVERSE = false, CT::Container C>
      auto Remove(this C&, const CT::NoIntent auto&) -> Count<C>;

      template<CT::Container C>
      auto RemoveAt(this C&, CT::Index auto, Count<C> = 1) -> Count<C>;

      template<CT::Container C>
      auto RemoveAtDeep(this C&, CT::Index auto) -> Count<C>;

      template<CT::Container C>
      auto RemoveIt(this C&, const Iterator<C>&, Count<C> = 1) -> Iterator<C>;

      template<CT::Container C>
      void Trim(this C&, Count<C>);

      template<CT::Container C>
      void Optimize(this C&);

      template<CT::Container C>
      void Clear(this C&);

      template<CT::Container C>
      void Reset(this C&);
   };
}
