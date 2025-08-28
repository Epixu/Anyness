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
   /// Implements insertion for containers                                    
   ///   @tparam ID - heap we're inserting to                                 
   template<unsigned ID>
   struct Concatenate {
      using CTTI_Component = Yes<>;
      static constexpr int ComponentPrecedence = 3000;

   private:
      template<CT::Container C>
      using Count = typename C::CountType;
      template<CT::Container C>
      using Deep = typename C::DeepType;

   public:
      /// Concatenation at specific index                                     
      template<bool FORCE = true, CT::IndexedLinearly C>
      auto ConcatAt(this C&, CT::Index auto, CT::Container auto&&)
         -> Count<C>;

      /// Concatenation at the back                                           
      template<bool FORCE = true, CT::Container C>
      auto Concat(this C&, CT::Container auto&&)
         -> Count<C>;
   };
}
