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
   /// Implements merging for containers                                      
   struct Merging {
   private:
      template<CT::Container C>
      using Count = typename C::CountType;
      template<CT::Container C>
      using Deep = typename C::DeepType;

   public:
      using CTTI_Component = Yes<>;
      static constexpr int ComponentPrecedence = 3000;

      /// Merging at specific index                                           
      template<bool FORCE = true, class A1, class...AN, CT::IndexedLinearly C>
      auto MergeAt(this C&, CT::Index auto, A1&&, AN&&...)
         -> Count<C> requires RangeInsertable<C, A1, AN...>;

      template<bool FORCE = true, CT::IndexedLinearly C>
      auto MergeRangeAt(this C&, CT::Index auto, CT::Container auto&&)
         -> Count<C>;

      /// Generic merge                                                       
      template<bool FORCE = true, class A1, class...AN, CT::Container C>
      auto Merge(this C&, A1&&, AN&&...)
         -> Count<C> requires RangeInsertable<C, A1, AN...>;

      template<bool FORCE = true, CT::Container C>
      auto MergeRange(this C&, CT::Container auto&&)
         -> Count<C>;
   };
}
