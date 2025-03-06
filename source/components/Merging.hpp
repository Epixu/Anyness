#pragma once
#include "../Container.hpp"
#include "Insertion.hpp"


namespace Langulus::Anyness::Component
{
   
   ///                                                                        
   /// Implements merging for containers                                      
   ///                                                                        
   struct Merging {
      using CTTI_Component = Yes;

      template<CT::Container SELF, class FORCE = typename SELF::DeepType, class T1, class...TN>
      auto Merge(this SELF&, CT::Index auto, T1&&, TN&&...)
         -> typename SELF::Count requires RangeInsertable<SELF, T1, TN...>;

      template<CT::Container SELF, class FORCE = typename SELF::DeepType, class T>
      auto MergeRange(this SELF&, CT::Index auto, CT::Container auto&&)
         -> typename SELF::Count;
   };

} // namespace Langulus::Anyness::Component
