#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Implements ForEach iteration interface for containers                  
   ///                                                                        
   struct IterationForEach {
      using CTTI_Component = Yes;

      template<CT::Container C>
      using Count = typename Deref<C>::CountType;
      
      template<CT::Container C>
      auto ForEachElement(this C&&, auto&&...) -> Count<C>;
      template<CT::Container C>
      auto ForEachElementRev(this C&&, auto&&...) -> Count<C>;

      template<CT::Container C>
      auto ForEach(this C&&, auto&&...) -> Count<C>;
      template<CT::Container C>
      auto ForEachRev(this C&&, auto&&...) -> Count<C>;

      template<bool SKIP = true, CT::Container C>
      auto ForEachDeep(this C&&, auto&&...) -> Count<C>;
      template<bool SKIP = true, CT::Container C>
      auto ForEachDeepRev(this C&&, auto&&...) -> Count<C>;
   };

} // namespace Langulus::Anyness::Component
