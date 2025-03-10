#pragma once
#include "Insertion.hpp"


namespace Langulus::Anyness::Component
{
   
   ///                                                                        
   /// Implements merging for containers                                      
   ///                                                                        
   struct Merging {
   private:
      template<CT::Container C>
      using Count = typename C::CountType;
      template<CT::Container C>
      using Deep = typename C::DeepType;

   public:
      using CTTI_Component = Yes;

      /// Merging at specific index                                           
      template<class FORCE = Deep<C>, class A1, class...AN, CT::Container C>
      auto MergeAt(this C&, CT::Index auto, A1&&, AN&&...)
         -> Count<C> requires (C::Indexed and RangeInsertable<C, A1, AN...>);

      template<class FORCE = Deep<C>, class T, CT::Container C>
      auto MergeRangeAt(this C&, CT::Index auto, CT::Container auto&&)
         -> Count<C> requires C::Indexed;

      /// Generic merge                                                       
      template<class FORCE = Deep<C>, class A1, class...AN, CT::Container C>
      auto Merge(this C&, A1&&, AN&&...)
         -> Count<C> requires RangeInsertable<C, A1, AN...>;

      template<class FORCE = Deep<C>, class T, CT::Container C>
      auto MergeRange(this C&, CT::Container auto&&)
         -> Count<C>;
   };

} // namespace Langulus::Anyness::Component
