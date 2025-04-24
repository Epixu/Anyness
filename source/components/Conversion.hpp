#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Implements conversion for containers                                   
   ///                                                                        
   struct Conversion {
      using CTTI_Component = Yes;

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

} // namespace Langulus::Anyness::Component
