#pragma once
#include "Types.hpp"


namespace Langulus::CT
{

   template<class...T>
   concept Component = ((T::CTTI_ComponentTag) and ...);
   template<class...T>
   concept NotComponent = ((not Component<T>) and ...);

} // namespace Langulus::CT


namespace Langulus::Anyness::Detail
{

   template<CT::Component...COMPONENTS>
   struct Container : COMPONENTS... {

   };

} // namespace Langulus::Anyness::Detail
