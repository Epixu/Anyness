#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// A static reserve                                                       
   ///                                                                        
   template<auto SIZE>
   struct ReserveStatic {
      using CTTI_Component = Yes;

      static_assert(SIZE > 0, "Can't have a container of zero or negative capacity");

      consteval auto GetReserved() { return SIZE; }
   };

} // namespace Langulus::Anyness::Component
