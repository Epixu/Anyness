#pragma once


namespace Langulus::Anyness::Component
{

   template<auto CAPACITY>
   struct CapacityStatic {
      using CTTI_Component = Yes;

      static_assert(CAPACITY > 0, "Can't have a container of zero or negative capacity");

      consteval auto GetCapacity() { return CAPACITY; }
   };

} // namespace Langulus::Anyness::Component
