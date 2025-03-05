#pragma once


namespace Langulus::Anyness::Component
{

   template<auto COUNT>
   struct CountStatic {
      using CTTI_Component = Yes;

      static_assert(COUNT > 0, "Can't have a container of zero or negative count");

      consteval auto GetCount()    { return COUNT; }
      consteval auto GetCapacity() { return COUNT; }
   };

} // namespace Langulus::Anyness::Component
