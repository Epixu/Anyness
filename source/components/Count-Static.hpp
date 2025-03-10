#pragma once


namespace Langulus::Anyness::Component
{

   template<auto COUNT>
   struct CountStatic {
      using CTTI_Component = Yes;

      static_assert(COUNT > 0, "Can't have a container of zero or negative count");

      consteval bool IsEmpty()     { return COUNT == 0; }
      consteval auto GetCount()    { return COUNT; }
      consteval auto GetCapacity() { return COUNT; }
      consteval explicit operator bool() { return COUNT != 0; }
   };

} // namespace Langulus::Anyness::Component
