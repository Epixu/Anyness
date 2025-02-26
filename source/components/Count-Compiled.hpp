#pragma once


namespace Langulus::Anyness::Components
{

   template<auto COUNT>
   struct CountCompiled {
      using CTTI_Component = Yes;

      static_assert(COUNT > 0, "Can't have a container of zero or negative count");

      consteval auto GetCount() {
         return COUNT;
      }

      consteval auto GetReserved() {
         return COUNT;
      }
   };

} // namespace Langulus::Anyness::Components
