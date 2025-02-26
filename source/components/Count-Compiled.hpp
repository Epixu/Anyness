#pragma once


namespace Langulus::Anyness::Components
{

   template<auto COUNT>
   struct CountCompiled {
      static constexpr bool CTTI_ComponentTag = true;
      static_assert(COUNT > 0, "Can't have a container of zero or negative count");

      consteval auto GetCount() {
         return COUNT;
      }

      consteval auto GetReserved() {
         return COUNT;
      }
   };

} // namespace Langulus::Anyness::Components
