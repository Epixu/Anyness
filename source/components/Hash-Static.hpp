#pragma once


namespace Langulus::Anyness::Component
{

   template<auto HASH>
   struct HashStatic {
      using CTTI_Component = Yes;

      consteval auto GetHash() { return HASH; }
   };

} // namespace Langulus::Anyness::Component
