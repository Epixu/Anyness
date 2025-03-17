#pragma once
#include "../Container.hpp"
#include <Langulus/CT/Index.hpp>


namespace Langulus::Anyness::Component
{

   template<auto COUNT>
   struct CountStatic {
      static_assert(COUNT > 0, "Can't have a container of zero or negative count");

      using CTTI_Component = Yes;
      using CountType = decltype(COUNT);
      using IndexType = Index::At<CountType>;

      constexpr bool IsEmpty()           { return COUNT == 0; }
      constexpr auto GetCount()          { return COUNT; }
      constexpr auto GetCapacity()       { return COUNT; }
      constexpr explicit operator bool() { return COUNT != 0; }
   };

} // namespace Langulus::Anyness::Component
