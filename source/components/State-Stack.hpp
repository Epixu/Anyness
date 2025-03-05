#pragma once
#include "../Container.hpp"
#include <bitset>


namespace Langulus::Anyness::Component
{

   template<CT::State...STATES>
   struct StateStack {
      using CTTI_Component = Yes;
      static constexpr ::std::size_t Count = sizeof...(STATES);
      using State = ::std::bitset<Count>;

   private:
      State mState;

   public:
      constexpr auto GetState() const noexcept { return mState; }
   };

} // namespace Langulus::Anyness::Component
