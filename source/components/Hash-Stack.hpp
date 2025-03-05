#pragma once
#include <Langulus/HashOf.hpp>


namespace Langulus::Anyness::Component
{

   template<class T = Hash, unsigned ID = 0>
   struct HashStack {
   private:
      T mHash;

   public:
      using CTTI_Component = Yes;

      constexpr T GetHash() const noexcept { return mHash; }
   };

} // namespace Langulus::Anyness::Component
