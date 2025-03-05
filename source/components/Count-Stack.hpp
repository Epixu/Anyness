#pragma once


namespace Langulus::Anyness::Component
{

   template<class T = ::std::size_t, unsigned ID = 0>
   struct CountStack {
   private:
      T mCount;

   public:
      using CTTI_Component = Yes;

      constexpr T GetCount() const noexcept { return mCount; }
   };

} // namespace Langulus::Anyness::Component
