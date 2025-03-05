#pragma once


namespace Langulus::Anyness::Component
{

   template<class T = ::std::size_t, unsigned ID = 0>
   struct CapacityStack {
   private:
      T mCapacity;

   public:
      using CTTI_Component = Yes;

      constexpr T GetCapacity() const noexcept { return mCapacity; }
   };

} // namespace Langulus::Anyness::Component
