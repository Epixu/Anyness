#pragma once


namespace Langulus::Anyness::Component
{

   template<unsigned ID = 0, class T = ::std::size_t>
   struct CountStack {
   private:
      T mCount;

   public:
      using CTTI_Component = Yes;

      constexpr bool IsEmpty()  const noexcept      { return mCount == 0; }
      constexpr auto GetCount() const noexcept -> T { return mCount; }
      explicit operator bool()  const noexcept      { return mCount != 0; }
   };

} // namespace Langulus::Anyness::Component
