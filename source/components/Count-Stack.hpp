#pragma once


namespace Langulus::Anyness::Components
{

   template<class T, unsigned ID>
   struct CountStack {
   private:
      T mCount;

   public:
      using CTTI_Component = Yes;

      constexpr T GetCount() const noexcept {
         return mCount;
      }
   };

} // namespace Langulus::Anyness::Components
