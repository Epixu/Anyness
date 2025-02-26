#pragma once


namespace Langulus::Anyness::Components
{

   template<class T, unsigned ID>
   struct CountStack {
   private:
      T mCount;

   public:
      static constexpr bool CTTI_ComponentTag = true;

      constexpr T GetCount() const noexcept {
         return mCount;
      }
   };

} // namespace Langulus::Anyness::Components
