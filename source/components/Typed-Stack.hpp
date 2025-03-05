#pragma once


namespace Langulus::Anyness::Component
{

   template<class T, unsigned ID = 0>
   struct TypedStack {
   private:
      T mType;

   public:
      using CTTI_Component = Yes;

      constexpr T GetType() const noexcept { return mType; }
   };

} // namespace Langulus::Anyness::Component
