#pragma once


namespace Langulus::Anyness::Component
{

   template<class T = ::std::size_t, unsigned ID = 0>
   struct Stack {
   private:
      T mStack;

   public:
      using CTTI_Component = Yes;
   };

} // namespace Langulus::Anyness::Component
