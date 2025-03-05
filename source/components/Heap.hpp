#pragma once


namespace Langulus::Anyness::Component
{

   template<class T = void, unsigned ID = 0>
   struct Heap {
   private:
      T* mHeap;

   public:
      using CTTI_Component = Yes;
   };

} // namespace Langulus::Anyness::Component
