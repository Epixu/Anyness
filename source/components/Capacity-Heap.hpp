#pragma once


namespace Langulus::Anyness::Component
{

   template<class T = ::std::size_t, unsigned ID = 0, unsigned HEAP_ID = 0>
   struct CapacityHeap {
      using CTTI_Component = Yes;

      template<class Self>
      T GetCapacity(this const Self& self) noexcept {
         return self.GetHeap<HEAP_ID>().GetElement<T, ID>();
      }
   };

} // namespace Langulus::Anyness::Component
