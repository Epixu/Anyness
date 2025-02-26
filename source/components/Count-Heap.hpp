#pragma once


namespace Langulus::Anyness::Components
{

   template<class T, unsigned ID, unsigned HEAP_ID>
   struct CountHeap {
      static constexpr bool CTTI_ComponentTag = true;

      template<class Self>
      T GetCount(this const Self& self) noexcept {
         return self.GetHeap<HEAP_ID>().GetElement<T, ID>();
      }
   };

} // namespace Langulus::Anyness::Components
