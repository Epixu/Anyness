#pragma once
#include <Langulus/HashOf.hpp>


namespace Langulus::Anyness::Component
{

   template<class T = Hash, unsigned ID = 0, unsigned HEAP_ID = 0>
   struct HashHeap {
      using CTTI_Component = Yes;

      template<class Self>
      T GetHash(this const Self& self) noexcept {
         return self.GetHeap<HEAP_ID>().GetElement<T, ID>();
      }
   };

} // namespace Langulus::Anyness::Component
