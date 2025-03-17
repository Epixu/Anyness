#pragma once
#include "../Container.hpp"
#include <Langulus/CT/Index.hpp>


namespace Langulus::Anyness::Component
{

   template<unsigned ID = 0, class T = ::std::size_t>
   struct CountHeap {
      using CTTI_Component = Yes;
      using CountType = T;
      using IndexType = Index::At<T>;

      T GetCount(this const auto& self) noexcept {
         return self.GetHeap<ID>().GetElement<T>();
      }

      bool IsEmpty(this const auto& self) noexcept {
         return self.GetCount() == 0;
      }

      explicit operator bool(this const auto& self) noexcept {
         return self.GetCount() != 0;
      }
   };

} // namespace Langulus::Anyness::Component
