#pragma once
#include "../Container.hpp"
#include <Langulus/CT/Index.hpp>


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Defines count as a member, increasing container bytesize               
   ///   @tparam ID - the heap/stack ID to keep count of                      
   ///   @tparam T - the count type                                           
   template<unsigned ID = 0, class T = ::std::size_t>
   struct CountStack {
   private:
      T mCount;

   public:
      using CTTI_Component = Yes;
      using CountType = T;
      using IndexType = Index::At<T>;

      constexpr bool IsEmpty()  const noexcept { return mCount == 0; }
      constexpr T    GetCount() const noexcept { return mCount; }
      explicit operator bool()  const noexcept { return mCount != 0; }

      T GetCountDeep() const noexcept;
      T GetCountItemsDeep() const noexcept;
   };

} // namespace Langulus::Anyness::Component
