#pragma once
#include "../Container.hpp"
#include "../Iterator.hpp"


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Implements ranged iteration interface for containers                   
   ///                                                                        
   struct IterationRange {
      using CTTI_Component = Yes;

   private:
      template<CT::Container C>
      using Count = typename Deref<C>::CountType;
      
   public:
      template<CT::Container C>
      constexpr auto begin(this C&& self) noexcept -> TIterator<Deref<C>>;

      template<CT::Container C>
      constexpr auto last(this C&& self) noexcept -> TIterator<Deref<C>>;

      constexpr IteratorEnd end() const noexcept { return {}; }
   };

} // namespace Langulus::Anyness::Component
