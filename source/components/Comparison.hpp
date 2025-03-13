#pragma once
#include "../Container.hpp"
#include <Langulus/CT/Character.hpp>
#include <Langulus/CT/Comparable.hpp>


namespace Langulus::Anyness
{
   
   /// Check if container's elements are comparable                           
   ///   @attention type-erased elements are always insertable, and will fail 
   ///      at runtime if not reflected as such                               
   template<class C, class T1, class...TN>
   concept RangeComparable = CT::Container<C> and (
      C::TypeErased or CT::Comparable<TypeOf<C>, T1, TN...>
   );

} // namespace Langulus::Anyness

namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Implements comparison for containers                                   
   ///                                                                        
   struct Comparison {
      using CTTI_Component = Yes;

   private:
      template<CT::Container C>
      using Count = typename C::CountType;

   public:
      template<CT::Container C1, CT::Container C2>
      bool operator == (this const C1&, const C2&);

      template<CT::Container C, CT::NotContainer T>
      bool operator == (this const C&, const T&) requires RangeComparable<C, T>;

      template<CT::Container C1, CT::Container C2>
      bool Compare(this const C1&, const C2&);
      template<CT::Container C1, CT::Container C2>
      auto Matches(this const C1&, const C2&) noexcept -> Count<C1>;

      template<CT::Container C1, CT::Container C2> requires CT::Character<TypeOf<C1>, TypeOf<C2>>
      bool CompareLoose(this const C1&, const C2&) noexcept;
      template<CT::Container C1, CT::Container C2> requires CT::Character<TypeOf<C1>, TypeOf<C2>>
      auto MatchesLoose(this const C1&, const C2&) noexcept -> Count<C1>;
   };

} // namespace Langulus::Anyness::Component
