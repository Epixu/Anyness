#pragma once
#include <Langulus/CT.hpp>


namespace Langulus::CTTI
{

   /// Can be used in two ways to satisfy CT::Component<T>:                   
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Component = Yes;` in T                     
   template<class T>
   struct Component {
      static constexpr bool Value = false;
   };
   
   /// Can be used in two ways to satisfy CT::Container<T>:                   
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Container = Yes;` in T                     
   template<class T>
   struct Container {
      static constexpr bool Value = false;
   };

} // namespace Langulus::CTTI

LANGULUS_CTTI_CONCEPT(Component);
LANGULUS_CTTI_CONCEPT(Container);

namespace Langulus::Anyness::Detail
{

   template<CT::Component...COMPONENTS>
   struct Container : COMPONENTS... {
      using CTTI_Container = Yes;
   };

} // namespace Langulus::Anyness::Detail
