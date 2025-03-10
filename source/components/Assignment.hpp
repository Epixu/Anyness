#pragma once
#include "../Container.hpp"
#include <Langulus/CT/Unfold.hpp>


namespace Langulus::Anyness::Component
{
   
   /// Check if container's elements are unfold-assignable                    
   ///   @attention type-erased elements are always assignable, and will fail 
   ///      at runtime if not reflected as such                               
   template<class C, class A>
   concept RangeAssignable = CT::Container<C> and (
      C::TypeErased or CT::UnfoldAssignable<TypeOf<C>, A>
   );


   ///                                                                        
   /// Implements assignment for containers                                   
   ///                                                                        
   struct Assignment {
      using CTTI_Component = Yes;

      template<CT::Container C, class A>
      void Fill(this C&, A&&) requires RangeAssignable<C, A>;
   };

} // namespace Langulus::Anyness::Component
