#pragma once
#include "Concatenate.hpp"


namespace Langulus::Anyness::Component
{
   
   ///                                                                        
   /// Adds operators for concatenation (+ and +=)                            
   ///                                                                        
   struct ConcatenateOperators {
      using CTTI_Component = Yes<>;

      /// Push back                                                           
      template<CT::Container C>
      C operator + (this C&, CT::Container auto&&);

      /// Push front                                                          
      template<CT::Container C>
      C& operator += (this C&, CT::Container auto&&);
   };

} // namespace Langulus::Anyness::Component
