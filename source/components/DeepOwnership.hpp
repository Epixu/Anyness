#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Reserves a part of the heap to keep track of sparse element's          
   /// allocations                                                            
   ///   @tparam ID - which heap are we keeping track of?                     
   ///                                                                        
   template<unsigned ID = 0>
   struct DeepOwnership {
      using CTTI_Component = Yes;
   };

} // namespace Langulus::Anyness::Component
