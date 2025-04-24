#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Marks a container as contiguous                                        
   /// Allows for a plethora of batch optimizations                           
   ///                                                                        
   struct Contiguous {
      using CTTI_Component  = Yes;
      using CTTI_Contiguous = Yes;
   };

} // namespace Langulus::Anyness::Component