#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Adds a variable to a container                                         
   /// Increases the container's bytesize                                     
   ///   @tparam T - type of the variable                                     
   ///   @tparam ID - multiple variables are supported                        
   ///                                                                        
   template<CT::NotVoid T, unsigned ID = 0>
   struct Stack {
   protected:
      T mStack;

   public:
      using CTTI_Component = Yes;
   };

} // namespace Langulus::Anyness::Component
