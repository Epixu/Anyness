#pragma once
#include "../Container.hpp"
#include <Langulus/CT/Index.hpp>


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Provides random element access based on a linear index, that is        
   /// mapped directly onto contiguous memory                                 
   ///   @tparam T - constrain the type of allowed indices. Leave as 'void'   
   ///      to allow for all the usual integer types                          
   template<class T = void>
   struct IndexedLinear {
      using CTTI_Component = Yes;
      static constexpr bool Indexed = true;
   };

} // namespace Langulus::Anyness::Component
