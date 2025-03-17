#pragma once
#include "../Container.hpp"
#include <Langulus/HashOf.hpp>


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Provides random element access by hashing a value of type T            
   ///   @tparam T - type to get hash of, use void for type-erasure           
   ///   @tparam HASH - type of the hash                                      
   template<class T = void, class HASH = Hash>
   struct IndexedHash {
      using CTTI_Component = Yes;
   };

} // namespace Langulus::Anyness::Component
