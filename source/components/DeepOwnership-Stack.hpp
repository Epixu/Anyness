#pragma once
#include "../Container.hpp"
#include <Langulus/Assume.hpp>
#include <Langulus/CT/Allocatable.hpp>
#include <Langulus/CT/Referenced.hpp>


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Manages deep ownership by holding a pointer to the entries locally     
   ///   @tparam ID - which heap are we keeping track of?                     
   ///                                                                        
   template<unsigned ID = 0>
   struct DeepOwnershipStack {
      using CTTI_Component = Yes;
      static constexpr bool DeeplyOwned = true;

   protected:
      AllocationPtr* mEntries;

      /// Get entries array                                                   
      auto GetEntries() const noexcept { return mEntries; }
   };

} // namespace Langulus::Anyness::Component