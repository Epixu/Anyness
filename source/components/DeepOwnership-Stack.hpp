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
      AllocationPtr* mEntries = nullptr;

      /// Get entries array                                                   
      auto GetEntries() const noexcept { return mEntries; }

   public:
      constexpr DeepOwnershipStack() noexcept = default;
      constexpr DeepOwnershipStack(DeepOwnershipStack const& other) noexcept
         : mEntries {other.mEntries} {}
      constexpr DeepOwnershipStack(DeepOwnershipStack&& other) noexcept
         : mEntries {other.mEntries} {}
      constexpr DeepOwnershipStack(AllocationPtr* entries) noexcept
         : mEntries {entries} {}
   };

} // namespace Langulus::Anyness::Component