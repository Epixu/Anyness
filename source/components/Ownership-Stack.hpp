#pragma once
#include "../Allocator.hpp"


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Keep a pointer to the heap allocation as a member                      
   /// Manage its ownership                                                   
   ///   @tparam ID - which heap are we keeping track of?                     
   ///                                                                        
   template<unsigned ID = 0>
   struct OwnershipStack {
   private:
      AllocationPtr mAllocation;

   public:
      using CTTI_Component = Yes;

      auto GetAllocation() const noexcept { return mAllocation; }

      auto GetUses() const noexcept {
         return mAllocation ? mAllocation->GetUses() : 0;
      }
   };
   
   ///                                                                        
   /// Keep a pointer to the heap allocation as a member                      
   /// Just for padding to keep binary-compatibility - doesn't reference      
   ///   @tparam ID - which heap are we keeping track of?                     
   ///                                                                        
   template<unsigned ID = 0>
   struct NoOwnershipStack {
   private:
      AllocationPtr mAllocation;

   public:
      using CTTI_Component = Yes;

      auto GetAllocation() const noexcept { return mAllocation; }
   };

} // namespace Langulus::Anyness::Component
