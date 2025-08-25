///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Allocator.hpp"

#if not LANGULUS_FEATURE(MANAGED_MEMORY)
   #error "This file shouldn't be included if managed memory is disabled"
#endif


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Use the memory manager to extract the allocation from heap pointer.    
   /// Manage its ownership.                                                  
   ///   @tparam ID - which heap are we keeping track of?                     
   ///   @tparam AUTO - whether ownership will be automatically used on       
   ///      construction/assignment. False if container is just a view, or in 
   ///      other cases where you want to carry an allocation pointer, but    
   ///      not necessarily reference it                                      
   template<unsigned ID = 0, bool AUTO = true>
   struct OwnershipEmergent {
      using CTTI_Component = Yes<>;
      static constexpr bool Owned = AUTO;
      static constexpr int ComponentPrecedence = -1000;

      /// Get the allocation                                                  
      auto GetAllocation(this auto const& self) noexcept -> AllocationPtr {
         return Allocator::Find(self.GetType(), self.GetHeapInner());
      }

      /// Get the memory reference count                                      
      auto GetUses(this auto const& self) noexcept {
         auto allocation = self.GetAllocation();
         return allocation ? allocation->GetUses() : 0;
      }

   protected:
      void Keep() const noexcept;
      void Free() const noexcept;
   };
}
