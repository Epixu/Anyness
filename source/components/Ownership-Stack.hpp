#pragma once
#include "../Allocator.hpp"


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Keep a pointer to the heap allocation as a member                      
   /// Manage its ownership                                                   
   ///   @tparam ID - which heap are we keeping track of?                     
   ///   @tparam AUTO - whether ownership will be automatically used on       
   ///      construction/assignment. False if container is just a view, or in 
   ///      other cases where you want to carry an allocation pointer, but    
   ///      not necessarily reference it                                      
   ///                                                                        
   template<unsigned ID = 0, bool AUTO = true>
   struct OwnershipStack {
   protected:
      AllocationPtr mAllocation = nullptr;

   public:
      using CTTI_Component = Yes;
      static constexpr bool Owned = AUTO;

      /// Get the allocation                                                  
      auto GetAllocation() const noexcept {
         return mAllocation;
      }

      /// Get the memory reference count                                      
      auto GetUses() const noexcept {
         return mAllocation ? mAllocation->GetUses() : 0;
      }

   protected:
      template<unsigned>
      friend struct HeapMovable;
      template<unsigned>
      friend struct DeepOwnershipHeap;

      void SetAllocation(AllocationPtr a) noexcept { mAllocation = a; }
      void Keep() const noexcept;
      void Free() const noexcept;
   };
   
} // namespace Langulus::Anyness::Component
