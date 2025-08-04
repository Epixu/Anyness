///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
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
      using CTTI_Component = Yes<>;
      static constexpr bool Owned = AUTO;

      /// Get the allocation                                                  
      auto GetAllocation() const noexcept {
         return mAllocation;
      }

      /// Get the memory reference count                                      
      auto GetUses() const noexcept {
         return mAllocation ? mAllocation->GetUses() : 0;
      }

      /// Shallow-copy all initialized elements in memory to another          
      /// allocation, that is owned once only by this container               
      ///   @attention if we already own the memory just Keep() it once       
      template<CT::Container C> requires C::HeapAllocated   
      void TakeOwnership(this C& self) {
         if (not self.GetRaw())
            return;

         if (self.mAllocation) {
            // We already have authority                                
            self.mAllocation->Keep();
            return;
         }

         // Shallow-copy all elements                                   
         C temp {Copy {self}};
         self = Abandon {temp};
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
}
