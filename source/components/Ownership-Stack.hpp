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
   private:
      // The allocation interface on the stack                          
      // It is private so that it isn't accessible when inherited       
      // It has to be accessed through GetAllocation()/SetAllocation()  
      AllocationPtr mAllocation;

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
      template<unsigned>
      friend struct Removal;

      /// Set the allocation                                                  
      void SetAllocation(AllocationPtr allocation) noexcept {
         mAllocation = allocation;
      }

      /// Reference memory block once                                         
      ///   @param DEEP - reference inner pointers/referenced instances, too? 
      template<CT::Container C>
      void Keep(this C const& self) noexcept {
         if (not self.mAllocation)
            return;

         self.mAllocation->Keep(1);

         // Keep elements, if DeepOwnership component exists            
         if constexpr (requires { self.KeepDeep(); })
            self.KeepDeep();
      }
      
      /// Dereference memory block once and destroy all elements if data was  
      /// fully dereferenced                                                  
      ///   @attention this never modifies any state, except ownership        
      template<CT::Container C>
      void Free(this C& self) noexcept {
         if (not self.mAllocation)
            return;

         LglsAssumeDev(self.mAllocation->GetUses() >= 1,
            "Bad memory dereferencing");

         if (self.mAllocation->GetUses() == 1) {
            // Free elements, if DeepOwnership component exists         
            if constexpr (requires { self.FreeDeep(); })
               self.FreeDeep();

            // Free memory                                              
            Allocator::Deallocate(self.mAllocation);
         }
         else {
            // Free elements, if DeepOwnership component exists         
            // Notice that no element will be destroyed, because in this
            // case we have a guarantee, that elements are referenced   
            // from elsewhere as well                                   
            if constexpr (requires { self.FreeDeep(); })
               self.template FreeDeep<false>();

            // Dereference memory                                       
            self.mAllocation->Free();
         }
         
         self.mAllocation = nullptr;
      }
   };
}
