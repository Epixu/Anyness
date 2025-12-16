///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Ownership-Emergent.hpp"


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Keep a pointer to the heap allocation as a member.                     
   /// Manage its ownership by referencing and dereferencing it.              
   /// Can also reference on per-element basis if enabled via DEEPREF.        
   ///   @tparam ID which heap are we keeping track of?                       
   ///   @tparam AUTO whether ownership will be automatically applied on      
   ///      construction, reassignment and destruction. False if container is 
   ///      just a view, or in other cases where you want to carry an         
   ///      allocation pointer, but not necessarily reference it.             
   ///   @tparam DEEPREF whether to reference individual elements.            
   template<unsigned ID, bool AUTO, bool DEEPREF>
   struct OwnershipStack : OwnershipEmergent<ID, AUTO, DEEPREF> {
      using StackRequest = AllocationPtr;

      /// Get the allocation                                                  
      auto GetAllocation(this auto const& self) noexcept {
         return self.GetAllocationInner();
      }

      /// Shallow-copy all initialized elements in memory to another          
      /// allocation, that is owned once only by this container.              
      ///   @attention if we already own the memory just Keep() it once       
      template<CT::Container C> requires C::HeapAllocated   
      void TakeOwnership(this C& self) {
         if (not self.GetHeapInner())
            return;

         auto& a = self.GetAllocationInner();
         if (a)
            return; // We already own this allocation                   

      #if LANGULUS_FEATURE(MANAGED_MEMORY)
         // The heap might already be ours and we just don't know it    
         if (auto found = Allocator::Find(self.GetHeapInner())) {
            a = const_cast<AllocationPtr>(found);
            a->AddRef(1);
            return;
         }
      #endif

         // Shallow-copy all elements in a fresh allocation             
         C temp {Copy {self}};
         self = Abandon {temp};
      }

   protected:
      template<unsigned> friend struct HeapMovable;
      template<unsigned> friend struct Removal;
      template<unsigned> friend struct Emplacement;

      /// Get allocation (inner)                                              
      ///   @attention may be uninitialized                                   
      constexpr auto& GetAllocationInner(this auto&& self) noexcept {
         return self.template AccessStack<OwnershipStack>();
      }
      
      /// Set allocation (inner)                                              
      ///   @attention this will not dereference previous allocation          
      constexpr void SetAllocationInner(this auto& self, Allocation const* a) noexcept {
         self.GetAllocationInner() = const_cast<Allocation*>(a);
      }

      /// Automatically set the allocation by searching for it using the heap 
      /// pointer. If allocation wasn't found, it will be set to nullptr.     
      ///   @attention this will not dereference previous allocation          
      void FindAllocationInner(this auto& self) noexcept {
         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            if (auto found = Allocator::Find(self.GetHeapInner())) {
               self.SetAllocationInner(found);
               if constexpr (AUTO)
                  self.Keep();
            }
            else
         #endif
         self.SetAllocationInner(nullptr);
      }

      /// Default-initialize the component                                    
      ///   @attention this will not dereference previous allocation          
      constexpr void ConstructDefault(this auto& self) noexcept {
         self.SetAllocationInner(nullptr);
      }
      
      /// Transfer from any kind of container, respecting intents             
      ///   @attention this will not dereference previous allocation          
      ///   @param self deduced this                                          
      ///   @param intent the intent and container to transfer from           
      template<CT::Intent I> requires CT::Container<I>
      void ConstructFrom(this auto& self, I&& intent) {
         decltype(auto) from = FWD(intent.what);

         // @important notice that Copy and Clone intents are not       
         //    handled here. They're handled in heap components instead,
         //    in case something throws an exception while constructing 
         if constexpr (CT::Referred<I>) {
            // Refer                                                    
            self.SetAllocationInner(from.GetAllocation());
            if constexpr (AUTO)
               self.Keep();
         }
         else if constexpr (CT::Abandoned<I> or CT::Moved<I>) {
            // Abandon/Move                                             
            self.SetAllocationInner(from.GetAllocation());

            if constexpr (requires { from.SetAllocationInner(nullptr); })
               from.SetAllocationInner(nullptr);
            else if constexpr (AUTO and CT::AutoOwned<I>) {
               // We can't reset source allocation pointer, which means 
               // that source destructor will dereference when out of   
               // scope. We have to reference the data here.            
               self.Keep();
            }
         }
         else if constexpr (CT::Disowned<I>) {
            // Disown                                                   
            self.SetAllocationInner(nullptr);
         }
      }
   };
}
