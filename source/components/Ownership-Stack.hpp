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
   ///   @tparam ID provider we're keeping track of                           
   ///   @tparam AUTO whether ownership will be automatically applied on      
   ///      construction, reassignment and destruction. False if container is 
   ///      just a view, or in other cases where you want to carry an         
   ///      allocation pointer, but not necessarily reference it.             
   ///   @tparam SHARED other providers that will share the same allocation   
   ///      variable.                                                         
   template<Cid ID, bool AUTO, Cid...SHARED>
   struct OwnershipStack : OwnershipEmergent<ID, AUTO, SHARED...> {
      using StackRequest = AllocationPtr;

      /// Get the allocation                                                  
      template<Cid SID = ID>
      constexpr auto GetAllocation(this auto const& self) noexcept {
         static_assert(SID == ID or ((SID == SHARED) or ...));
         return self.GetAllocationInner();
      }

      /// Shallow-copy all initialized elements in memory to another          
      /// allocation, that is owned once only by this container.              
      ///   @attention if we already own the memory just Keep() it once       
      template<Cid SID = ID, CT::Container C> requires CT::HeapAllocated<C>
      void TakeOwnership(this C& self) {
         static_assert(SID == ID or ((SID == SHARED) or ...));
         if (not self.GetHeapInner())
            return;

         auto& a = self.GetAllocationInner();
         if (a)
            return; // We already own this allocation                   

      #if LANGULUS_FEATURE(MANAGED_MEMORY)
         // The heap might already be ours and we just don't know it    
         if (auto found = Allocator::Find(self.GetHeapInner())) {
            a = found;
            DecvqAllCast(a)->AddRef(1);
            return;
         }
      #endif

         // Shallow-copy all elements in a fresh allocation             
         C temp {Copy {self}};
         self = Abandon {temp};
      }

   protected:
      template<Cid, CT::HeapEntry...>              friend struct HeapReference;
      template<Cid, uint, uint, CT::HeapEntry...>  friend struct HeapMovable;
      template<Cid, Cid...>                        friend struct Removal;
      template<Cid, Cid...>                        friend struct Emplacement;

      /// Get allocation (inner)                                              
      ///   @attention may be uninitialized                                   
      template<Cid SID = ID>
      constexpr auto& GetAllocationInner(this auto&& self) noexcept {
         static_assert(SID == ID or ((SID == SHARED) or ...));
         return self.template AccessStack<OwnershipStack>();
      }
      
      /// Set allocation (inner)                                              
      ///   @attention this will not dereference previous allocation          
      template<Cid SID = ID>
      constexpr void SetAllocationInner(this auto& self, Allocation const* a) noexcept {
         static_assert(SID == ID or ((SID == SHARED) or ...));
         self.GetAllocationInner() = const_cast<Allocation*>(a);
      }

      /// Automatically set the allocation by searching for it using the heap 
      /// pointer. If allocation wasn't found, it will be set to nullptr.     
      ///   @attention this will not dereference previous allocation          
      template<Cid SID = ID>
      void FindAllocationInner(this auto& self) noexcept {
         static_assert(SID == ID or ((SID == SHARED) or ...));
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
         decltype(auto) from = LglsFwd(intent.what);

         // @important notice that Copy and Clone intents are not       
         //    handled here. They're handled in heap components instead,
         //    in case something throws an exception while constructing 
         if constexpr (CT::Referred<I>) {
            // Refer                                                    
            if constexpr (requires { from.GetAllocationInner(); }) {
               self.SetAllocationInner(from.GetAllocationInner());
               if constexpr (AUTO)
                  self.Keep();
            }
            else self.FindAllocationInner();
         }
         else if constexpr (CT::Abandoned<I> or CT::Moved<I>) {
            // Abandon/Move                                             
            if constexpr (requires { from.GetAllocationInner(); }) {
               self.SetAllocationInner(from.GetAllocationInner());

               if_available(from.SetAllocationInner(nullptr))
               else if constexpr (AUTO and CT::AutoOwned<I>) {
                  // We can't reset source allocation pointer, which    
                  // means that source destructor will dereference when 
                  // out of scope. We have to reference the data here.  
                  self.Keep();
               }
            }
            else self.FindAllocationInner();
         }
         else if constexpr (CT::Disowned<I>) {
            // Disown                                                   
            self.SetAllocationInner(nullptr);
         }
      }
   };
}
