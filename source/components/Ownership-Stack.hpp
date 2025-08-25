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
   ///   @tparam AUTO - whether ownership will be automatically applied on    
   ///      construction, reassignment and destruction. False if container is 
   ///      just a view, or in other cases where you want to carry an         
   ///      allocation pointer, but not necessarily reference it              
   template<unsigned ID = 0, bool AUTO = true>
   struct OwnershipStack {
      using CTTI_Component = Yes<>;
      static constexpr bool Owned = AUTO;
      static constexpr int  StackSize = sizeof(AllocationPtr);
      static constexpr int  ComponentPrecedence = -1000;

      /// Get the allocation                                                  
      auto GetAllocation(this auto const& self) noexcept {
         return self.GetAllocationInner();
      }

      /// Get the memory reference count                                      
      auto GetUses(this auto const& self) noexcept {
         auto a = self.GetAllocationInner();
         return a ? a->GetUses() : 0;
      }

      /// Shallow-copy all initialized elements in memory to another          
      /// allocation, that is owned once only by this container               
      ///   @attention if we already own the memory just Keep() it once       
      template<CT::Container C> requires C::HeapAllocated   
      void TakeOwnership(this C& self) {
         if (not self.GetRaw())
            return;

         auto a = self.GetAllocationInner();
         if (a) {
            // We already have authority                                
            a->Keep();
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

      /// Get allocation (inner)                                              
      constexpr auto& GetAllocationInner(this auto const& self) noexcept {
         return *reinterpret_cast<AllocationPtr const*>(
            self.mStack + self.template StackOffset<OwnershipStack>
         );
      }
      
      constexpr void SetAllocationInner(this auto& self, AllocationPtr a) noexcept {
         const_cast<AllocationPtr&>(self.GetAllocationInner()) = a;
      }

      /// Default-initialize the component                                    
      constexpr void ConstructDefault(this auto& self) noexcept {
         self.SetAllocationInner(nullptr);
      }
      
      /// Transfer from any kind of container, respecting intents             
      ///   @param intent - the intent and container to transfer from         
      template<CT::Intent I> requires CT::Container<I>
      void ConstructFrom(this auto& self, I&& intent) {
         using IT = Decay<TypeOf<I>>;
         decltype(auto) from = FWD(intent.what);

         if constexpr (I::IsShallow()) {
            // Move/Copy/Refer/Abandon/Disown other                     
            if constexpr (I::IsKept()) {
               // Move/Copy/Refer other                                 
               if constexpr (I::IsMoved()) {
                  // Move                                               
                  self.SetAllocationInner(from.GetAllocationInner());

                  if constexpr (AUTO and not IT::Owned) {
                     // Since we are not aware if that block is         
                     // referenced or not we reference it just in case, 
                     // and we also do not reset 'from' to avoid leaks. 
                     // When using containers without ownership, it's   
                     // _your_ responsibility to handle it              
                     self.Keep();
                  }
                  else from.SetAllocationInner(nullptr);
               }
               else if constexpr (CT::Referred<I>) {
                  // Refer                                              
                  self.SetAllocationInner(from.GetAllocationInner());
                  if constexpr (AUTO)
                     self.Keep();
               }
            }
            else if constexpr (I::IsMoved()) {
               // Abandon                                               
               self.SetAllocationInner(from.GetAllocationInner());
               
               // Discard only ownership from source container          
               from.SetAllocationInner(nullptr);
            }
            else {
               // Disown                                                
               self.SetAllocationInner(nullptr);
            }
         }
      }
      
      /// Reference memory block once                                         
      /// If container has DeepOwnership component, all elements will be      
      /// referenced as well, if they're CT::Referenced                       
      void Keep(this auto const& self) noexcept {
         auto& a = self.GetAllocationInner();
         if (not a)
            return;

         a->Keep(1);

         // Keep elements, if DeepOwnership component exists            
         if constexpr (requires { self.KeepDeep(); })
            self.KeepDeep();
      }
      
      /// Dereference memory block once and destroy all elements if data was  
      /// fully dereferenced. If container has DeepOwnership component, all   
      /// elements will be individually dereferenced as well, if they are     
      /// CT::Referenced                                                      
      ///   @attention this never modifies any state except ownership,        
      ///      effectively making the data disowned (and constant) after this 
      void Free(this auto& self) noexcept {
         self.FreeInner();
         self.SetAllocationInner(nullptr);
      }

      /// Dereference memory block once and destroy all elements if data was  
      /// fully dereferenced                                                  
      ///   @attention this never modifies any state                          
      void FreeInner(this auto& self) noexcept {
         auto& a = self.GetAllocationInner();
         if (not a)
            return;

         LglsAssumeDev(a->GetUses() >= 1,
            "Bad memory dereferencing");

         if (a->GetUses() == 1) {
            // Free elements, if DeepOwnership component exists         
            if constexpr (requires { self.FreeDeep(); })
               self.FreeDeep();

            // Free memory                                              
            Allocator::Deallocate(a);
         }
         else {
            // Free elements, if DeepOwnership component exists         
            // Notice that no element will be destroyed, because in this
            // case we have a guarantee, that elements are referenced   
            // from elsewhere as well                                   
            if constexpr (requires { self.FreeDeep(); })
               self.template FreeDeep<false>();

            // Dereference memory                                       
            a->Free();
         }
      }
      
      /// Called on container destruction                                     
      ///   @attention this never modifies any state                          
      void Destroy(this auto& self) noexcept requires AUTO {
         self.FreeInner();
      }
   };
}
