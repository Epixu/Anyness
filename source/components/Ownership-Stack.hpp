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
   private:
      // The allocation interface on the stack                          
      // It is private so that it isn't accessible when inherited       
      // It has to be accessed through GetAllocation()/SetAllocation()  
      AllocationPtr mAllocation;

   public:
      using CTTI_Component = Yes<>;
      static constexpr bool Owned = AUTO;
      static constexpr int ComponentPrecedence = -1000;

      /// Get the allocation                                                  
      auto GetAllocation() const noexcept { return mAllocation; }

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
      
      /// Default-initialize the component                                    
      void ConstructDefault() {
         mAllocation = nullptr;
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
                  self.mAllocation = from.GetAllocation();

                  if constexpr (AUTO and not IT::Owned) {
                     // Since we are not aware if that block is         
                     // referenced or not we reference it just in case, 
                     // and we also do not reset 'from' to avoid leaks. 
                     // When using containers without ownership, it's   
                     // _your_ responsibility to handle it              
                     self.Keep();
                  }
                  else from.SetAllocation(nullptr);
               }
               else if constexpr (CT::Referred<I>) {
                  // Refer                                              
                  self.mAllocation = from.GetAllocation();
                  if constexpr (AUTO)
                     self.Keep();
               }
            }
            else if constexpr (I::IsMoved()) {
               // Abandon                                               
               self.mAllocation = from.GetAllocation();
               
               // Discard only ownership from source container          
               from.SetAllocation(nullptr);
            }
            else {
               // Disown                                                
               self.mAllocation = nullptr;
            }
         }
      }
      
      /// Get a pointer to the allocation on the stack                        
      auto GetAllocationRef()       noexcept { return &mAllocation; }
      auto GetAllocationRef() const noexcept { return &mAllocation; }

      /// Set the allocation                                                  
      void SetAllocation(AllocationPtr a) noexcept { mAllocation = a; }

      /// Reference memory block once                                         
      /// If container has DeepOwnership component, all elements will be      
      /// referenced as well, if they're CT::Referenced                       
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
      /// fully dereferenced. If container has DeepOwnership component, all   
      /// elements will be individually dereferenced as well, if they are     
      /// CT::Referenced                                                      
      ///   @attention this never modifies any state except ownership,        
      ///      effectively making the data disowned (and constant) after this 
      template<CT::Container C>
      void Free(this C& self) noexcept {
         self.FreeInner();
         self.mAllocation = nullptr;
      }

      /// Dereference memory block once and destroy all elements if data was  
      /// fully dereferenced                                                  
      ///   @attention this never modifies any state                          
      template<CT::Container C>
      void FreeInner(this C& self) noexcept {
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
      }
      
      /// Called on container destruction                                     
      ///   @attention this never modifies any state                          
      template<CT::Container C>
      void Destroy(this C& self) noexcept requires AUTO {
         self.FreeInner();
      }
   };
}
