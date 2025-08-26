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
   /// Keep a pointer to the heap allocation as a member.                     
   /// Manage its ownership.                                                  
   ///   @tparam ID - which heap are we keeping track of?                     
   ///   @tparam AUTO - whether ownership will be automatically applied on    
   ///      construction, reassignment and destruction. False if container is 
   ///      just a view, or in other cases where you want to carry an         
   ///      allocation pointer, but not necessarily reference it.             
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
      /// allocation, that is owned once only by this container.              
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
      ///   @attention may be uninitialized                                   
      constexpr auto& GetAllocationInner(this auto const& self) noexcept {
         return *reinterpret_cast<AllocationPtr const*>(
            self.mStack + self.template StackOffset<OwnershipStack>
         );
      }
      
      /// Set allocation (inner)                                              
      ///   @attention this will not dereference previous allocation          
      constexpr void SetAllocationInner(this auto& self, AllocationPtr a) noexcept {
         const_cast<AllocationPtr&>(self.GetAllocationInner()) = a;
      }

      /// Automatically set the allocation by searching for it using the heap 
      /// pointer. If allocation wasn't found, it will be set to nullptr.     
      ///   @attention this will not dereference previous allocation          
      void FindAllocationInner(this auto& self) noexcept {
         auto found = Allocator::Find(self.GetType(), self.GetHeapInner());
         self.SetAllocationInner(found ? const_cast<AllocationPtr>(found) : nullptr);
      }

      /// Default-initialize the component                                    
      ///   @attention this will not dereference previous allocation          
      constexpr void ConstructDefault(this auto& self) noexcept {
         self.SetAllocationInner(nullptr);
      }
      
      /// Transfer from any kind of container, respecting intents             
      ///   @attention this will not dereference previous allocation          
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
      
      /// Reference memory block once.                                        
      /// If container has DeepOwnership component, all elements will be      
      /// referenced as well, if they're CT::Referenced.                      
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
      /// CT::Referenced.                                                     
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

      /// Dereference, and eventually destroy the first element               
      ///   @attention assumes first element is validly constructed           
      ///   @attention does not modify any container state                    
      template<CT::Container C>
      void DestroyElement(this C& self) {
         if constexpr (TypeErased) {
            LANGULUS_ASSUME(DevAssumes, meta,
               "Invalid type provided for type-erased handle");

            if constexpr (Sparse) {
               // Handle is sparse, we should handle each indirection layer
               LANGULUS_ASSUME(DevAssumes, meta->mIsSparse,
                  "Provided meta must match T sparseness");

               if (GetEntry()) {
                  if (1 == GetEntry()->GetUses()) {
                     // This is the last occurence of that element         
                     LANGULUS_ASSUME(DevAssumes, Get(), "Null pointer");

                     if (meta->mDeptr->mIsSparse) {
                        // Pointer to pointer                              
                        // Release all nested indirection layers           
                        HandleLocal<void*> {Get()}.FreeInner(meta->mDeptr);
                     }
                     else if (meta->mDestructor) {
                        // Pointer to a complete, destroyable dense        
                        // Call the destructor                             
                        if (meta->mReference) {
                           if (meta->mReference(Get(), -1) == 0)
                              meta->mDestructor(Get());
                        }
                        else meta->mDestructor(Get());
                     }

                     if constexpr (DEALLOCATE)
                        Allocator::Deallocate(const_cast<Allocation*>(GetEntry()));
                  }
                  else {
                     // This element occurs in more than one place         
                     // We're not allowed to deallocate the memory behind  
                     // it, but we must call destructors if T is           
                     // referencable, and its individual references have   
                     // reached 0. This usually happens when elements from 
                     // a THive are referenced.                            
                     if (not meta->mDeptr->mIsSparse and meta->mReference) {
                        if (meta->mReference(Get(), -1) == 0)
                           meta->mDestructor(Get());
                     }

                     const_cast<Allocation*>(GetEntry())->Free();
                  }
               }

               if constexpr (RESET) {
                  // Handle is dense and embedded, we should call remote   
                  // destructor, but don't touch the entry, its irrelevant 
                  const_cast<Type&>(Get()) = nullptr;
                  const_cast<AllocType&>(GetEntry()) = nullptr;
               }
            }
         }
         else {
            using DT = Decay<T>;

            if constexpr (Sparse) {
               // Handle is sparse, we should handle each indirection layer
               if (GetEntry()) {
                  if (1 == GetEntry()->GetUses()) {
                     // This is the last occurence of that element         
                     LANGULUS_ASSUME(DevAssumes, Get(), "Null pointer");

                     if constexpr (CT::Sparse<Deptr<T>>) {
                        // Pointer to pointer                              
                        // Release all nested indirection layers           
                        HandleLocal<Deptr<T>> {*Get()}.FreeInner();
                     }
                     else if constexpr (not CT::Complete<DT> and not CT::Function<DT>) {
                        // CT::Destroyable<DT> will fail silently if DT    
                        // isn't defined yet, causing nasty leaks. So make 
                        // it not-so-silent...                             
                        static_assert(false, "Attempting to destroy an incomplete type");
                     }
                     else if constexpr (CT::Destroyable<DT>) {
                        // Pointer to a complete, destroyable dense        
                        // Call the destructor                             
                        if constexpr (CT::Referencable<DT>) {
                           if (DecvqCast(Get())->Reference(-1) == 0)
                              Get()->~DT();
                        }
                        else Get()->~DT();
                     }

                     if constexpr (DEALLOCATE)
                        Allocator::Deallocate(const_cast<Allocation*>(GetEntry()));
                  }
                  else {
                     // This element occurs in more than one place         
                     // We're not allowed to deallocate the memory behind  
                     // it, but we must call destructors if T is           
                     // referencable, and its individual references have   
                     // reached 1. This usually happens when elements from 
                     // a THive are referenced.                            
                     if constexpr (CT::Dense<Deptr<T>> and CT::Referencable<DT>) {
                        if (DecvqCast(Get())->Reference(-1) == 0)
                           Get()->~DT();
                     }

                     const_cast<Allocation*>(GetEntry())->Free();
                  }
               }

               if constexpr (RESET) {
                  const_cast<Type&>(Get()) = nullptr;
                  const_cast<AllocType&>(GetEntry()) = nullptr;
               }
            }
            else if constexpr (not CT::Complete<DT> and not CT::Function<DT>) {
               // CT::Destroyable<DT> will fail silently if DT isn't       
               // defined yet, causing nasty leaks. So make it             
               // not-so-silent...                                         
               static_assert(false, "Attempting to destroy an incomplete type");
            }
            else if constexpr (EMBED and CT::Destroyable<DT>) {
               // Handle is dense and embedded, we should call the remote  
               // destructor, but don't touch the entry, its irrelevant    
               //TODO the function above states that this does nothing if dense, but apparently that isn't true
               // firgure it out!
               if constexpr (CT::Referencable<DT>)
                  Get().Reference(-1);
               Get().~DT();
            }
         }
      }

   };
}
