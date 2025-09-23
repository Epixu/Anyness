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
   using RTTI::DMeta;
   
   namespace Inner
   {
      /// Nest-dereference/destroy an element on the heap                     
      void DestroyElementDeep(auto* ptr, DMeta type, Allocation const* entry) has_assumptions {
         LglsAssumeDevAndOptimize(ptr, "No heap");
         LglsAssumeDev(type, "No type");
         
         if (type.IsSparse()) {
            LglsAssumeDevAndOptimize(entry, "No entry");
            
            auto subType = type.GetDeptr();
            if (1 == entry->GetUses()) {
               // This is the last occurence of that element            
               if (subType.IsSparse()) {
                  // Pointer to pointer                                 
                  // Release all nested indirection layers              
                  void* subPtr = *static_cast<void**>(ptr); //TODO this won't work for packed pointers
                  if (auto subEntry = Allocator::Find(subType, subPtr))
                     DestroyElementDeep(subPtr, subType, subEntry);
               }
               else if (subType.GetDestructor()) {
                  // Pointer to a complete, destroyable dense           
                  // Call the destructor                                
                  if (subType.GetReferencer()) {
                     if (subType.GetReferencer()(ptr, -1) == 0)
                        subType.GetDestructor()(ptr);
                  }
                  else subType.GetDestructor()(ptr);
               }

               Allocator::Deallocate(const_cast<Allocation*>(entry));
            }
            else {
               // This element occurs in more than one place.           
               // We're not allowed to deallocate the memory behind it, 
               // but we must call destructors if T is referencable and 
               // its individual references have reached 0. This can    
               // happen when hive elements are dereferenced.           
               if (not subType.IsSparse() and type.GetReferencer()) {
                  if (type.GetReferencer()(ptr, -1) == 0)
                     type.GetDestructor()(ptr);
               }

               const_cast<Allocation*>(entry)->Free();
            }
         }
         else if (type.GetDestructor()) {
            // Call destructor of dense element                         
            if (type.GetReferencer())
               type.GetReferencer()(ptr, -1);
            type.GetDestructor()(ptr);
         }
      }
   }
   
   ///                                                                        
   /// Keep a pointer to the heap allocation as a member.                     
   /// Manage its ownership.                                                  
   ///   @tparam ID - which heap are we keeping track of?                     
   ///   @tparam AUTO - whether ownership will be automatically applied on    
   ///      construction, reassignment and destruction. False if container is 
   ///      just a view, or in other cases where you want to carry an         
   ///      allocation pointer, but not necessarily reference it.             
   template<unsigned ID, bool AUTO>
   struct OwnershipStack {
      using CTTI_Component = Yes<>;
      using StackRequest   = AllocationPtr;

      static constexpr bool Owned = true;
      static constexpr bool OwnedOnConstructOrAssign = AUTO;
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
         if (not self.GetHeapInner())
            return;

         auto& a = self.GetAllocationInner();
         if (a)
            return; // We have already owned that allocation            
      
         // The heap might already be ours but we're just not aware     
         if (auto found = Allocator::Find(self.GetType(), self.GetHeapInner())) {
            a = const_cast<AllocationPtr>(found);
            a->Keep();
            return;
         }

         // Shallow-copy all elements in a new, owned allocation        
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
      template<unsigned>
      friend struct Emplacement;

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
         auto found = Allocator::Find(self.GetType(), self.GetHeapInner());
         self.SetAllocationInner(found ? found : nullptr);
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

                  if constexpr (OwnedOnConstructOrAssign and not IT::Owned) {
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
      ///      effectively making the data disowned (and by extension         
      ///      constant) after this                                           
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

         LglsAssumeDev(a->GetUses() >= 1, "Bad memory dereferencing");

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
      template<bool RESET, CT::Container C>
      void DestroyElement(this C& self) {
         if constexpr (C::TypeErased) {
            auto T = self.GetType();
            if (T.IsSparse()) {
               auto& ptr = *self.template GetRawAs<void*>();
               auto& entry = self.GetEntry();
               if (entry) {
                  Inner::DestroyElementDeep(ptr, T, entry);
                  if constexpr (RESET)
                     entry = nullptr;
               }
               if constexpr (RESET)
                  ptr = nullptr;
            }
            else if (T.GetDestructor()) {
               // Call destructor of dense element                      
               if (T.GetReferencer())
                  T.GetReferencer()(self.GetRaw(), -1);
               T.GetDestructor()(self.GetRaw());
            }
         }
         else {
            using T = TypeOf<C>;
            using DT = Decay<T>;
            if constexpr (CT::Sparse<T>) {
               auto& ptr = *self.template GetRawAs<T>();
               auto& entry = self.GetEntry();
               if (entry) {
                  if (1 == entry->GetUses()) {
                     // This is the last occurence of that element      
                     LglsAssumeDev(ptr, "Null pointer");

                     if constexpr (CT::Sparse<Deptr<T>>) {
                        // Pointer to pointer                           
                        // Release all nested indirection layers        
                        /*THandle*/ C {*ptr}.template DestroyElement<false>();
                     }
                     /*else if constexpr (not CT::Complete<DT> and not CT::Function<DT>) {
                        // CT::Destroyable<DT> will fail silently if DT 
                        // isn't defined yet, causing nasty leaks. So   
                        // make it not-so-silent...                     
                        static_assert(false, "Attempting to destroy an incomplete type");
                     }*/
                     else if constexpr (CT::Destroyable<DT>) {
                        // Pointer to a complete, destroyable dense     
                        // Call the destructor                          
                        if constexpr (CT::Referenced<DT>) {
                           if (ptr->Reference(-1) == 0)
                              ptr->~DT();
                        }
                        else ptr->~DT();
                     }

                     Allocator::Deallocate(entry);
                  }
                  else {
                     // This element occurs in more than one place         
                     // We're not allowed to deallocate the memory behind  
                     // it, but we must call destructors if T is           
                     // referencable, and its individual references have   
                     // reached 1. This usually happens when elements from 
                     // a THive are referenced.                            
                     if constexpr (CT::Dense<Deptr<T>> and CT::Referenced<DT>) {
                        if (ptr->Reference(-1) == 0)
                           ptr->~DT();
                     }

                     entry->Free();
                  }
               }

               if constexpr (RESET) {
                  ptr = nullptr;
                  entry = nullptr;
               }
            }
            /*else if constexpr (not CT::Complete<DT> and not CT::Function<DT>) {
               // CT::Destroyable<DT> will fail silently if DT isn't       
               // defined yet, causing nasty leaks. So make it             
               // not-so-silent...                                         
               static_assert(false, "Attempting to destroy an incomplete type");
            }*/
            else if constexpr (CT::Destroyable<DT>) {
               // Call destructor of dense element                      
               if constexpr (CT::Referenced<DT>)
                  self.Get().Reference(-1);
               self.Get().~DT();
            }
         }
      }
   };
}
