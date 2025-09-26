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
   
   ///                                                                        
   /// Keep a pointer to the heap allocation as a member.                     
   /// Manage its ownership by referencing and dereferencing it.              
   /// Can also reference on per-element basis if enabled via DEEPREF.        
   ///   @tparam ID - which heap are we keeping track of?                     
   ///   @tparam AUTO - whether ownership will be automatically applied on    
   ///      construction, reassignment and destruction. False if container is 
   ///      just a view, or in other cases where you want to carry an         
   ///      allocation pointer, but not necessarily reference it.             
   ///   @tparam DEEPREF - whether to reference individual elements.          
   template<unsigned ID, bool AUTO, bool DEEPREF>
   struct OwnershipStack {
      using CTTI_Component = Yes<>;
      using StackRequest   = AllocationPtr;

      static constexpr bool Owned = true;
      static constexpr bool OwnedOnConstructOrAssign = AUTO;
      static constexpr bool DeeplyReferenced = DEEPREF;
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
            return; // We already own this allocation                   
      
         // The heap might already be ours and we just don't know it    
         if (auto found = Allocator::Find(self.GetType(), self.GetHeapInner())) {
            a = const_cast<AllocationPtr>(found);
            a->Keep();
            return;
         }

         // Shallow-copy all elements in a fresh allocation             
         C temp {Copy {self}};
         self = Abandon {temp};
      }

   protected:
      template<unsigned> friend struct HeapMovable;
      template<unsigned> friend struct DeepOwnershipHeap;
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

         // @important notice that Copy and Clone intents are not       
         //    handled here. They're handled in heap components instead,
         //    in case something throws an exception while constructing 
         if constexpr (CT::Moved<I>) {
            // Move                                                     
            self.SetAllocationInner(from.GetAllocationInner());

            if constexpr (OwnedOnConstructOrAssign and not IT::Owned) {
               // Since we are not aware if that block is referenced or 
               // not we reference it just in case, and we also do not  
               // reset 'from' to avoid leaks. When using containers    
               // without ownership, it's _your_ responsibility to      
               // handle it                                             
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
         else if constexpr (CT::Abandoned<I>) {
            // Abandon                                                  
            self.SetAllocationInner(from.GetAllocationInner());

            // Discard only ownership from source container             
            from.SetAllocationInner(nullptr);
         }
         else if constexpr (CT::Disowned<I>) {
            // Disown                                                   
            self.SetAllocationInner(nullptr);
         }
      }
      
      /// Reference the allocation once.                                      
      /// If container has DeepOwnership component, all entries will be       
      /// referenced as well.                                                 
      void Keep(this auto const& self) noexcept {
         auto& a = self.GetAllocationInner();
         if (not a)
            return;

         a->Keep(1);

         // Keep all entries if DeepOwnership component exists          
         if_available(self.KeepDeep());
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
            // Free all entries if DeepOwnership component exists       
            if_available(self.FreeDeep());

            // Free memory                                              
            Allocator::Deallocate(a);
         }
         else {
            // Free all entries if DeepOwnership component exists.      
            // Notice that no element will be destroyed, because in this
            // case we have a guarantee that elements are referenced    
            // from elsewhere as well.                                  
            if_available(self.template FreeDeep<false>());

            // Dereference memory                                       
            a->Free();
         }
      }
      
      /// Called on container destruction                                     
      ///   @attention this never modifies any state                          
      void Destroy(this auto& self) noexcept requires AUTO {
         self.FreeInner();
      }
      
      /// Dereference and eventually destroy the first element                
      /// This function is completely overridden by OwnershipDeep component,  
      /// if present.                                                         
      ///   @attention assumes first element is validly constructed           
      ///   @attention does not modify any container state                    
      template<CT::Container C> requires (not CT::DeeplyOwned<C>)
      void DestroyElement(this C& self) noexcept {
         static_assert(CT::ContainsOne<C>);
         if constexpr (CT::TypeErased<C>) {
            // Destroying a type-erased element                         
            auto T = self.GetType();
            if (const auto destructor = T.GetDestructor()) {
               if (const auto referencer = T.GetReferencer())
                  referencer(self.GetRaw(), -1);
               destructor(self.GetRaw());
            }
         }
         else {
            // Destroying a statically-typed element                    
            using T = TypeOf<C>;            
            if constexpr (CT::Destroyable<T>) {
               auto& element = self.Get();
               if constexpr (CT::Referenced<T>)
                  element.Reference(-1);
               element.~T();
            }
         }
      }
   };
}
