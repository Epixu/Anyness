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
   /// Heap allocation will be searched on demand every time.                 
   /// Manage its ownership by referencing and dereferencing it.              
   /// Can also reference on per-element basis if enabled via DEEPREF.        
   ///   @tparam ID - which heap are we keeping track of?                     
   ///   @tparam AUTO - whether ownership will be automatically applied on    
   ///      construction, reassignment and destruction. False if container is 
   ///      just a view, or in other cases where you want to carry an         
   ///      allocation pointer, but not necessarily reference it.             
   ///   @tparam DEEPREF - whether to reference individual elements.          
   template<unsigned ID, bool AUTO, bool DEEPREF>
   struct OwnershipEmergent {
      using CTTI_Component = Yes<>;

      static constexpr bool Owned = true;
      static constexpr bool AutoOwned = AUTO;
      static constexpr bool DeeplyReferenced = DEEPREF;
      static constexpr int  ComponentPrecedence = -1000;

      /// Get the allocation                                                  
      auto GetAllocation(this auto const& self) noexcept {
         return Allocator::Find(self.GetType(), self.GetHeapInner());
      }

      /// Get the memory reference count                                      
      auto GetUses(this auto const& self) noexcept {
         auto a = self.GetAllocation();
         return a ? a->GetUses() : 0;
      }

      /// Shallow-copy all initialized elements in memory to another          
      /// allocation, that is owned once only by this container.              
      ///   @attention does nothing if we already have ownership              
      template<CT::Container C> requires C::HeapAllocated   
      void TakeOwnership(this C& self) {
         if (not self.GetHeapInner() or self.GetAllocation())
            return;

         // Shallow-copy all elements in a fresh allocation             
         C temp {Copy {self}};
         self = Abandon {temp};
      }

   protected:
      /*
      template<unsigned> friend struct DeepOwnershipHeap;*/
      template<unsigned> friend struct HeapMovable;
      template<unsigned> friend struct Removal;
      template<unsigned> friend struct Emplacement;

      /// Transfer from any kind of container, respecting intents             
      ///   @attention this will not dereference previous allocation          
      ///   @param intent - the intent and container to transfer from         
      template<CT::Intent I> requires CT::Container<I>
      void ConstructFrom(this auto& self, I&& intent) {
         decltype(auto) from = FWD(intent.what);

         // @important notice that Copy and Clone intents are not       
         //    handled here. They're handled in heap components instead,
         //    in case something throws an exception while constructing 
         if constexpr (CT::Referred<I>) {
            // Refer                                                    
            if constexpr (AUTO)
               self.Keep();
         }
         else if constexpr (CT::Abandoned<I> or CT::Moved<I>) {
            // Abandon/Move                                             
            if constexpr (requires { from.SetAllocationInner(nullptr); })
               from.SetAllocationInner(nullptr);
            else if constexpr (AUTO and CT::AutoOwned<I>) {
               // We can't reset source allocation pointer, which means 
               // that source destructor will dereference when out of   
               // scope. We have to reference the data here.            
               // Keeping 'from' because it is more likely to have the  
               // allocation pointer cached.                            
               from.Keep();
            }
         }
         else if constexpr (CT::Disowned<I>) {
            // Disown                                                   
            LglsAssert(not from.GetAllocation(),
               "Emergent ownership doesn't allow disownment");
         }
      }

      /// Called on container destruction                                     
      ///   @attention this never modifies any state                          
      void Destroy(this auto& self) noexcept requires AUTO {
         self.Free();
      }

      /// Reference the allocation once.                                      
      /// If container has DeepOwnership component, all entries will be       
      /// individually referenced as well.                                    
      void Keep(this auto const& self) noexcept {
         auto a = self.GetAllocation();
         if (not a)
            return;

         a->Keep(1);
         if_available(self.KeepDeep());
      }

      /// Dereference memory block once and destroy all elements if data was  
      /// fully dereferenced                                                  
      ///   @attention this never modifies any state                          
      void Free(this auto& self) noexcept {
         auto a = self.GetAllocation();
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
      
      /// Dereference and eventually destroy the first element                
      ///   @attention this function should be completely overridden by       
      ///      OwnershipDeep component's equivalent, if both are present      
      ///   @attention assumes first element is validly constructed           
      ///   @attention does not modify any container state                    
      template<CT::Container C> requires (not CT::DeeplyOwned<C>)
      void DestroyElement(this C& self) noexcept {
         static_assert(CT::ContainsOne<C>,
            "Destroying only first element in a container with many");

         if constexpr (CT::TypeErased<C>) {
            // Destroying a type-erased element                         
            auto T = self.GetType();
            if (const auto destructor = T.GetDestructor()) {
               const auto ptr = self.GetRaw();
               if (const auto referencer = T.GetReferencer())
                  referencer(ptr, -1);
               destructor(ptr);
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
