///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
#include "Iteration-Range.hpp"
#include <Langulus/Allocator.hpp>


namespace Langulus::Anyness::Component
{
   using RTTI::DMeta;
   
   ///                                                                        
   /// Heap allocation will be searched on demand every time.                 
   /// Manage its ownership by referencing and dereferencing it.              
   ///   @tparam ID which heap are we keeping track of?                       
   ///   @tparam AUTO whether ownership will be automatically applied on      
   ///      construction, reassignment and destruction. False if container is 
   ///      just a view, or in other cases where you want to carry an         
   ///      allocation pointer, but not necessarily reference it.             
   template<Cid ID, bool AUTO>
   struct OwnershipEmergent {
      using CTTI_Component = Yes<>;

      static constexpr bool Owned = true;
      static constexpr bool AutoOwned = AUTO;
      static constexpr int  ComponentPrecedence = 1000;

      /// Get the allocation                                                  
      auto GetAllocation(this auto const& self) noexcept {
         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            return Allocator::Find(self.GetType(), self.GetHeapInner());
         #else
            (void)self;
            static_assert(false, "Emergent ownership is not allowed when managed memory is disabled");
         #endif
      }

      /// Get the memory reference count                                      
      auto GetUses(this auto const& self) noexcept {
         auto a = self.GetAllocation();
         return a ? a->GetUses() : 0;
      }

      /// Shallow-copy all initialized elements in memory to another          
      /// allocation, that is owned once only by this container.              
      ///   @attention does nothing if we already have ownership              
      template<CT::Container C> requires CT::HeapAllocated<C>
      void TakeOwnership(this C& self) {
         if (not self.GetHeapInner() or self.GetAllocation())
            return;

         // Shallow-copy all elements in a fresh allocation             
         C temp {Copy {self}};
         self = Abandon {temp};
      }

   protected:
      template<Cid, CT::Sparse>             friend struct HeapReference;
      template<Cid, uint, uint, CT::Sparse> friend struct HeapMovable;
      template<Cid>                         friend struct Removal;
      template<Cid>                         friend struct Emplacement;
      template<Cid, bool>                   friend struct OwnershipEmergent;

      /// Transfer from any kind of container, respecting intents             
      ///   @attention this will not dereference previous allocation          
      ///   @param intent the intent and container to transfer from           
      template<CT::Intent I> requires CT::Container<I>
      void ConstructFrom(this auto& self, I&& intent) {
         decltype(auto) from = LglsFwd(intent.what);

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
            if_available(from.SetAllocationInner(nullptr))
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
      template<CT::Container C>
      void Keep(this C& self) noexcept {
         auto a = self.GetAllocation();
         if (not a)
            return; // Container is disowned, and nothing gets reffed   

         DecvqAllCast(a)->AddRef(1);

         if constexpr (CT::DeeplyOwned<C>) {
            // Reference all indirections and (optionally) items        
            if constexpr (CT::TypeErased<C>) {
               if (self.IsSparse()) {
                  self.Apply([](auto&& item) {
                     if constexpr (CT::Supported<decltype(item)>) {
                        #if LANGULUS_FEATURE(MANAGED_MEMORY)
                           item.KeepElementDeepCustomPointers();
                        #else
                           item.KeepElementDeepStandardPointers();
                        #endif
                     }
                  });
               }
            }
            else if constexpr (CT::Sparse<TypeOf<C>>) {
               self.Apply([](auto&& item) {
                  if constexpr (CT::Supported<decltype(item)>) {
                     #if LANGULUS_FEATURE(MANAGED_MEMORY)
                        item.KeepElementDeepCustomPointers();
                     #else
                        item.KeepElementDeepStandardPointers();
                     #endif
                  }
               });
            }
         }
      }

      /// Dereference memory block once and destroy all elements if data was  
      /// fully dereferenced                                                  
      ///   @attention this never modifies any state                          
      template<CT::Container C>
      void Free(this C& self) noexcept {
         auto a = self.GetAllocation();
         if (not a)
            return;

         LglsAssumeDev(a->GetUses() >= 1, "Bad memory dereferencing");
         if (a->GetUses() == 1) {
            // Dereference, and eventually destroy all elements - all   
            // indirections, as well as dense elements.                 
            self.DestroyAllElements();
            Allocator::Deallocate(DecvqAllCast(a));
         }
         else {
            // Dereference, and eventually destroy all elements -       
            // affect indirections and elements behind them only!       
            self.template DestroyAllElements<false>();
            DecvqAllCast(a)->AddRef(-1);
         }
      }
      
      /// Destroy the first element                                           
      ///   @attention doesn't perform any referencing or indirection         
      ///   @attention assumes first element is validly constructed           
      ///   @attention does not modify any container state                    
      template<CT::Container C>
      void DestroyElementShallow(this C& self) noexcept {
         //static_assert(CT::ContainsOne<C>,
         //   "Destroying only first element in a container with many");
         static_assert(not CT::DeeplyOwned<C>,
            "Can't shallow-destroy a deeply-owned container");

         if (self.IsEmpty())
            return;

         if constexpr (CT::TypeErased<C>) {
            // Destroying a type-erased element                         
            auto T = self.GetType();
            if (const auto destructor = T.GetDestructor()) {
               const auto ptr = self.GetRaw();
               destructor(ptr);
            }
         }
         else {
            // Destroying a statically-typed element                    
            using T = TypeOf<C>;
            if constexpr (CT::Destroyable<T>) {
               auto& element = self.Get();
               element.~T();
            }
         }
      }
   };
}
