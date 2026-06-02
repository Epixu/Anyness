///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
#include <Langulus/Allocator.hpp>


namespace Langulus::Anyness::Component
{
   //using RTTI::DMeta;

   /// Refers back to this particular component instance through the deduced  
   /// 'this'. Just for convenience. It is #undef-ed at the end of this file. 
   #define ThisCom self.OwnershipEmergent<STYLE, ID, SHARED...>

   ///                                                                        
   /// Heap allocation will be searched on demand every time.                 
   /// Manage its ownership by referencing and dereferencing it, if enabled.  
   /// Emergent ownership disallows disownment.                               
   ///   @tparam STYLE whether ownership will be automatically applied on     
   ///      construction, reassignment and destruction. Usually 0 if container
   ///      is just a view, or in other cases where you want to carry an      
   ///      allocation pointer, but not necessarily reference it.             
   ///   @tparam ID provider we're keeping track of                           
   ///   @tparam SHARED other providers that will share the same allocation   
   ///      variable.                                                         
   template<uint STYLE, Cid ID, Cid...SHARED>
   struct OwnershipEmergent {
      using CTTI_Component = Yes<>;
      using CTTI_ReflectAs = void;
      using Id = Values<ID, SHARED...>;

      static constexpr uint Owned = STYLE;
      static constexpr int  ComponentPrecedence = 1000;
      template<Cid SID>
      static constexpr bool Relevant = Id::template Contains<SID>;

      #if LANGULUS_FEATURE(MANAGED_MEMORY)
         /// Get the allocation by searching the memory manager               
         template<Cid SID = ID> requires Relevant<SID>
         auto GetAllocation(this auto const& self) noexcept -> AllocationPtr {
            return Allocator::Find(self.template GetRaw<SID>());
         }
      #else
         /// Always invalid allocation when managed memory is disabled.       
         /// Emergent containers without memory management can't reference.   
         template<Cid SID = ID> requires Relevant<SID>
         constexpr auto GetAllocation() const noexcept -> AllocationPtr {
            return nullptr;
         }
      #endif

      /// Get the memory reference count                                      
      template<Cid SID = ID> requires Relevant<SID>
      auto GetUses(this auto const& self) noexcept {
         auto a = self.template GetAllocation<SID>();
         return a ? a->GetUses() : 0;
      }

      /// Shallow-copy all initialized elements in memory to another          
      /// allocation, that is owned once only by this container.              
      ///   @attention does nothing if we already have ownership              
      ///   @attention when emergent, this will copy data only if not owned   
      ///      by the memory manager.                                         
      template<Cid SID = ID, CT::Container C>
      requires (CT::HeapAllocated<C> and Relevant<SID>)
      void TakeOwnership(this C& self) {
         if (not self.template GetHeapInner<SID>()
         or      self.template GetAllocation<SID>())
            return;

         // Shallow-copy all elements in a fresh allocation             
         // Notice this works on the entire container, not the SID only.
         // SID is used only for early exit, and it should stop copying 
         // the moment ownership has been provided.                     
         C temp {Copy {self}};
         self = Abandon {temp};
      }

   protected:
      LglsComHeapReference(friend);
      LglsComHeapMovable(friend);
      LglsComRemoval(friend);
      LglsComEmplacement(friend);

      /// Transfer from any kind of container, respecting intents             
      ///   @attention this will not dereference previous allocation          
      ///   @param intent the intent and container to transfer from           
      ///   @important notice that Copy and Clone intents are not handled     
      ///      here. They're handled in heap components instead, in case      
      ///      something throws an exception while constructing.              
      template<class SELF, CT::Intent I>
      requires (CT::Container<I> and not CT::Copied<I> and not CT::Cloned<I>)
      void ConstructFrom(this SELF& self, I&& intent) {
         decltype(auto) from = LglsFwd(intent.what);

         if constexpr (CT::Referred<I>) {
            // Refer                                                    
            if constexpr (STYLE & OnCreate)
               ThisCom::Keep();
         }
         else if constexpr (CT::Abandoned<I> or CT::Moved<I>) {
            // Abandon/Move                                             
            if_available(from.SetAllocationInner(nullptr))
            else if constexpr ((STYLE & OnCreate) and CT::StronglyOwned<I>) {
               // We can't reset source allocation pointer, which means 
               // that source destructor will dereference when out of   
               // scope, because it is likely emergent. We are forced   
               // to reference the data here. Keeping 'from' because it 
               // is more likely to have the allocation cached.         
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
      ///   @attention operates on all relevant dimensions at once!           
      template<bool DEALLOCATE = true, class SELF>
      void Destroy(this SELF& self) noexcept requires ((STYLE & OnDestroy) != 0) {
         ThisCom::template Free<DEALLOCATE>();
      }

      /// Reference the allocation once                                       
      void Keep(this auto& self) noexcept {
         auto a = self.template GetAllocation<Id::First>();
         if (not a)
            return;

         DecvqAllCast(a)->AddRef(1);
      }

      /// Dereference memory block once and destroy all immediate elements if 
      /// local allocation was fully dereferenced.                            
      ///   @tparam DEALLOCATE are we going to reuse the allocation? if not,  
      ///      set this to true, to discard it.                               
      ///   @attention this never modifies any state                          
      ///   @attention operates on all relevant dimensions at once!           
      template<bool DEALLOCATE = true, CT::Container C>
      void Free(this C& self) noexcept {
         auto a = self.template GetAllocation<Id::First>();
         if (not a)
            return;

         LglsAssumeDev(a->GetUses() >= 1, "Bad memory dereferencing");
         if (a->GetUses() != 1) {
            // Memory is used elsewhere, just dereference once          
            if constexpr (DEALLOCATE)
               DecvqAllCast(a)->AddRef(-1);
            return;
         }
      
         //                                                             
         // If reached, this was the only container that owns the       
         // immediate elements. It is time to destroy them.             
         if (not self.template IsEmpty<Id::First>()) {
            Id::ForEach([&self]<Cid D> {
               if constexpr (CT::TypeErased<C>) {
                  auto T = self.template GetType<D>();
                  if (const auto destructor = T.GetDestructor()) {
                     self.Apply([&destructor](auto&& item) {
                        const auto ptr = item.template GetRaw<D>();
                        destructor(ptr);
                     });
                  }
               }
               else {
                  using T = TypeOf<C, D>;
                  if constexpr (CT::Destroyable<T>) {
                     self.Apply([](auto&& item) {
                        auto* element = item.template Get<void, D>();
                        element->~T();
                     });
                  }
               }
            });
         }

         if constexpr (DEALLOCATE)
            Allocator::Deallocate(DecvqAllCast(a));
      }
   };

   #undef ThisCom
}
