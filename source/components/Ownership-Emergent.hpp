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
   using RTTI::DMeta;

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

      /// Get the allocation                                                  
      template<Cid SID = ID> requires Relevant<SID>
      auto GetAllocation(this auto const& self) noexcept -> AllocationPtr {
         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            return Allocator::Find(self.template GetRaw<SID>());
         #else
            (void)self;
            return nullptr;
            //static_assert(false, "Emergent ownership is not allowed "
            //                     "when managed memory is disabled");
         #endif
      }

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
              or self.template GetAllocation<SID>())
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
               self.Keep();
         }
         else if constexpr (CT::Abandoned<I> or CT::Moved<I>) {
            // Abandon/Move                                             
            if_available(from.SetAllocationInner(nullptr))
            else if constexpr ((STYLE & OnCreate) and CT::StronglyOwned<I>) {
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
      ///   @attention operates on all relevant dimensions at once!           
      void Destroy(this auto& self) noexcept /*requires ((STYLE & OnDestroy) != 0)*/ {
         self.OwnershipEmergent<STYLE, ID, SHARED...>::Free();
      }

      /// Reference the allocation once.                                      
      /// If container has DeepOwnership component, all entries will be       
      /// individually referenced as well.                                    
      ///   @attention operates on all relevant dimensions at once!           
      template<Cid SID = ID, CT::Container C> requires Relevant<SID>
      void Keep(this C& self) noexcept {
         auto a = self.template GetAllocation<SID>();
         if (not a) {
            // Container is disowned, and nothing gets reffed, unless   
            // the container is marked Emergent. Emergent containers    
            // don't support disownment, so that you can do deep        
            // ownership without having a main allocation, so that      
            // elements can lie on the stack instead. Examples:         
            // TPair and THandlePair                                    
            if constexpr (requires { C::Emergent; }) {
               if constexpr (CT::DeeplyOwned<C>) {
                  // Reference all indirections and (optionally) items  
                  if constexpr (CT::TypeErased<C>) {
                     if (self.template IsSparse<SID>()) {
                        self.Apply([](auto&& item) {
                           Id::ForEach([&item]<Cid D> {
                              item.template KeepElementDeep<false, D>();
                           });
                        });
                     }
                  }
                  else if constexpr (CT::Sparse<TypeOf<C, SID>>) {
                     self.Apply([](auto&& item) {
                        Id::ForEach([&item]<Cid D> {
                           item.template KeepElementDeep<false, D>();
                        });
                     });
                  }
               }
            }
            return;
         }

         DecvqAllCast(a)->AddRef(1);

         if constexpr (CT::DeeplyOwned<C>) {
            // Reference all indirections and (optionally) items        
            if constexpr (CT::TypeErased<C>) {
               if (self.template IsSparse<SID>()) {
                  self.Apply([](auto&& item) {
                     Id::ForEach([&item]<Cid D> {
                        item.template KeepElementDeep<false, D>();
                     });
                  });
               }
            }
            else if constexpr (CT::Sparse<TypeOf<C, SID>>) {
               self.Apply([](auto&& item) {
                  Id::ForEach([&item]<Cid D> {
                     item.template KeepElementDeep<false, D>();
                  });
               });
            }
         }
      }

      /// Dereference memory block once and destroy all elements if data was  
      /// fully dereferenced                                                  
      ///   @attention this never modifies any state                          
      ///   @attention operates on all relevant dimensions at once!           
      template<Cid SID = ID, CT::Container C> requires Relevant<SID>
      void Free(this C& self) noexcept {
         auto a = self.template GetAllocation<SID>();
         if (not a) {
            // Container is disowned, and nothing gets dereffed, unless 
            // the container is marked Emergent. Emergent containers    
            // don't support disownment, so that you can do deep        
            // ownership without having a main allocation, so that      
            // elements can lie on the stack instead. Examples:         
            // TPair and THandlePair                                    
            if constexpr (requires { C::Emergent; }) {
               if constexpr (CT::DeeplyOwned<C>) {
                  // Dereference all indirections and (optionally) items
                  if constexpr (CT::TypeErased<C>) {
                     if (not self.IsEmpty() and self.template IsSparse<SID>()) {
                        self.Apply([](auto&& item) {
                           Id::ForEach([&item]<Cid D> {
                              item.template DestroyElementDeep<false, D>();
                           });
                        });
                     }
                  }
                  else if constexpr (CT::Sparse<TypeOf<C, SID>>) {
                     if (not self.IsEmpty()) {
                        self.Apply([](auto&& item) {
                           Id::ForEach([&item]<Cid D> {
                              item.template DestroyElementDeep<false, D>();
                           });
                        });
                     }
                  }
               }
            }
            return;
         }

         LglsAssumeDev(a->GetUses() >= 1, "Bad memory dereferencing");
         if (a->GetUses() == 1) {
            // Dereference, and eventually destroy all elements - all   
            // indirections, as well as dense elements.                 
            if (not self.IsEmpty())
               self.template DestroyAllElements<true>();
            Allocator::Deallocate(DecvqAllCast(a));
         }
         else {
            // Dereference, and eventually destroy all elements -       
            // affect indirections and elements behind them only!       
            if (not self.IsEmpty())
               self.template DestroyAllElements<false>();
            DecvqAllCast(a)->AddRef(-1);
         }
      }

      /// Same as free, but here in case container isn't Multiown             
      void FreeAll(this auto& self) noexcept {
         self.OwnershipEmergent<STYLE, ID, SHARED...>::Free();
      }

      /// Destroy the first element                                           
      ///   @attention doesn't perform any referencing or indirection         
      ///   @attention assumes first element is validly constructed           
      ///   @attention does not modify any container state                    
      template<Cid SID = ID, CT::Container C> requires Relevant<SID>
      void DestroyElementShallow(this C& self) noexcept {
         //static_assert(CT::ContainsOne<C>,
         //   "Destroying only first element in a container with many");
         static_assert(not CT::DeeplyOwned<C>,
            "Can't shallow-destroy a deeply-owned container");

         if (self.template IsEmpty<SID>())
            return;

         if constexpr (CT::TypeErased<C>) {
            // Destroying a type-erased element                         
            auto T = self.template GetType<SID>();
            if (const auto destructor = T.GetDestructor()) {
               const auto ptr = self.template GetRaw<SID>();
               destructor(ptr);
            }
         }
         else {
            // Destroying a statically-typed element                    
            using T = TypeOf<C, SID>;
            if constexpr (CT::Destroyable<T>) {
               auto* element = self.template Get<void, SID>();
               element->~T();
            }
         }
      }
   };
}
