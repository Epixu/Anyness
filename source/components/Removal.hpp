///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Implements removal for containers. This includes Trim, Clear, Reset    
   /// and other destruction-associated services.                             
   ///   @tparam ID provider we're removing from                              
   ///   @tparam SHARED additional providers we're removing from              
   //TODO add bool ORDER_PRESERVING as an optimization. if order is not required, we can do 'swap & pop tactic in contiguous containers for faster removal
   template<Cid ID, Cid...SHARED>
   struct Removal {
      using CTTI_Component = Yes<>;
      using CTTI_ReflectAs = void;
      using Id = Values<ID, SHARED...>;

      static constexpr int ComponentPrecedence = 3000;

   private:
      template<CT::Container C>
      using Count = typename Deref<C>::CountType;
      template<CT::Container C>
      using Iterator = typename Deref<C>::Iterator;

   public:
      template<bool REVERSE = false, CT::Container C> requires CT::ContainsMany<C>
      auto Remove(this C&, CT::NoIntent auto const&) -> Count<C>;

      template<CT::Container C> requires (CT::ContainsMany<C> and CT::IndexedLinearly<C>)
      auto RemoveAt(this C&, CT::Index auto, Count<C> = 1) -> Count<C>;

      template<CT::Container C> requires CT::ContainsMany<C>
      auto RemoveIt(this C&, Iterator<C> const&, Count<C> = 1) -> Iterator<C>;
      
      template<CT::Container C>
      auto RemoveDeepAt(this C&, CT::Index auto) -> Count<C>;

      /// Sets a new smaller count by destroying elements on the back.        
      /// Does nothing if 'desiredCount' is larger or equals the current.     
      ///   @attention never reallocates                                      
      ///   @param desiredCount the new count                                 
      template<CT::Container C> requires (CT::ContainsMany<C> and CT::IndexedLinearly<C>)
      void Trim(this C& self, Count<C> desiredCount) noexcept {
         const auto currentCount = self.GetCount();
         if (desiredCount >= currentCount)
            return;

         // If data doesn't need destructors just reduce count          
         if constexpr (C::TypeErased) {
            if (not self.GetType().GetDestructor()) {
               self.SetCount(desiredCount);
               return;
            }
         }
         else {
            if constexpr (not CT::Destroyable<TypeOf<C>>) {
               self.SetCount(desiredCount);
               return;
            }
         }

         // Call destructors and change count                           
         LglsAssert(self.GetAllocation(),
            "Can't trim disowned container");
         LglsAssert(self.GetAllocation()->GetUses() != 1,
            "Can't trim container used elsewhere");

         self.SelectInner(desiredCount, currentCount - desiredCount).FreeInner();
         self.SetCount(desiredCount);
      }

      template<CT::Container C> requires CT::ContainsMany<C>
      void Optimize(this C&);

      /// Destroy all elements but don't deallocate memory, unless we have to 
      ///   @attention will never reset state except disownment               
      ///   @attention will never reset type                                  
      void Clear(this auto& self) {
         const auto al = self.GetAllocation();
         if (self.IsEmpty()) {
            // Container is already empty. Just make sure that we're    
            // not gatekeeping an allocation that's used elsewhere.     
            if (al and al->GetUses() > 1) {
               // Since container is empty, all that this does is       
               // dereference and reset all allocations                 
               self.Free();
               self.ResetAllAllocations();
            }
            if_available(self.DisableDisowned());
            return;
         }
         
         if (not al) {
            // Data is either unallocated, static, or emergent.         
            // Free any emergent items, or static sparse items.         
            // Allocations are already nonexistent, so no need to reset.
            self.Free();
            if_available(self.SetReservedInner(0));
            if_available(self.SetHashTableInner(nullptr));
            self.ResetCount();
            if_available(self.DisableDisowned());
            return;
         }

         if (al->GetUses() == 1) {
            // Entry is used only in this block, so it's safe to        
            // destroy all elements. We will reuse the memory and type  
            // only if the container keeps track of the count separately
            self.template Free<false>();
            if_available(self.ResetHashTable());
            self.ResetCount();
         }
         else {
            // If reached, then data is referenced from multiple places.
            // Don't call local destructors, just dereference and clear 
            // allocation, because it isn't ours. Indirections will     
            // always get destroyed if they are fully dereferenced,     
            // unless disowned.                                         
            self.Free();
            self.ResetAllAllocations();
         }

         if_available(self.DisableDisowned());
      }

      /// Destroy all elements, deallocate block and reset state and type.    
      ///   @attention notice that heap pointer is not zeroed here, as it     
      ///      is not a requirement. It is UB if you GetRaw while count is 0! 
      void Reset(this auto& self) {
         if_available(self.Free());
         if_available(self.ResetAllAllocations());
         if_available(self.ResetState());
         if_available(self.ResetAllTypes());
      }
   };
}
