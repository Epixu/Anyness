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
   template<Cid ID, Cid...SHARED>
   struct Removal {
      using CTTI_Component = Yes<>;

      static constexpr Cid Id = ID;
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
      ///   @attention will never reset state or type                         
      void Clear(this auto& self) {
         const auto al = self.GetAllocation();
         if (not al) {
            // Data is either static or unallocated.                    
            // Don't call destructors, just clear it up.                
            if_available(self.SetReservedInner(0));
            if_available(self.SetHashTableInner(nullptr));
            self.ResetCount();
            return;
         }

         if (al->GetUses() == 1) {
            // Entry is used only in this block, so it's safe to        
            // destroy all elements. We will reuse the memory and type  
            // only if the container keeps track of the count separately
            self.DestroyAllElements();
            if_available(self.ResetHashTable());
         }
         else {
            // If reached, then data is referenced from multiple places.
            // Don't call destructors, just dereference.                
            self.template DestroyAllElements<false>();

            // Dereference memory                                       
            DecvqAllCast(al)->AddRef(-1);
            if_available(self.SetAllocationInner(nullptr));
            if_available(self.SetReservedInner(0));
            if_available(self.SetHashTableInner(nullptr));
         }

         self.ResetCount();
      }

      /// Destroy all elements, deallocate block and reset state and type,    
      /// if type-erased.                                                     
      ///   @attention notice that heap pointer is not zeroed here, as it     
      ///      is not a requirement. It is UB if you GetRaw while count is 0! 
      void Reset(this auto& self) {
         self.Free();
         if_available(self.SetAllocationInner(nullptr));
         if_available(self.SetReservedInner(0));
         if_available(self.SetHashTableInner(nullptr));
         self.ResetCount();
         if_available(self.ResetState());
         if_available(self.ResetType());
      }
   };
}
