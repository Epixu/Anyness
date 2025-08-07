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
   /// Implements removal for containers                                      
   ///   @tparam ID - heap we're removing from                                
   template<unsigned ID = 0>
   struct Removal {
      using CTTI_Component = Yes<>;

   private:
      template<CT::Container C>
      using Count = typename Deref<C>::CountType;
      template<CT::Container C>
      using Iterator = typename Deref<C>::Iterator;

   public:
      template<bool REVERSE = false, CT::Container C>
      auto Remove(this C&, const CT::NoIntent auto&) -> Count<C>;

      template<CT::Container C>
      auto RemoveAt(this C&, CT::Index auto, Count<C> = 1) -> Count<C>;

      template<CT::Container C>
      auto RemoveAtDeep(this C&, CT::Index auto) -> Count<C>;

      template<CT::Container C>
      auto RemoveIt(this C&, const Iterator<C>&, Count<C> = 1) -> Iterator<C>;

      /// Sets a new smaller count by destroying elements on the back         
      /// Does nothing if count is larger or equals the current count         
      /// Never reallocates                                                   
      ///   @param desiredCount - the new count                               
      template<CT::Container C>
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

      template<CT::Container C>
      void Optimize(this C&);

      /// Destroy all elements but don't deallocate memory                    
      template<CT::Container C>
      void Clear(this C& self) {
         if (not self.mAllocation) {
            // Data is either static or unallocated                     
            // Don't call destructors, just clear it up                 
            self.mHeap = nullptr;
            self.mCount = 0;
            if constexpr (requires { self.mReserved; })
               self.mReserved = 0;
            self.ResetType();
            return;
         }

         if (self.mAllocation->GetUses() == 1) {
            // Entry is used only in this block, so it's safe to        
            // destroy all elements. We will reuse the entry and type   
            if constexpr (requires { self.FreeDeep(); })
               self.FreeDeep();
            self.mCount = 0;
         }
         else {
            // If reached, then data is referenced from multiple places 
            // Don't call destructors, just clear it up and dereference 
            self.mAllocation->Free();
            self.mAllocation = nullptr;
            self.mHeap = nullptr;
            self.mCount = 0;
            if constexpr (requires { self.mReserved; })
               self.mReserved = 0;
            self.ResetType();
         }
      }

      /// Destroy all elements, deallocate block and reset state              
      template<CT::Container C>
      void Reset(this C& self) {
         self.Free();
         self.mHeap = nullptr;
         self.mAllocation = nullptr;
         self.mCount = 0;
         if constexpr (requires { self.mReserved; })
            self.mReserved = 0;
         self.ResetState();
         self.ResetType();
      }
   };
}
