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
      using Count = typename C::CountType;
      template<CT::Container C>
      using Iterator = typename C::Iterator;

   public:
      template<bool REVERSE = false, CT::Container C>
      auto Remove(this C&, const CT::NoIntent auto&) -> Count<C>;

      template<CT::Container C>
      auto RemoveAt(this C&, CT::Index auto, Count<C> = 1) -> Count<C>;

      template<CT::Container C>
      auto RemoveAtDeep(this C&, CT::Index auto) -> Count<C>;

      template<CT::Container C>
      auto RemoveIt(this C&, const Iterator<C>&, Count<C> = 1) -> Iterator<C>;

      template<CT::Container C>
      void Trim(this C&, Count<C>);

      template<CT::Container C>
      void Optimize(this C&);

      /// Destroy all elements but don't deallocate memory                    
      template<CT::Container C>
      void Clear(this C& self) {
         auto allocation = self.GetAllocation();
         if (not allocation) {
            // Data is either static or unallocated                     
            // Don't call destructors, just clear it up                 
            self.SetHeap(nullptr);
            self.SetCount(0);
            self.SetReserved(0);
            self.ResetType();
            return;
         }

         if (allocation->GetUses() == 1) {
            // Entry is used only in this block, so it's safe to        
            // destroy all elements. We will reuse the entry and type   
            if constexpr (requires { self.FreeDeep(); })
               self.FreeDeep();
            self.SetCount(0);
         }
         else {
            // If reached, then data is referenced from multiple places 
            // Don't call destructors, just clear it up and dereference 
            allocation->Free();
            self.SetHeap(nullptr);
            self.SetAllocation(nullptr);
            self.SetCount(0);
            self.SetReserved(0);
            self.ResetType();
         }
      }

      /// Destroy all elements, deallocate block and reset state              
      template<CT::Container C>
      void Reset(this C& self) {
         self.Free();
         self.SetHeap(nullptr);
         self.SetAllocation(nullptr);
         self.SetCount(0);
         self.SetReserved(0);
         mState &= DataState::Typed;
         self.ResetType();
      }
   };
}
