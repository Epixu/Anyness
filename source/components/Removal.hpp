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
   ///   @tparam ID heap we're removing from                                  
   template<unsigned ID>
   struct Removal {
      using CTTI_Component = Yes<>;
      static constexpr int ComponentPrecedence = 3000;

   private:
      template<CT::Container C>
      using Count = typename Deref<C>::CountType;
      template<CT::Container C>
      using Iterator = typename Deref<C>::Iterator;

   public:
      template<bool REVERSE = false, CT::Container C> requires CT::ContainsMany<C>
      auto Remove(this C&, const CT::NoIntent auto&) -> Count<C>;

      template<CT::Container C> requires CT::ContainsMany<C>
      auto RemoveAt(this C&, CT::Index auto, Count<C> = 1) -> Count<C>;

      template<CT::Container C> requires CT::ContainsMany<C>
      auto RemoveIt(this C&, const Iterator<C>&, Count<C> = 1) -> Iterator<C>;
      
      template<CT::Container C>
      auto RemoveDeepAt(this C&, CT::Index auto) -> Count<C>;

      /// Sets a new smaller count by destroying elements on the back.        
      /// Does nothing if 'desiredCount' is larger or equals the current.     
      ///   @attention never reallocates                                      
      ///   @param desiredCount the new count                                 
      template<CT::Container C> requires CT::ContainsMany<C>
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
      template<CT::Container C>
      void Clear(this C& self) {
         const auto al = self.GetAllocation();
         if (not al) {
            // Data is either static or unallocated.                    
            // Don't call destructors, just clear it up.                
            self.SetHeapInner(nullptr);
            if_available(self.SetCountInner(0));
            if_available(self.SetHashInner(1));
            return;
         }

         if (al->GetUses() == 1) {
            // Entry is used only in this block, so it's safe to        
            // destroy all elements. We will reuse the memory and type  
            // only if the container keeps track of the count separately
            if constexpr (CT::ContainsOne<C>) {
               if constexpr (CT::DeeplyOwned<C>) {
                  #if LANGULUS_FEATURE(MANAGED_MEMORY)
                     self.DestroyElementDeepCustomPointers();
                  #else
                     self.DestroyElementDeepStandardPointers();
                  #endif
               }
               else self.DestroyElement();

               // Count is dictated by the availability of a heap       
               // pointer. We can't afford to reuse it, and be          
               // cleared at the same time, unless SetCountInner is     
               // available.                                            
               if_available(self.SetCountInner(0))
               else self.SetHeapInner(nullptr);
            }
            else {
               auto item = IterateHandles(self).begin();
               while (item) {
                  if constexpr (CT::DeeplyOwned<C>) {
                     #if LANGULUS_FEATURE(MANAGED_MEMORY)
                        item->DestroyElementDeepCustomPointers();
                     #else
                        item->DestroyElementDeepStandardPointers();
                     #endif
                  }
                  else item->DestroyElement();
                  
                  ++item;
               }
               if_available(self.SetCountInner(0));
            }
         }
         else {
            // If reached, then data is referenced from multiple places.
            // Don't call destructors, just clear it up and dereference.
            if constexpr (CT::DeeplyOwned<C>) {
               if constexpr (CT::ContainsOne<C>) {
                  #if LANGULUS_FEATURE(MANAGED_MEMORY)
                     self.template DestroyElementDeepCustomPointers<false>();
                  #else
                     self.template DestroyElementDeepStandardPointers<false>();
                  #endif
               }
               else {
                  auto item = IterateHandles(self).begin();
                  while (item) {
                     #if LANGULUS_FEATURE(MANAGED_MEMORY)
                        item->template DestroyElementDeepCustomPointers<false>();
                     #else
                        item->template DestroyElementDeepStandardPointers<false>();
                     #endif

                     ++item;
                  }
               }
            }

            // Dereference memory                                       
            al->AddRef(-1);
            self.SetHeapInner(nullptr);
            if_available(self.SetAllocationInner(nullptr));
            if_available(self.SetReserveInner(0));
            if_available(self.SetCountInner(0));
         }

         if_available(self.SetHashInner(1));
      }

      /// Destroy all elements, deallocate block and reset state and type,    
      /// if type-erased.                                                     
      void Reset(this auto& self) {
         self.Free();
         self.SetHeapInner(nullptr);
         if_available(self.SetAllocationInner(nullptr));
         if_available(self.SetCountInner(0));
         if_available(self.SetReserveInner(0));
         if_available(self.SetHashInner(1));
         if_available(self.ResetState());
         if_available(self.ResetType());
      }
   };
}
