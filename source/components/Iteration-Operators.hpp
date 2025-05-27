#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::Component
{
   
   ///                                                                        
   /// Adds prefix and suffix operators for increment and decrement           
   /// These operators are fundamentally unsafe, so the API is protected,     
   /// used mainly internally in other components and/or iterators            
   ///   @tparam ID - heap we're iterating                                    
   ///                                                                        
   template<unsigned ID = 0>
   struct IterationOperators {
      using CTTI_Component = Yes;

      /// Prefix increment operator                                           
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @return reference to this, after being modified                   
      template<CT::Container C>
      constexpr C& operator ++ (this C& self) noexcept {
         // Increment the heap pointer                                  
         if constexpr (C::template HasComponent<HeapReference<ID>>) {
            auto& heap = self.HeapReference<ID>::mHeap;
            heap = static_cast<uint8_t*>(heap) + self.GetStride();
         }
         else if constexpr (C::template HasComponent<HeapMovable<ID>>) {
            auto& heap = self.HeapMovable<ID>::mHeap;
            heap = static_cast<uint8_t*>(heap) + self.GetStride();
         }
         else static_assert(false, "Container doesn't have a compatible heap component");

         // Increment deep ownership entries, but only if on the stack  
         if constexpr (C::template HasComponent<DeepOwnershipStack<ID>>)
            ++self.DeepOwnershipStack<ID>::mEntries;

         return self;
      }

      /// Suffix increment operator                                           
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @return a copy of the state, before modifying it                  
      template<CT::Container C>
      constexpr C operator ++ (this C& self, int) noexcept {
         C backup = self;
         ++self;
         return backup;
      }

      /// Prefix decrement operator                                           
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @return reference to this, after being modified                   
      template<CT::Container C>
      constexpr C& operator -- (this C& self) noexcept {
         // Decrement the heap pointer                                  
         if constexpr (C::template HasComponent<HeapReference<ID>>) {
            auto& heap = self.HeapReference<ID>::mHeap;
            heap = static_cast<uint8_t*>(heap) - self.GetStride();
         }
         else if constexpr (C::template HasComponent<HeapMovable<ID>>) {
            auto& heap = self.HeapMovable<ID>::mHeap;
            heap = static_cast<uint8_t*>(heap) - self.GetStride();
         }
         else static_assert(false, "Container doesn't have a compatible heap component");

         // Decrement deep ownership entries, but only if on the stack  
         if constexpr (C::template HasComponent<DeepOwnershipStack<ID>>)
            --self.DeepOwnershipStack<ID>::mEntries;

         return self;
      }

      /// Suffix decrement operator                                           
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @return a copy of the state, before modifying it                  
      template<CT::Container C>
      constexpr C operator -- (this C& self, int) noexcept {
         C backup = self;
         --self;
         return backup;
      }
   };

} // namespace Langulus::Anyness::Component
