#pragma once


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// A dynamic capacity derived from the heap directly                      
   ///   @tparam T - type of the counter                                      
   ///   @tparam ID - ID of the heap to track capacity for                    
   ///                                                                        
   template<class T = ::std::size_t, unsigned ID = 0>
   struct CapacityHeap {
      using CTTI_Component = Yes;

      template<CT::Container C>
      T GetCapacity(this const C& self) noexcept {
         auto heap = self.GetHeap<ID>();
         return heap ? heap->GetCapacity() : 0;
      }
   };

} // namespace Langulus::Anyness::Component
