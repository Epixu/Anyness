#pragma once


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// A dynamic reserve derived from the heap directly                       
   /// As such, it will not increase container's stack size                   
   ///   @tparam T - type of the counter                                      
   ///   @tparam ID - ID of the heap to track capacity for                    
   ///                                                                        
   template<unsigned ID = 0, class T = ::std::size_t>
   struct ReserveHeap {
      using CTTI_Component = Yes;

      T GetReserved(this const auto& self) noexcept {
         auto heap = self.template GetHeap<ID>();
         return heap ? heap->GetReserved() : 0;
      }

      void Reserve(this auto& self, const T count) {
         if (count < self.template GetCount<ID>())
            self.template AllocateLess<ID>(count);
         else
            self.template AllocateMore<ID>(count);
      }
   };

} // namespace Langulus::Anyness::Component
