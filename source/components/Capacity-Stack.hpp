#pragma once


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// A dynamic capacity, stored as a member variable                        
   ///   @tparam T - type of the counter                                      
   ///   @tparam ID - ID of the heap/stack to track capacity for              
   ///                                                                        
   template<class T = ::std::size_t, unsigned ID = 0>
   struct CapacityStack {
   private:
      T mCapacity = 0;

   public:
      using CTTI_Component = Yes;

      constexpr T GetCapacity() const noexcept { return mCapacity; }
   };

} // namespace Langulus::Anyness::Component
