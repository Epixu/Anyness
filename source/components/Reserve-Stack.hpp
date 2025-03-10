#pragma once


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// A dynamic reserve, stored as a member variable                         
   /// Will increase container's stack size                                   
   ///   @tparam T - type of the counter                                      
   ///   @tparam ID - ID of the heap/stack to track capacity for              
   ///                                                                        
   template<unsigned ID = 0, class T = ::std::size_t>
   struct ReserveStack {
   private:
      T mReserved = 0;

   public:
      using CTTI_Component = Yes;

      constexpr T GetReserved() const noexcept { return mReserved; }
   };

} // namespace Langulus::Anyness::Component
