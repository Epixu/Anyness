#pragma once
#include "../Container.hpp"


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
      using ReserveType = T;

      /// Get the number of reserved (maybe uninitialized) elements           
      ///   @return the number of reserved (maybe uninitialized) elements     
      constexpr auto GetReserved() const noexcept { return mReserved; }

   protected:
      template<unsigned>
      friend struct HeapMovable;

      /// Set number of reserved elements                                     
      constexpr void SetReserved(ReserveType r) noexcept { mReserved = r;    }
   };

} // namespace Langulus::Anyness::Component
