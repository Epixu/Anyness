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
   /// A dynamic reserve, stored as a member variable                         
   /// Will increase container's stack size                                   
   ///   @tparam T type of the counter                                        
   ///   @tparam ID ID of the heap/stack to track capacity for                
   template<unsigned ID, class T>
   struct ReserveStack {
   private:
      T mReserved;

   public:
      using CTTI_Component = Yes<>;
      using ReserveType = T;
      static constexpr int ComponentPrecedence = 1000;

      /// Get the number of reserved (maybe uninitialized) elements           
      ///   @return the number of reserved (maybe uninitialized) elements     
      constexpr T GetReserved() const noexcept { return mReserved; }

   protected:
      template<unsigned>
      friend struct HeapMovable;
      template<unsigned>
      friend struct Removal;
   };
}
