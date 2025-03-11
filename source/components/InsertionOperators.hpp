#pragma once
#include "Insertion.hpp"


namespace Langulus::Anyness::Component
{
   
   ///                                                                        
   /// Adds operators for front (>>) and back (<<) insertion                  
   ///   @tparam AS - type to serialize as before inserting. Useful for byte  
   ///      and text containers. Use void to insert without serialization     
   template<class AS = void>
   struct InsertionOperators {
      using CTTI_Component = Yes;

      /// Push back                                                           
      template<CT::Container C, class A>
      C& operator << (this C&, A&&) requires RangeInsertable<C, A>;

      /// Push front                                                          
      template<CT::Container C, class A>
      C& operator >> (this C&, A&&) requires RangeInsertable<C, A>;
   };

} // namespace Langulus::Anyness::Component
