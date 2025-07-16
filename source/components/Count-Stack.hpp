#pragma once
#include "../Container.hpp"
#include <Langulus/CT/Index.hpp>


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Defines count as a member                                              
   /// Count shows how many elements inside a container are initialized       
   /// Stack-based counting increases the container size, but doesn't require 
   /// indirections, making count lookup faster and more cache-friendly.      
   ///   @tparam ID - the heap/stack ID to keep count of                      
   ///   @tparam T - the count type                                           
   template<unsigned ID = 0, class T = ::std::size_t>
   struct CountStack {
   private:
      T mCount;

   public:
      using CTTI_Component = Yes<>;
      using CountType = T;
      using IndexType = Index::At<T>;

      /// Check if there are no initialized elements                          
      constexpr bool IsEmpty() const noexcept { return mCount == 0; }

      /// Get the number of initialized elements                              
      constexpr T GetCount() const noexcept { return mCount; }

      /// Explicit boolean conversion to allow using containers in ifs        
      explicit operator bool() const noexcept { return mCount != 0; }

      T GetCountDeep() const noexcept;
      T GetCountItemsDeep() const noexcept;
   };

} // namespace Langulus::Anyness::Component
