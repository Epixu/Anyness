///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
#include <Langulus/CT/Index.hpp>


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Defines count as a member                                              
   /// Count shows how many elements inside a container are initialized       
   /// Stack-based counting increases the container size, but doesn't require 
   /// indirections, making count lookup faster and more cache-friendly       
   ///   @tparam ID - the heap/stack ID to keep count of                      
   ///   @tparam T - the count type                                           
   template<unsigned ID = 0, class T = size_t>
   struct CountStack {
   protected:
      template<unsigned>        friend struct Removal;
      template<unsigned, class> friend struct Insertion;
      template<class>           friend struct IndexedLinear;
      template<unsigned>        friend struct HeapMovable;

      /// Set the number of initialized elements                              
      void SetCount(T count) noexcept { mCount = count; }

   private:
      // The count on the stack                                         
      // It is private so that it isn't accessible when inherited       
      // It has to be accessed through GetCount() and SetCount()        
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
}
