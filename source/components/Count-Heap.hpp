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
#include <Langulus/CT/Contiguous.hpp>


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Defines count as a part of the heap                                    
   /// Count shows how many elements inside a container are initialized       
   /// Heap-based counting keeps the counter inside the container's heap      
   /// allocation, and requires an indirection everytime count is accessed.   
   /// It is a bit slower and less cache-friendly, but results in more        
   /// compact containers                                                     
   ///   @tparam ID - the heap ID to keep count of                            
   ///   @tparam T - the count type                                           
   template<unsigned ID = 0, class T = size_t>
   struct CountHeap {
      using CTTI_Component = Yes<>;
      using CountType = T;
      using IndexType = Index::At<T>;
      static constexpr int ComponentPrecedence = 1000;

      /// Get the number of initialized elements                              
      template<CT::Container C>
      T GetCount(this C const& self) noexcept {
         if constexpr (CT::Contiguous<C>) {
            //TODO we can determine count by subtracting the allocation pointer from the heap pointer
            // to at least determine if it is zero (when type-erased) or calculate it exactly (when statically typed),
            // which can save on an indirection in many cases. this won't work for maps for obvious reasons
            // but is very efficient for contiguous containers
         }
         else return self.GetHeap<ID>().GetElement<T>();
      }

      /// Check if there are no initialized elements                          
      bool IsEmpty(this auto const& self) noexcept {
         return self.GetCount() == 0;
      }

      /// Explicit boolean conversion to allow using containers in ifs        
      explicit operator bool(this auto const& self) noexcept {
         return self.GetCount() != 0;
      }
   };
}
