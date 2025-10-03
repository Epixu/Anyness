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
   ///   A compile-time count                                                 
   ///                                                                        
   ///   Count shows how many elements inside a container are initialized.    
   ///   Compile-time counting isn't really counting, and doesn't take up     
   /// space, but is useful for defining single-element containers that       
   /// still need the API required to function alongside other components.    
   ///   In these cases, count is equal to COUNT if container has a heap      
   /// component that has been allocated - otherwise it is 0. If no heap      
   /// component exists or can't be null, then the count is always COUNT.     
   template<auto COUNT>
   struct CountStatic {
      using CTTI_Component  = Yes<>;
      using CTTI_Contiguous = Maybe<COUNT == 1>;
      static constexpr int  ComponentPrecedence = 1000;
      static constexpr bool ContainsMany = COUNT > 1;

      static_assert(COUNT > 0, "Can't have a container of zero or negative count");
      using CountType   = decltype(COUNT);
      using ReserveType = CountType;
      using IndexType   = Index::At<CountType>;

      /// Equal to COUNT if container has a heap component that has been      
      /// allocated - zero otherwise. If no heap component exists, then the   
      /// count is always COUNT.                                              
      constexpr auto GetCount(this auto const& self) noexcept -> CountType {
         return self.GetCountInner();
      }
      
      /// Check if empty                                                      
      constexpr bool IsEmpty(this auto const& self) noexcept {
         return self.GetCountInner() == CountType {0};
      }

      /// Explicit boolean conversion to allow using containers in ifs        
      constexpr explicit operator bool(this auto const& self) noexcept {
         return self.GetCountInner() != CountType {0};
      }

      /// Static count means static reserve                                   
      /*constexpr auto GetReserved(this auto const& self) noexcept -> CountType {
         return self.GetCountInner();
      }*/

   protected:
      template<unsigned> friend struct HeapMovable;

      /// Get count (inner)                                                   
      template<CT::Container C>
      constexpr auto GetCountInner(this C const& self) noexcept -> CountType {
         if constexpr (CT::HasVariableCount<C>)
            return self.GetHeapInner() ? COUNT : CountType {0};
         else
            return COUNT;
      }
   };
}
