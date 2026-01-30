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
   /// Tracks count on the stack                                              
   /// Count shows how many elements inside a container are initialized       
   /// Stack-based counting increases the container size, but doesn't require 
   /// indirections, making count lookup faster and more cache-friendly       
   ///   @tparam ID the heap/stack ID to keep count of                        
   ///   @tparam T the count type                                             
   template<unsigned ID, class T>
   struct CountStack {
      using CTTI_Component = Yes<>;
      using CountType = T;
      using IndexType = Index::At<T>;
      using StackRequest = T;

      static constexpr int  ComponentPrecedence = -1000;
      static constexpr bool ContainsMany = true;

      /// Check if there are no initialized elements                          
      constexpr bool IsEmpty(this auto const& self) noexcept {
         return self.GetCountInner() == 0;
      }

      /// Get the number of initialized elements                              
      constexpr T GetCount(this auto const& self) noexcept {
         return self.GetCountInner();
      }

      /// Explicit boolean conversion to allow using containers in ifs        
      explicit operator bool(this auto const& self) noexcept {
         return self.GetCountInner() != 0;
      }

      T GetCountDeep() const noexcept;
      T GetCountItemsDeep() const noexcept;

   protected:
      template<unsigned>        friend struct Removal;
      template<unsigned>        friend struct Emplacement;
      template<unsigned, class> friend struct Insertion;
      template<unsigned, class> friend struct IndexedLinear;
      template<unsigned>        friend struct HeapMovable;
      
      /// Get count (inner)                                                   
      constexpr auto& GetCountInner(this auto&& self) noexcept {
         return self.template AccessStack<CountStack>();
      }
      
      /// Set the number of initialized elements                              
      constexpr void SetCountInner(this auto& self, T c) noexcept {
         self.GetCountInner() = c;
      }
      
      /// Default-initialize count to zero                                    
      constexpr void ConstructDefault(this auto& self) noexcept {
         self.SetCountInner(0);
      }
      
      /// Transfer from any kind of container, respecting intents             
      ///   @attention this is noop when constructing from deep intents,      
      ///      since element constructors might throw and stuff be partially  
      ///      inserted. In those cases, count is set by the heap components. 
      ///   @param intent the intent and container to transfer from           
      template<CT::Intent I> requires CT::Container<I>
      void ConstructFrom(this auto& self, I&& intent) {
         if constexpr (not CT::Copied<I> and not CT::Cloned<I>) {
            decltype(auto) from = LglsFwd(intent.what);
            self.SetCountInner(from.GetCountInner());
            if constexpr (I::ResetsOnMove())
               if_available(from.SetCountInner(0));
         }
      }
   };
}
