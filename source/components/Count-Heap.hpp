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
   ///   @tparam ID the heap ID to keep count of                              
   ///   @tparam T the count type                                             
   template<Cid ID, class T>
   struct CountHeap {
      using CTTI_Component = Yes<>;
      using CountType   = T;
      using IndexType   = Index::At<T>;
      using HeapRequest = T;

      static constexpr int  ComponentPrecedence = -1000;
      static constexpr bool ContainsMany = true;
      
      /// Check if there are no initialized elements                          
      constexpr bool IsEmpty(this auto const& self) noexcept {
         return self.GetCountInner() == 0;
      }

      /// Get the number of initialized elements                              
      template<CT::Container C>
      constexpr T GetCount(this C const& self) noexcept {
         return self.GetCountInner();
      }

      /// Explicit boolean conversion to allow using containers in ifs        
      explicit constexpr operator bool(this auto const& self) noexcept {
         return self.GetCountInner() != 0;
      }

      T GetCountDeep() const noexcept;
      T GetCountItemsDeep() const noexcept;

   protected:
      template<Cid>             friend struct Removal;
      template<Cid>             friend struct Emplacement;
      template<Cid, class>      friend struct Insertion;
      template<Cid, class>      friend struct IndexedLinear;
      template<Cid, uint, uint, CT::Sparse> friend struct HeapMovable;
                                friend struct Conversion;

      /// Get count (inner)                                                   
      constexpr auto& GetCountInner(this auto&& self) noexcept {
         return self.template AccessHeap<CountStack>();
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
               from.SetCountInner(0);
         }
      }

      /// Reset count (inner)                                                 
      ///   @attention doesn't destroy elements, only resets hash and count   
      template<CT::Container C>
      constexpr void ResetCount(this C& self) noexcept {
         self.SetCountInner(0);
         if_available(self.SetHashInner(1));
      }
   };
}
