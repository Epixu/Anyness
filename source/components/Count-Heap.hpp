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
   ///   @tparam ID provider ID to keep count of                              
   ///   @tparam T the count type                                             
   ///   @tparam SHARED provider IDs that share the same count variable       
   template<Cid ID, class T, Cid...SHARED>
   struct CountHeap {
      using CTTI_Component = Yes<>;
      using CountType   = T;
      using IndexType   = Index::At<T>;
      using HeapRequest = T;

      static constexpr Cid  Id = ID;
      static constexpr int  ComponentPrecedence = -1000;
      static constexpr bool ContainsMany = true;

      using Dimensions = Values<ID, SHARED...>;

      static_assert(CT::Integer<T> and not CT::Signed<T>,
         "Count type must be an unsigned integer");

      /// Check if there are no initialized elements                          
      template<Cid SID = ID>
      constexpr bool IsEmpty(this auto const& self) noexcept {
         static_assert(SID == ID or ((SID == SHARED) or ...));
         return self.template GetCountInner<SID>() == 0;
      }

      /// Get the number of initialized elements                              
      template<Cid SID = ID, CT::Container C>
      constexpr T GetCount(this C const& self) noexcept {
         static_assert(SID == ID or ((SID == SHARED) or ...));
         return self.template GetCountInner<SID>();
      }

      /// Explicit boolean conversion to allow using containers in ifs        
      explicit constexpr operator bool(this auto const& self) noexcept {
         return self.GetCountInner() != 0;
      }

      T GetCountDeep() const noexcept;
      T GetCountItemsDeep() const noexcept;

   protected:
      LglsComRemoval(friend);
      LglsComEmplacement(friend);
      LglsComInsertion(friend);
      LglsComIndexedCommon(friend);
      LglsComIndexedLinear(friend);
      LglsComHeapMovable(friend);
      LglsComConversion(friend);

      /// Get count (inner)                                                   
      template<Cid SID = ID>
      constexpr auto& GetCountInner(this auto&& self) noexcept {
         static_assert(SID == ID or ((SID == SHARED) or ...));
         return self.template AccessHeap<CountHeap>();
      }
      
      /// Set the number of initialized elements                              
      template<Cid SID = ID>
      constexpr void SetCountInner(this auto& self, T c) noexcept {
         static_assert(SID == ID or ((SID == SHARED) or ...));
         self.template GetCountInner<SID>() = c;
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
      template<Cid SID = ID, CT::Container C>
      constexpr void ResetCount(this C& self) noexcept {
         static_assert(SID == ID or ((SID == SHARED) or ...));
         self.template SetCountInner<SID>(0);
         if_available(self.template SetHashInner<SID>(1));
      }
   };
}
