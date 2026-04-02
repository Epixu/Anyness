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
   /// A dynamic reserve, stored as a member variable.                        
   /// Will increase container's stack size.                                  
   ///   @tparam ID provider ID to keep reserve of                            
   ///   @tparam T the reserve type                                           
   ///   @tparam SHARED provider IDs that share the same reserve variable     
   template<Cid ID, class T, Cid...SHARED>
   struct ReserveStack {
      using CTTI_Component = Yes<>;
      using ReserveType = T;
      using StackRequest = T;
      static constexpr int ComponentPrecedence = -1000;
     
      static_assert(CT::Integer<T> and not CT::Signed<T>,
         "Reserve type must be an unsigned integer");

      /// Get the number of reserved (maybe uninitialized) elements           
      constexpr T GetReserved(this auto const& self) noexcept {
         return self.GetReservedInner();
      }

      /// Reserve a number of elements without initializing them.             
      /// If reserved data is smaller than currently initialized count, the   
      /// excess elements will be dereferenced/destroyed.                     
      ///   @param reserve number of elements to reserve                      
      template<CT::ContainsMany C>
      C& Reserve(this C& self, const T reserve) {
         if (reserve < self.GetCount())
            self.AllocateLess(reserve);
         else
            self.AllocateMore(reserve);
         return self;
      }

   protected:
      template<Cid, uint, uint, CT::HeapEntry...> friend struct HeapMovable;
      template<Cid>                               friend struct Emplacement;
      template<Cid, Cid...>                       friend struct Removal;

      /// Get reserved (inner)                                                
      constexpr auto& GetReservedInner(this auto&& self) noexcept {
         return self.template AccessStack<ReserveStack>();
      }
      
      /// Set the number of reserved elements                                 
      constexpr void SetReservedInner(this auto& self, T c) noexcept {
         self.GetReservedInner() = c;
      }
      
      /// Default-initialize reserve to zero                                  
      constexpr void ConstructDefault(this auto& self) noexcept {
         self.SetReservedInner(0);
      }
      
      /// Transfer from any kind of container, respecting intents             
      ///   @param intent the intent and container to transfer from           
      template<CT::Intent I> requires CT::Container<I>
      void ConstructFrom(this auto& self, I&& intent) {
         if constexpr (not CT::Copied<I> and not CT::Cloned<I>) {
            decltype(auto) from = LglsFwd(intent.what);
            if_available(self.SetReservedInner(from.GetReserved()));
            if constexpr (I::ResetsOnMove()) {
               if_available(from.SetReservedInner(0));
            }
         }
      }
   };
}
