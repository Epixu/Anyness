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
   /// Refers back to this particular component instance through the deduced  
   /// 'this'. Just for convenience. It is #undef-ed at the end of this file. 
   #define ThisCom self.ReserveStack<T, ID, SHARED...>

   ///                                                                        
   /// A dynamic reserve, stored as a member variable.                        
   /// Will increase container's stack size.                                  
   ///   @tparam T the reserve counter type                                   
   ///   @tparam ID provider ID to keep reserve of                            
   ///   @tparam SHARED provider IDs that share the same reserve variable     
   template<class T, Cid ID, Cid...SHARED>
   struct ReserveStack {
      using CTTI_Component = Yes<>;
      using CTTI_ReflectAs = void;
      using ReserveType = T;
      using StackRequest = T;

      static constexpr Cid Id = ID;
      static constexpr int ComponentPrecedence = -1000;
      template<Cid SID>
      static constexpr bool Relevant = IdMatch<SID, ID, SHARED...>;

      static_assert(CT::Integer<T> and not CT::Signed<T>,
         "Reserve type must be an unsigned integer");

      /// Get the number of reserved (maybe uninitialized) elements           
      template<Cid SID = ID> requires Relevant<SID>
      constexpr T GetReserved(this auto const& self) noexcept {
         return ThisCom::GetReservedInner();
      }

      /// Reserve a number of elements without initializing them.             
      /// If reserved data is smaller than currently initialized count, the   
      /// excess elements will be dereferenced/destroyed.                     
      ///   @param reserve number of elements to reserve                      
      template<Cid SID = ID, CT::ContainsMany C> requires Relevant<SID>
      C& Reserve(this C& self, const T reserve) {
         if (reserve < self.template GetCount<SID>())
            self.template AllocateLess<SID>(reserve);
         else
            self.template AllocateMore<SID>(reserve);
         return self;
      }

   protected:
      LglsComHeapMovable(friend);
      LglsComRemoval(friend);
      LglsComEmplacement(friend);

      /// Get reserved (inner)                                                
      constexpr auto& GetReservedInner(this auto&& self) noexcept {
         return self.template AccessStack<ReserveStack>();
      }
      
      /// Set the number of reserved elements                                 
      constexpr void SetReservedInner(this auto& self, T c) noexcept {
         ThisCom::GetReservedInner() = c;
      }
      
      /// Default-initialize reserve to zero                                  
      constexpr void ConstructDefault(this auto& self) noexcept {
         ThisCom::SetReservedInner(0);
      }
      
      /// Transfer from any kind of container, respecting intents             
      ///   @param intent the intent and container to transfer from           
      template<CT::Intent I>
      requires (CT::Container<I> and not CT::Copied<I> and not CT::Cloned<I>)
      void ConstructFrom(this auto& self, I&& intent) {
         decltype(auto) from = LglsFwd(intent.what);
         ThisCom::SetReservedInner(from.GetReserved());
         if constexpr (I::ResetsOnMove()) {
            if_available(from.SetReservedInner(0));
         }
      }
   };

   #undef ThisCom
}
