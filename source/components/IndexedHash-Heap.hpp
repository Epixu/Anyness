///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
#include <Langulus/HashOf.hpp>


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Provides random element access by hashing a value of the provided ID.  
   /// Uses a modified Robin Hood algorithm to reuse table space and minimize 
   /// movement on rehash. Doesn't keep a local pointer to the hash table,    
   /// and istead recalculates it on demand from the heap.                    
   ///   @tparam ID the stack/heap we're indexing                             
   ///   @tparam HASH type of the hash                                        
   template<unsigned ID, class HASH>
   struct IndexedHashHeap {
      using CTTI_Component  = Yes<>;
      using CTTI_Contiguous = No;
      using TableType       = uint8_t;
      using HeapRequest     = PerElement<TableType>;
      
      static constexpr bool Indexed = true;
      static constexpr int  ComponentPrecedence = 3000;

   protected:
      template<CT::Container C>
      using Count = typename Deref<C>::CountType;

      template<CT::Container C>
      using Key = Tmut<C, typename Deref<C>::KeyMut, typename Deref<C>::Key>;

      template<CT::Container C>
      using Val = Tmut<C, typename Deref<C>::ValMut, typename Deref<C>::Val>;

   public:
      template<CT::Container C>
      auto operator[] (this C&&, Key<C>) has_assumptions -> Val<C>;
   };
}
