///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
#include "Indexed-Common.hpp"
#include <Langulus/HashOf.hpp>


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Provides random element access by hashing a value of the provided ID   
   /// Uses a modified Robin Hood algorithm to reuse table space and minimize 
   /// movement on rehash. Keeps a local pointer to the hash table for faster 
   /// and more cache-friendly access.                                        
   ///   @tparam ID the stack/heap we're indexing                             
   ///   @tparam HASH type of the hash                                        
   template<Cid ID, class HASH = Hash>
   struct IndexedHashStack : IndexedCommon<ID> {
      using TableType        = uint8_t;
      using HeapRequest      = PerElement<TableType>;
      using StackRequest     = TableType*;
      using IteratorCategory = ::std::random_access_iterator_tag;

   protected:
      friend struct IndexedCommon<ID>;
   };
}
