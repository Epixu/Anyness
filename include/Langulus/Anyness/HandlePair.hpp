///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Handle.hpp"


namespace Langulus::Anyness
{
   template<CT::Handle K, CT::Handle V>
   struct THandlePair {
      using CTTI_Handle    = Yes<>;
      using CTTI_Pair      = Yes<>;
      using CTTI_Typed     = Types<TypeOf<K>, TypeOf<V>>;
      using CTTI_ReflectAs = void;
      //using Denser         = Types<typename K::Denser,   typename V::Denser>;
      //using DeepType       = Types<typename K::DeepType, typename V::DeepType>;

      K key;
      V val;

      auto GetHash() const -> Hash {
         return key.GetHash() ^ val.GetHash();
      }
   };
}
