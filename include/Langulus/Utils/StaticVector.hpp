///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "StaticCounter.hpp"

namespace Langulus
{
   template<class Key, int Index>
   struct StaticVectorReader
   {
      constexpr auto friend StaticVectorGet(StaticVectorReader<Key, Index>);
   };

   template<class Key, auto Value, int Index = unique_id<Key>()>
   struct StaticVectorWriter
   {
      constexpr auto friend StaticVectorGet(StaticVectorReader<Key, Index>) {
         return Value;
      }
   };
}