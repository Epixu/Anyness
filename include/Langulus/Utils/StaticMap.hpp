///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
/// Source - https://stackoverflow.com/a/79521082                             
/// Posted by HolyBlackCat                                                    
/// Retrieved 2026-08-31, License - CC BY-SA 4.0                              
#pragma once

namespace Langulus
{
   template<class Key>
   struct StaticMapReader
   {
      constexpr auto friend StaticMapGet(StaticMapReader<Key>);
   };

   template<class Key, auto Value>
   struct StaticMapWriter
   {
      constexpr auto friend StaticMapGet(StaticMapReader<Key>) {
         return Value;
      }
   };
}