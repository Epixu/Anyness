///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once

/// Original code from:                                                       
/// stackoverflow.com/questions/6166337/does-c-support-compile-time-counters  
template<auto Id>
struct counter {
   using tag = counter;

   struct generator {
      friend consteval auto is_defined(tag) { return true; }
   };

   friend consteval auto is_defined(tag);

   template<typename Tag = tag, auto = is_defined(Tag{})>
   static consteval auto exists(auto) {
      return true;
   }

   static consteval auto exists(...) {
      return generator(), false;
   }
};

template<auto Id = int{}, typename = decltype([]{})>
consteval auto unique_id() {
   if constexpr (not counter<Id>::exists(Id))
      return Id;
   else
      return unique_id<Id + 1>();
}

static_assert(unique_id() == 0);
static_assert(unique_id() == 1);
static_assert(unique_id() == 2);
static_assert(unique_id() == 3);