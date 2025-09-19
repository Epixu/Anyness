///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include <Langulus/CT/Referenced.hpp>
#include <Langulus/IntentOf.hpp>


/// Simple type for testing Referenced types                                  
struct RT : Langulus::Referenced {
   int data;
   const char* t;
   bool destroyed = false;
   bool copied_in = false;
   bool cloned_in = false;
   bool moved_in  = false;
   bool moved_out = false;

   constexpr RT() : data {0}, t {nullptr} {}
   constexpr RT(int a) : data {a}, t {nullptr} {}
   constexpr RT(const char* tt) : data(0), t {tt} {}
   constexpr RT(const RT& rhs) : data(rhs.data), t {rhs.t}, copied_in {true} {}

   constexpr RT(RT&& rhs)
      : data(rhs.data), t {rhs.t}, moved_in {true} {
      rhs.moved_in = false;
      rhs.moved_out = true;
   }

   constexpr RT(Langulus::Clone<RT>&& rhs)
      : data(rhs->data), t {rhs->t}, cloned_in {true} { }

   constexpr ~RT() {
      destroyed = true;

      if (GetReferences() == 1)
         Reference(-1);
   }

   RT& operator = (const RT& rhs) {
      data = rhs.data;
      t = rhs.t;
      copied_in = true;
      moved_in = moved_out = false;
      return *this;
   }

   RT& operator = (RT&& rhs) {
      data = rhs.data;
      t = rhs.t;
      copied_in = false;
      moved_in = true;
      moved_out = false;
      rhs.copied_in = false;
      rhs.moved_in = false;
      rhs.moved_out = true;
      return *this;
   }

   operator const int& () const noexcept { return data; }
};
