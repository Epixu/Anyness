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
#include <Langulus/Anyness/Text.hpp>
#include <Langulus/Anyness/Bytes.hpp>


/// Simple type for testing Referenced types                                  
struct RT : Langulus::Referenced {
   using Text = Langulus::Anyness::Text;
   //using CTTI_MapsTo = Text;

   int data;
   const char* t;
   bool destroyed = false;
   bool copied_in = false;
   bool cloned_in = false;
   bool copy_intent_in = false;
   bool disown_intent_in = false;
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

   constexpr RT(Langulus::Disown<RT>&& rhs)
      : data(rhs->data), t {rhs->t}, disown_intent_in {true} { }
   constexpr RT(Langulus::Copy<RT>&& rhs)
      : data(rhs->data), t {rhs->t}, copy_intent_in {true} { }
   constexpr RT(Langulus::Clone<RT>&& rhs)
      : data(rhs->data), t {rhs->t}, cloned_in {true} { }

   constexpr ~RT() {
      destroyed = true;

      if (GetReferences() == 1)
         Reference(-1);
   }

   constexpr RT& operator = (const RT& rhs) {
      data = rhs.data;
      t = rhs.t;
      copied_in = true;
      moved_in = moved_out = false;
      return *this;
   }

   constexpr RT& operator = (RT&& rhs) {
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

   constexpr operator const int& () const noexcept {
      return data;
   }

   explicit operator Text () const noexcept {
      if (copied_in) return "RT(copied)";
      if (cloned_in) return "RT(cloned)";
      if (copy_intent_in) return "RT(copied with intent)";
      if (disown_intent_in) return "RT(disowned)";
      if (moved_in) return "RT(moved-in)";
      if (moved_out) return "RT(moved-out)";
      return "RT(unknown)";
   }

   auto GetHash() const noexcept {
      return Langulus::HashOf(data);
   }
};

static_assert(not Langulus::CT::Deep<RT>);

namespace Langulus::CTTI
{
   template<>
   struct ConverterFrom<RT, LglsUniqueConverterIndex(RT)> {
      LANGULUS_MORPHISM(Anyness::Text);
   };

   template<>
   struct ConverterFrom<RT, LglsUniqueConverterIndex(RT)> {
      LANGULUS_MORPHISM(Anyness::Bytes);

      template<class TO>
      static constexpr TO Convert(RT const& from) noexcept {
         TO bytes;
         bytes += from.data;
         uint8_t mask = 0;
         if (from.destroyed)
            mask |= 1;
         if (from.copied_in)
            mask |= 2;
         if (from.cloned_in)
            mask |= 4;
         if (from.copy_intent_in)
            mask |= 8;
         if (from.disown_intent_in)
            mask |= 16;
         if (from.moved_in)
            mask |= 32;
         if (from.moved_out)
            mask |= 64;
         bytes += mask;
         return bytes;
      }
   };
}

static_assert(Langulus::CT::ConvertibleCustom<RT, Langulus::Anyness::Text>);
static_assert(Langulus::CT::ConvertibleCustom<RT, Langulus::Anyness::Bytes>);