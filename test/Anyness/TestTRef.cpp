///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#include "../Main.hpp"
#include <Langulus/Anyness/TRef.hpp>

using namespace Langulus;
using Anyness::TRef;
using Anyness::Allocator;

namespace
{
   /// Simple type for testing Referenced types                               
   struct RT : Referenced {
      int data;
      const char* t;
      bool destroyed = false;
      bool copied_in = false;
      bool cloned_in = false;
      bool moved_in  = false;
      bool moved_out = false;

      RT() : data {0}, t {nullptr} {}
      RT(int a) : data {a}, t {nullptr} {}
      RT(const char* tt) : data(0), t {tt} {}
      RT(const RT& rhs) : data(rhs.data), t {rhs.t}, copied_in {true} {}

      RT(RT&& rhs)
         : data(rhs.data), t {rhs.t}, moved_in {true} {
         rhs.moved_in = false;
         rhs.moved_out = true;
      }

      RT(Clone<RT>&& rhs) : data(rhs->data), t {rhs->t}, cloned_in {true} { }

      ~RT() {
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
}


TEMPLATE_TEST_CASE("Shared pointer", "[TRef]",
   TRef<RT>,
   TRef<const RT>,
   TRef<int>,
   TRef<const int>
) {
   static Allocator::State memoryState;
   using T  = TestType;
   using TT = TypeOf<T>;
   
   STATIC_REQUIRE(T{} == T{});
   STATIC_REQUIRE(T{} == nullptr);
   STATIC_REQUIRE(T{nullptr} == T{nullptr});
   STATIC_REQUIRE(T{nullptr} == nullptr);

   GIVEN("A nullptr-initialized templated shared pointer") {
      T pointer {nullptr};
      T pointer2 {nullptr};

      REQUIRE_FALSE(pointer.GetRaw());
      REQUIRE_FALSE(pointer);
      REQUIRE(pointer == pointer2);
   }

   GIVEN("A default-initialized templated shared pointer") {
      T pointer;
      T pointer2;

      REQUIRE_FALSE(pointer.GetRaw());
      REQUIRE_FALSE(pointer);
      REQUIRE(pointer == pointer2);

      WHEN("Create an instance") {
         pointer.Emplace(5);

         REQUIRE(*pointer == 5);
         REQUIRE(pointer.GetAllocation());
         REQUIRE(pointer.GetUses() == 1);
         if constexpr (CT::Referenced<TT>)
            REQUIRE(pointer->GetReferences() == 1);
      }

      WHEN("Create and copy an instance") {
         pointer.Emplace(5);
         pointer2 = pointer;

         REQUIRE(pointer == pointer2);
         REQUIRE(*pointer == 5);
         REQUIRE(*pointer2 == 5);
         REQUIRE(pointer.GetAllocation());
         REQUIRE(pointer2.GetAllocation());
         REQUIRE(pointer.GetUses() == 2);
         REQUIRE(pointer2.GetUses() == 2);
         if constexpr (CT::Referenced<TT>)
            REQUIRE(pointer->GetReferences() == 1); //TODO major design change - TRef is no longer deep referenced for now, let's see how that goes
      }

      WHEN("Create and move an instance") {
         pointer.Emplace(5);
         pointer2 = ::std::move(pointer);

         REQUIRE_FALSE(pointer);
         REQUIRE(pointer2);
         REQUIRE(*pointer2 == 5);
         REQUIRE_FALSE(pointer.GetAllocation());
         REQUIRE(pointer2.GetAllocation());
         REQUIRE(pointer.GetUses() == 0);
         REQUIRE(pointer2.GetUses() == 1);
         if constexpr (CT::Referenced<TT>)
            REQUIRE(pointer2->GetReferences() == 1);
      }

      WHEN("Overwrite an instance") {
         pointer.Emplace(5);
         IF_LANGULUS_MANAGED_MEMORY(auto backup = pointer.GetRaw());
         pointer2.Emplace(6);
         pointer = pointer2;

         REQUIRE(pointer == pointer2);
         REQUIRE(*pointer == 6);
         REQUIRE(*pointer2 == 6);
         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            REQUIRE(Allocator::CheckAuthority(pointer.GetType(), backup));
            REQUIRE_FALSE(Allocator::Find(pointer.GetType(), backup));
         #endif
         REQUIRE(pointer2.GetAllocation());
         REQUIRE(pointer.GetAllocation());
         REQUIRE(pointer.GetUses() == 2);
         if constexpr (CT::Referenced<TT>)
            REQUIRE(pointer->GetReferences() == 1); //TODO major design change - TRef is no longer deep referenced for now, let's see how that goes
      }

      auto raw = new Decay<TT> {3};
      const auto rawBackUp = raw;

      WHEN("Given an xvalue pointer created via `new` statement") {
         pointer = ::std::move(raw);

         REQUIRE(pointer == rawBackUp);
         REQUIRE(*pointer == *rawBackUp);
         REQUIRE(raw == rawBackUp);
         #if LANGULUS_FEATURE(NEWDELETE)
            REQUIRE(pointer.GetAllocation());
            REQUIRE(pointer.GetReferences() == 2);
         #else
            REQUIRE_FALSE(pointer.GetAllocation());
            if constexpr (CT::Referenced<TT>)
               REQUIRE(pointer->GetReferences() == 1);
         #endif
      }

      #if LANGULUS_FEATURE(NEWDELETE)
         WHEN("Given an immediate xvalue pointer created via `new` statement - a very bad practice, unless LANGULUS_FEATURE(NEWDELETE) is enabled!") {
            pointer = new Decay<TT> {3};

            #if LANGULUS_FEATURE(NEWDELETE)
               REQUIRE(pointer.GetAllocation());
               REQUIRE(pointer.GetReferences() == 2);
            #endif
         }

         WHEN("Given an xvalue pointer and then reset") {
            pointer = ::std::move(raw);
            auto unused = Allocator::Free(pointer.GetType(), raw, 1);
            pointer = nullptr;

            REQUIRE_FALSE(raw->GetAllocation());
            REQUIRE(Allocator::CheckAuthority(pointer.GetType(), raw));
            REQUIRE_FALSE(Allocator::Find(pointer.GetType(), raw));
            REQUIRE_FALSE(pointer.GetAllocation());
         }
      #endif

      WHEN("Given an lvalue pointer") {
         pointer = raw;

         REQUIRE(pointer == raw);
         REQUIRE(*pointer == *raw);
         #if LANGULUS_FEATURE(NEWDELETE)
            REQUIRE(pointer.GetAllocation());
            REQUIRE(pointer.GetReferences() == 2);
         #else
            REQUIRE_FALSE(pointer.GetAllocation());
            if constexpr (CT::Referenced<TT>)
               REQUIRE(pointer->GetReferences() == 1);
         #endif
      }

      #if not LANGULUS_FEATURE(NEWDELETE)
         if constexpr (CT::Referenced<Deptr<TT>>)
            raw->Reference(-1);
         delete raw;
      #endif
   }

   REQUIRE(memoryState.Assert());
   REQUIRE_FALSE(Allocator::CollectGarbage());
}
