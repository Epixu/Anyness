///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#include "../Main.hpp"
#include <Langulus/Anyness/TRef.hpp>
#include <Langulus/Anyness/SerializeText.hpp>
#include "../TestTypes/ReferencedType.hpp"
#include "../TestTypes/ScopedElement.hpp"

using namespace Langulus;
using Anyness::TRef;


TEST_CASE_TEMPLATE("Shared pointer", TestType
   , Types<TRef<RT>,        ScopedElement<RT, true>>
   
   , Types<TRef<RT>,        ScopedElement<RT>>
   , Types<TRef<const RT>,  ScopedElement<RT>>
   , Types<TRef<int>,       ScopedElement<int>>
   , Types<TRef<const int>, ScopedElement<int>>
   
   , Types<TRef<const RT>,  ScopedElement<RT, true>>
   , Types<TRef<int>,       ScopedElement<int, true>>
   , Types<TRef<const int>, ScopedElement<int, true>>
) {
   static MemoryState memoryState;
   using T  = typename TestType::First;
   using TT = TypeOf<T>;
   using ScopedE = typename TestType::Second;

   GIVEN("Nullptr-initialized") {
      T pointer {nullptr};
      T pointer2 {nullptr};

      REQUIRE_FALSE(pointer.GetRaw());
      REQUIRE_FALSE(pointer);
      REQUIRE(pointer == pointer2);
   }

   GIVEN("Default-initialized") {
      T pointer;
      T pointer2;
      const ScopedE raw {3};

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
            REQUIRE(pointer->GetReferences() == 1);
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

         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            auto backup = pointer.GetRaw();
         #endif

         pointer2.Emplace(6);
         pointer = pointer2;

         REQUIRE(pointer == pointer2);
         REQUIRE(*pointer == 6);
         REQUIRE(*pointer2 == 6);
         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            REQUIRE(Allocator::CheckAuthority(backup));
            REQUIRE_FALSE(Allocator::Find(backup));
         #endif
         REQUIRE(pointer2.GetAllocation());
         REQUIRE(pointer.GetAllocation());
         REQUIRE(pointer.GetUses() == 2);
         if constexpr (CT::Referenced<TT>)
            REQUIRE(pointer->GetReferences() == 1);
      }

      WHEN("Given an xvalue pointer created via `new` statement") {
         auto movable = raw.element;
         pointer = ::std::move(movable);

         REQUIRE(pointer == raw.element);
         REQUIRE(*pointer == *raw);
         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            REQUIRE(pointer.GetAllocation() == *raw.entries);
         #else
            REQUIRE(pointer.GetAllocation() == nullptr);
         #endif
         REQUIRE(pointer.GetUses() == (pointer.GetUses() ? 2 : 0));
         if constexpr (CT::Referenced<TT>)
            REQUIRE(pointer->GetReferences() == 1);
      }

      WHEN("Given an lvalue pointer") {
         pointer = raw.element;

         REQUIRE(pointer == raw.element);
         REQUIRE(*pointer == *raw);
         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            REQUIRE(pointer.GetAllocation() == *raw.entries);
         #else
            REQUIRE(pointer.GetAllocation() == nullptr);
         #endif
         REQUIRE(pointer.GetUses() == (pointer.GetUses() ? 2 : 0));
         if constexpr (CT::Referenced<TT>)
            REQUIRE(pointer->GetReferences() == 1);
      }

      WHEN("Compared") {
         static_assert(not static_cast<bool>(T{}));

         static_assert(T{} == T{});
         static_assert(T{} == nullptr);
         static_assert(nullptr == T{});
         static_assert(T{} == (TT*) {});
         static_assert((TT*) {} == T{});
         static_assert(T{ nullptr } == T{ nullptr });
         static_assert(T{ (TT*) {} } == T{ (TT*) {} });
         static_assert(T{ nullptr } == nullptr);
         static_assert(nullptr == T{ nullptr });
         static_assert(T{ (TT*) {} } == (TT*) {});
         static_assert((TT*) {} == T{ (TT*) {} });
      }
   }

   REQUIRE(memoryState.Assert());
   REQUIRE_FALSE(Allocator::CollectGarbage());
}
