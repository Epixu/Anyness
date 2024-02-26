///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// Distributed under GNU General Public License v3+                          
/// See LICENSE file, or https://www.gnu.org/licenses                         
///                                                                           
#include <Anyness/Text.hpp>
#include <any>
#include <vector>
#include "Common.hpp"


SCENARIO("Deep sequential containers", "[any]") {
   static Allocator::State memoryState;

   static_assert(sizeof(A::Block) == sizeof(Block));

   GIVEN("Any with some deep items") {
      IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());

      Any pack;
      Any subpack1;
      Any subpack2;
      Any subpack3;
      subpack1 << int(1) << int(2) << int(3) << int(4) << int(5);
      subpack2 << int(6) << int(7) << int(8) << int(9) << int(10);
      subpack3 << subpack1 << subpack2;
      pack << subpack1 << subpack2 << subpack3;
      pack.MakeTypeConstrained();

      auto memory = pack.GetRaw<Any>();
      //auto& submemory4 = subpack3.As<Any>(0);
      //auto& submemory5 = subpack3.As<Any>(1);

      REQUIRE(pack.GetCount() == 3);
      REQUIRE(pack.GetReserved() >= 3);
      REQUIRE(pack.Is<Any>());
      REQUIRE(pack.GetRaw<Any>());

      WHEN("Getting deep elements") {
         REQUIRE(pack.GetCountDeep() == 6);
         REQUIRE(pack.GetCountElementsDeep() == 20);
         REQUIRE(pack.GetBlockDeep(0));
         REQUIRE(pack.GetBlockDeep(1));
         REQUIRE(pack.GetBlockDeep(2));
         REQUIRE(pack.GetBlockDeep(3));
         REQUIRE(pack.GetBlockDeep(4));
         REQUIRE(pack.GetBlockDeep(5));
         REQUIRE(pack.GetBlockDeep(666) == nullptr);
         REQUIRE(*pack.GetBlockDeep(0) == pack);
         REQUIRE(*pack.GetBlockDeep(1) == subpack1);
         REQUIRE(*pack.GetBlockDeep(2) == subpack2);
         REQUIRE(*pack.GetBlockDeep(3) == subpack3);
         REQUIRE(*pack.GetBlockDeep(4) == subpack1);
         REQUIRE(*pack.GetBlockDeep(5) == subpack2);
         for (int i = 0; i < 10; ++i) {
            REQUIRE(pack.GetElementDeep(i) == i + 1);
            REQUIRE(pack.GetElementDeep(i + 10) == i + 1);
         }
         REQUIRE(pack.GetElementDeep(666).IsEmpty());
      }

      WHEN("Push more stuff") {
         REQUIRE_THROWS_AS(pack << int(6), Except::Mutate);

         REQUIRE(pack.GetCount() == 3);
         REQUIRE(pack.GetReserved() >= 3);
         REQUIRE(pack.Is<Any>());
         REQUIRE(pack.GetRaw<Any>());
      }

      WHEN("Element 0 is removed") {
         const auto refsBefore = pack.GetUses();
         pack.RemoveIndex(0);

         REQUIRE(pack.GetCount() == 2);
         REQUIRE(pack.As<Any>(0) == subpack2);
         REQUIRE(pack.As<Any>(1) == subpack3);
         REQUIRE(pack.GetReserved() >= 3);
         REQUIRE(pack.Is<Any>());
         REQUIRE(pack.GetRaw<Any>() == memory);
         REQUIRE(pack.GetUses() == refsBefore);
         REQUIRE(subpack1.GetUses() == 2);
         REQUIRE(subpack2.GetUses() == 3);
         REQUIRE(subpack3.GetUses() == 2);
      }

      WHEN("Element 1 is removed") {
         const auto refsBefore = pack.GetUses();
         pack.RemoveIndex(1);

         REQUIRE(pack.GetCount() == 2);
         REQUIRE(pack.As<Any>(0) == subpack1);
         REQUIRE(pack.As<Any>(1) == subpack3);
         REQUIRE(pack.GetReserved() >= 3);
         REQUIRE(pack.Is<Any>());
         REQUIRE(pack.GetRaw<Any>() == memory);
         REQUIRE(pack.GetUses() == refsBefore);
         REQUIRE(subpack1.GetUses() == 3);
         REQUIRE(subpack2.GetUses() == 2);
         REQUIRE(subpack3.GetUses() == 2);
      }

      WHEN("Element 2 is removed") {
         const auto refsBefore = pack.GetUses();
         pack.RemoveIndex(2);

         REQUIRE(pack.GetCount() == 2);
         REQUIRE(pack.As<Any>(0) == subpack1);
         REQUIRE(pack.As<Any>(1) == subpack2);
         REQUIRE(pack.GetReserved() >= 3);
         REQUIRE(pack.Is<Any>());
         REQUIRE(pack.GetRaw<Any>() == memory);
         REQUIRE(pack.GetUses() == refsBefore);
         REQUIRE(subpack1.GetUses() == 3);
         REQUIRE(subpack2.GetUses() == 3);
         REQUIRE(subpack3.GetUses() == 1);
      }

      WHEN("All element are removed one by one") {
         pack.RemoveIndex(0);
         pack.RemoveIndex(0);
         pack.RemoveIndex(0);

         REQUIRE(!pack);
         REQUIRE(pack.GetReserved() > 0);
         REQUIRE(pack.Is<Any>());
         REQUIRE(pack.IsTypeConstrained());
         REQUIRE(pack.GetRaw<Any>() != nullptr);
         REQUIRE(pack.GetUses() > 0);
         REQUIRE(subpack1.GetUses() == 2);
         REQUIRE(subpack2.GetUses() == 2);
         REQUIRE(subpack3.GetUses() == 1);
      }

      WHEN("The size is reduced, by finding and removing") {
         pack.RemoveIndex(pack.Find(subpack1));

         REQUIRE(pack.GetCount() == 2);
         REQUIRE(pack.As<Any>(0) == subpack2);
         REQUIRE(pack.As<Any>(1) == subpack3);
         REQUIRE(pack.GetReserved() >= 3);
         REQUIRE(pack.Is<Any>());
         REQUIRE(pack.GetRaw<Any>() != nullptr);
      }

      WHEN("Pack is cleared") {
         pack.Clear();

         REQUIRE(pack.GetCount() == 0);
         REQUIRE(pack.GetReserved() >= 3);
         REQUIRE(pack.GetRaw<Any>() == memory);
         REQUIRE(pack.Is<Any>());
      }

      WHEN("Pack is reset") {
         pack.Reset();

         REQUIRE(pack.GetCount() == 0);
         REQUIRE(pack.GetReserved() == 0);
         REQUIRE(pack.GetRaw<Any>() == nullptr);
         REQUIRE(pack.Is<Any>());
         REQUIRE(pack.IsTypeConstrained());
      }

      WHEN("Pack is shallow-copied") {
         pack.As<Any>(2).As<Any>(1).MakeOr();
         pack.As<Any>(0).MakeOr();

         auto copy = pack;

         REQUIRE(copy.GetRaw<Any>() == pack.GetRaw<Any>());
         REQUIRE(copy.GetCount() == pack.GetCount());
         REQUIRE(copy.GetReserved() == pack.GetReserved());
         REQUIRE(copy.GetState() == pack.GetState());
         REQUIRE(copy.GetType() == pack.GetType());
         REQUIRE(copy.GetUses() == 2);
         REQUIRE(copy.As<Any>(0).GetRaw<Any>() == subpack1.GetRaw<Any>());
         REQUIRE(copy.As<Any>(0).IsOr());
         REQUIRE(copy.As<Any>(0).GetCount() == subpack1.GetCount());
         REQUIRE(copy.As<Any>(0).GetUses() == 3);
         REQUIRE(copy.As<Any>(1).GetRaw<Any>() == subpack2.GetRaw<Any>());
         REQUIRE(copy.As<Any>(1).GetState() == DataState::Default);
         REQUIRE(copy.As<Any>(1).GetCount() == subpack2.GetCount());
         REQUIRE(copy.As<Any>(1).GetUses() == 3);
         REQUIRE(copy.As<Any>(2).GetRaw<Any>() == subpack3.GetRaw<Any>());
         REQUIRE(copy.As<Any>(2).GetState() == DataState::Default);
         REQUIRE(copy.As<Any>(2).GetCount() == subpack3.GetCount());
         REQUIRE(copy.As<Any>(2).GetUses() == 2);
         REQUIRE(copy.As<Any>(2).As<Any>(0).GetRaw<Any>() == subpack1.GetRaw<Any>());
         REQUIRE(copy.As<Any>(2).As<Any>(0).GetState() == DataState::Default);
         REQUIRE(copy.As<Any>(2).As<Any>(0).GetCount() == subpack1.GetCount());
         REQUIRE(copy.As<Any>(2).As<Any>(1).GetRaw<Any>() == subpack2.GetRaw<Any>());
         REQUIRE(copy.As<Any>(2).As<Any>(1).IsOr());
         REQUIRE(copy.As<Any>(2).As<Any>(1).GetCount() == subpack2.GetCount());
      }

      WHEN("Pack is cloned") {
         pack.As<Any>(2).As<Any>(1).MakeOr();
         pack.As<Any>(0).MakeOr();

         Any clone = Clone(pack);

         REQUIRE(clone.GetRaw<Any>() != pack.GetRaw<Any>());
         REQUIRE(clone.GetCount() == pack.GetCount());
         REQUIRE(clone.GetReserved() >= clone.GetCount());
         REQUIRE(clone.GetState() == pack.GetState());
         REQUIRE(clone.GetType() == pack.GetType());
         REQUIRE(clone.GetUses() == 1);
         REQUIRE(pack.GetUses() == 1);
         REQUIRE(clone.As<Any>(0).GetRaw<Any>() != subpack1.GetRaw<Any>());
         REQUIRE(clone.As<Any>(0).IsOr());
         REQUIRE(clone.As<Any>(0).GetCount() == subpack1.GetCount());
         REQUIRE(clone.As<Any>(0).GetUses() == 1);
         REQUIRE(pack.As<Any>(0).GetUses() == 3);
         REQUIRE(clone.As<Any>(1).GetRaw<Any>() != subpack2.GetRaw<Any>());
         REQUIRE(clone.As<Any>(1).GetState() == DataState::Default);
         REQUIRE(clone.As<Any>(1).GetCount() == subpack2.GetCount());
         REQUIRE(clone.As<Any>(1).GetUses() == 1);
         REQUIRE(pack.As<Any>(1).GetUses() == 3);
         REQUIRE(clone.As<Any>(2).GetRaw<Any>() != subpack3.GetRaw<Any>());
         REQUIRE(clone.As<Any>(2).GetState() == DataState::Default);
         REQUIRE(clone.As<Any>(2).GetCount() == subpack3.GetCount());
         REQUIRE(clone.As<Any>(2).GetUses() == 1);
         REQUIRE(pack.As<Any>(2).GetUses() == 2);
         REQUIRE(clone.As<Any>(2).As<Any>(0).GetRaw<Any>() != subpack1.GetRaw<Any>());
         REQUIRE(clone.As<Any>(2).As<Any>(0).GetState() == DataState::Default);
         REQUIRE(clone.As<Any>(2).As<Any>(0).GetCount() == subpack1.GetCount());
         REQUIRE(clone.As<Any>(2).As<Any>(0).GetUses() == 1);
         REQUIRE(pack.As<Any>(2).As<Any>(0).GetUses() == 3);
         REQUIRE(clone.As<Any>(2).As<Any>(1).GetRaw<Any>() != subpack2.GetRaw<Any>());
         REQUIRE(clone.As<Any>(2).As<Any>(1).IsOr());
         REQUIRE(clone.As<Any>(2).As<Any>(1).GetCount() == subpack2.GetCount());
         REQUIRE(clone.As<Any>(2).As<Any>(1).GetUses() == 1);
         REQUIRE(pack.As<Any>(2).As<Any>(1).GetUses() == 3);
      }

      WHEN("Smart pushing different type without retainment") {
         auto result = subpack1.SmartPush<true, void>(IndexBack, '?');

         REQUIRE(result == 0);
         REQUIRE(subpack1.GetCount() == 5);
      }

      WHEN("Smart pushing with retainment") {
         Any deepened;
         deepened << int(1) << int(2) << int(3) << int(4) << int(5);
         auto result = deepened.SmartPush<false>(IndexBack, '?');

         REQUIRE(result == 1);
         REQUIRE(deepened.IsDeep());
         REQUIRE(deepened.GetCount() == 2);
         REQUIRE(deepened.As<Any>(0).GetCount() == 5);
         REQUIRE(deepened.As<Any>(1).GetCount() == 1);
      }

      WHEN("Smart pushing an empty container (but not stateless) with retainment") {
         Any deepened;
         deepened << int(1) << int(2) << int(3) << int(4) << int(5);
         auto pushed = Any::FromMeta(nullptr, DataState::Missing);
         auto result = deepened.SmartPush(IndexBack, pushed);

         REQUIRE(result == 1);
         REQUIRE(deepened.IsDeep());
         REQUIRE(deepened.GetCount() == 2);
         REQUIRE(deepened.As<Any>(0).GetCount() == 5);
         REQUIRE(deepened.As<Any>(1).GetCount() == 0);
         REQUIRE(deepened.As<Any>(1).GetState() == DataState::Missing);
      }

      WHEN("Smart pushing an empty container (but not stateless) with retainment to another empty container") {
         auto pushed = Any::FromMeta(nullptr, DataState::Missing);
         auto pushed2 = Any::FromMeta(nullptr, DataState {});
         auto result = pushed2.SmartPush(IndexBack, pushed);

         REQUIRE(result == 1);
         REQUIRE(pushed2.GetCount() == 0);
         REQUIRE(pushed2.GetState() == DataState::Missing);
      }

      WHEN("Smart pushing to an empty container (concat & retain enabled)") {
         Any pushed;
         auto result = pushed.SmartPush(IndexBack, pack);

         REQUIRE(pushed == pack);
         REQUIRE(result == 1);
      }

      WHEN("Smart pushing to a different container with retain enabled") {
         Any pushed;
         pushed << 666;
         pushed.MakeOr();
         auto result = pushed.SmartPush(IndexBack, '?');

         REQUIRE(result == 1);
         REQUIRE(pushed.IsOr());
         REQUIRE(!pushed.As<Any>(0).IsOr());
         REQUIRE(!pushed.As<Any>(1).IsOr());
      }

      WHEN("ForEachDeep with dense flat element (immutable, skipping)") {
         int it = 1;
         Count total = 0;
         const auto iterated = pack.ForEachDeep(
            [&](const int& i) {
               REQUIRE(i == it);
               ++total;
               if (++it == 11)
                  it = 1;
            }
         );

         REQUIRE(it == 1);
         REQUIRE(total == 20);
         REQUIRE(total == iterated);
      }

      WHEN("ForEachDeep with dense flat element (mutable, skipping)") {
         int it = 1;
         Count total = 0;
         const auto iterated = pack.ForEachDeep(
            [&](int& i) {
               REQUIRE(i == it);
               ++total;
               if (++it == 11)
                  it = 1;
            }
         );

         REQUIRE(it == 1);
         REQUIRE(total == 20);
         REQUIRE(total == iterated);
      }

      WHEN("ForEachDeep with dense flat element (immutable, non-skipping)") {
         int it = 1;
         Count total = 0;
         const auto iterated = pack.template ForEachDeep<false, false>(
            [&](const int& i) {
               REQUIRE(i == it);
               ++total;
               if (++it == 11)
                  it = 1;
            }
         );

         REQUIRE(it == 1);
         REQUIRE(total == 20);
         REQUIRE(total == iterated);
      }

      WHEN("ForEachDeep with dense flat element (mutable, non-skipping)") {
         int it = 1;
         Count total = 0;
         const auto iterated = pack.template ForEachDeep<false, false>(
            [&](int& i) {
               REQUIRE(i == it);
               ++total;
               if (++it == 11)
                  it = 1;
            }
         );

         REQUIRE(it == 1);
         REQUIRE(total == 20);
         REQUIRE(total == iterated);
      }

      WHEN("ForEachDeep with dense Block element (immutable, skipping)") {
         Count total = 0;
         const auto iterated = pack.ForEachDeep(
            [&](const Block& i) {
               (void)i;
               ++total;
            }
         );

         REQUIRE(total == 4);
         REQUIRE(total == iterated);
      }

      WHEN("ForEachDeep with dense Block element (mutable, skipping)") {
         Count total = 0;
         const auto iterated = pack.ForEachDeep(
            [&](Block& i) {
               (void)i;
               ++total;
            }
         );

         REQUIRE(total == 4);
         REQUIRE(total == iterated);
      }

      WHEN("ForEachDeep with dense Block element (immutable, non-skipping)") {
         Count total = 0;
         const auto iterated = pack.template ForEachDeep<false, false>(
            [&](const Block& i) {
               (void)i;
               ++total;
            }
         );

         REQUIRE(total == 6);
         REQUIRE(total == iterated);
      }

      WHEN("ForEachDeep with dense Block element (mutable, non-skipping)") {
         Count total = 0;
         const auto iterated = pack.template ForEachDeep<false, false>(
            [&](Block& i) {
               (void)i;
               ++total;
            }
         );

         REQUIRE(total == 6);
         REQUIRE(total == iterated);
      }
   }

   GIVEN("Any with some deep items for the purpose of optimization") {
      IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());

      Any pack;
      Any subpack1;
      Any subpack2;
      Any subpack3;
      subpack1 << int(1) << int(2) << int(3) << int(4) << int(5);
      subpack2 << int(6) << int(7) << int(8) << int(9) << int(10);
      subpack3 << subpack1;
      subpack3.MakeOr();
      pack << subpack1 << subpack2 << subpack3;

      WHEN("The container is optimized") {
         pack.Optimize();

         REQUIRE(pack.GetCount() == 3);
         REQUIRE(pack.As<Any>(0) == subpack1);
         REQUIRE(pack.As<Any>(1) == subpack2);
         REQUIRE(pack.As<Any>(2) == subpack1);
         REQUIRE(pack.GetUses() == 1);
         REQUIRE(subpack1.GetUses() == 3);
         REQUIRE(subpack2.GetUses() == 2);
         REQUIRE(subpack3.GetUses() == 1);
      }
   }

   GIVEN("Any with some deep items, and their Blocks coalesced") {
      IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());

      Any pack;
      Any subpack1;
      Any subpack2;
      Any subpack3;
      subpack1 << int(1) << int(2) << int(3) << int(4) << int(5);
      subpack2 << int(6) << int(7) << int(8) << int(9) << int(10);
      subpack3 << subpack1;
      subpack3.MakeOr();
      pack << subpack1 << subpack2 << subpack3;

      auto baseRange = Any::From<Block>();
      baseRange.Reserve(3);

      for (Count e = 0; e < pack.GetCount(); ++e) {
         auto element = pack.GetElement(e);
         RTTI::Base base;
         REQUIRE(element.GetType()->GetBase<Block>(0, base));
         auto baseBlock = element.GetBaseMemory(MetaOf<Block>(), base);
         baseRange.InsertBlock(IndexBack, baseBlock);
      }

      WHEN("The Block bases from the subpacks are coalesced in a single container") {
         REQUIRE(pack.GetUses() == 1);
         REQUIRE(subpack1.GetUses() == 3); //4 if that functionality is added
         REQUIRE(subpack2.GetUses() == 2); //3 if that functionality is added
         REQUIRE(subpack3.GetUses() == 2); //3 if that functionality is added
      }

      WHEN("The coalesced Block bases are freed") {
         baseRange.Reset();

         REQUIRE(pack.GetUses() == 1);
         REQUIRE(subpack1.GetUses() == 3);
         REQUIRE(subpack2.GetUses() == 2);
         REQUIRE(subpack3.GetUses() == 2);
      }

      WHEN("The master pack is freed") {
         pack.Reset();

         REQUIRE(pack.GetUses() == 0);
         REQUIRE(subpack1.GetUses() == 2); // 3 if that functionality is added
         REQUIRE(subpack2.GetUses() == 1); // 2 if that functionality is added
         REQUIRE(subpack3.GetUses() == 1); // 2 if that functionality is added
      }
   }

   REQUIRE(memoryState.Assert());
}
