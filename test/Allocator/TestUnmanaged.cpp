///                                                                           
/// Langulus::Fractalloc                                                      
/// Copyright (c) 2015 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "../Main.hpp"
#include <Langulus/Allocator.hpp>
#include <random>

#if LANGULUS_FEATURE(MANAGED_MEMORY)
   #error "This file shouldn't be included if MANAGED_MEMORY is enabled"
#endif

using namespace Langulus;
using namespace Anyness;

static_assert(alignof(Allocation) % Alignment == 0);

std::random_device rd;
std::mt19937 gen(rd());

using Type1 = uint8_t;
using Type2 = uint16_t;

#pragma pack(push, 1)
struct Type3 {
   uint16_t m1;
   uint8_t  m2;
};
#pragma pack(pop)

using Type4 = uint32_t;
using Type8 = uint64_t;

struct TypeBig {
   Type1 t1;
   Type2 t2;
   Type4 t4;
   Type8 t8;
};

struct TypeVeryBig {
   TypeBig t1;
   TypeBig t2;
   TypeBig t4;
   TypeBig t8[5];
};

bool IsAligned(const void* a) noexcept {
   return 0 == (reinterpret_cast<uintptr_t>(a) & uintptr_t { Alignment - 1 });
}

TEMPLATE_TEST_CASE("Testing allocator functions", "[allocator]",
   Type1,
   Type2,
   Type3,
   Type4,
   Type8,
   TypeBig,
   TypeVeryBig
) {
   static Allocator::State memoryState;

   GIVEN("An allocation") {
      Allocation* entry = nullptr;

      WHEN("Memory is allocated on the heap") {
         entry = Allocator::Allocate(alignof(TestType), 512);

         REQUIRE(entry);
         REQUIRE(entry->GetBlockStart() != nullptr);
         REQUIRE(entry->GetBlockStart() != reinterpret_cast<uint8_t*>(entry));
         REQUIRE(reinterpret_cast<uintptr_t>(entry) % Alignment == 0);
         REQUIRE(reinterpret_cast<uintptr_t>(entry->GetBlockStart()) % Alignment == 0);
         REQUIRE(entry->GetFrontendSize() >= 512);
         REQUIRE(entry->GetBackendSize() >= 512 + sizeof(Allocation));
         REQUIRE(entry->GetBlockEnd() == entry->GetBlockStart() + entry->GetFrontendSize());
         REQUIRE(entry->GetBlockStart() == reinterpret_cast<uint8_t*>(entry) + Align(sizeof(Allocation), alignof(TestType)));
         REQUIRE(entry->GetUses() == 1);

         for (size_t i = 0; i < 512; ++i) {
            auto p = entry->GetBlockStart() + i;
            REQUIRE(entry->Contains(p));
         }

         for (size_t i = 512; i < 513; ++i) {
            auto p = entry->GetBlockStart() + i;
            REQUIRE_FALSE(entry->Contains(p));
         }

         Allocator::Deallocate(entry);

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("Allocator::Allocate(5)") (timer meter) {
               std::vector<Allocation*> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = Allocator::Allocate(5);
                  });

               for (auto& i : storage) {
                  if (i)
                     Allocator::Deallocate(i);
                  else
                     LANGULUS_THROW(Deallocate, "The test is invalid, because memory got full");
               }
            };

            BENCHMARK_ADVANCED("malloc(5)") (timer meter) {
               std::vector<void*> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = ::std::malloc(5);
                  });

               for (auto& i : storage) {
                  if (i)
                     ::std::free(i);
                  else
                     LANGULUS_THROW(Deallocate, "The test is invalid, because memory got full");
               }
            };

            BENCHMARK_ADVANCED("Allocator::Allocate(512)") (timer meter) {
               std::vector<Allocation*> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = Allocator::Allocate(512);
                  });

               for (auto& i : storage) {
                  if (i)
                     Allocator::Deallocate(i);
                  else
                     LANGULUS_THROW(Deallocate, "The test is invalid, because memory got full");
               }
            };

            BENCHMARK_ADVANCED("malloc(512)") (timer meter) {
               std::vector<void*> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = ::std::malloc(512);
                  });

               for (auto& i : storage) {
                  if (i)
                     ::std::free(i);
                  else
                     LANGULUS_THROW(Deallocate, "The test is invalid, because memory got full");
               }
            };

            BENCHMARK_ADVANCED("Allocator::Allocate(Pool::DefaultPoolSize)") (timer meter) {
               std::vector<Allocation*> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = Allocator::Allocate(1024 * 1024);
                  });

               for (auto& i : storage) {
                  if (i)
                     Allocator::Deallocate(i);
                  else
                     LANGULUS_THROW(Deallocate, "The test is invalid, because memory got full");
               }
            };

            BENCHMARK_ADVANCED("malloc(Pool::DefaultPoolSize)") (timer meter) {
               std::vector<void*> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = ::std::malloc(1024 * 1024);
                  });

               for (auto& i : storage) {
                  if (i)
                     ::std::free(i);
                  else
                     LANGULUS_THROW(Deallocate, "The test is invalid, because memory got full");
               }
            };
         #endif
      }

      WHEN("Referenced once") {
         entry = Allocator::Allocate(alignof(TestType), 512);
         REQUIRE(entry);
         entry->Keep();

         REQUIRE(entry->GetUses() == 2);

         IF_SAFE(REQUIRE_THROWS(Allocator::Deallocate(entry)));
         entry->Free();
         Allocator::Deallocate(entry);
      }

      WHEN("Referenced multiple times") {
         entry = Allocator::Allocate(alignof(TestType), 512);
         REQUIRE(entry);
         entry->Keep(5);

         REQUIRE(entry->GetUses() == 6);

         IF_SAFE(REQUIRE_THROWS(Allocator::Deallocate(entry)));
         entry->Free(5);
         Allocator::Deallocate(entry);
      }

      WHEN("Dereferenced once without deletion") {
         entry = Allocator::Allocate(alignof(TestType), 512);
         REQUIRE(entry);
         entry->Keep();
         entry->Free();

         REQUIRE(entry->GetUses() == 1);

         Allocator::Deallocate(entry);
      }

      WHEN("Dereferenced multiple times without deletion") {
         entry = Allocator::Allocate(alignof(TestType), 512);
         REQUIRE(entry);
         entry->Keep(5);
         entry->Free(4);

         REQUIRE(entry->GetUses() == 2);

         IF_SAFE(REQUIRE_THROWS(Allocator::Deallocate(entry)));
         entry->Free(1);
         Allocator::Deallocate(entry);
      }

      WHEN("Dereferenced once with deletion") {
         entry = Allocator::Allocate(alignof(TestType), 512);
         REQUIRE(entry);
         Allocator::Deallocate(entry);

      }

      WHEN("Dereferenced multiple times with deletion") {
         entry = Allocator::Allocate(alignof(TestType), 512);
         REQUIRE(entry);
         entry->Keep(5);

         IF_SAFE(REQUIRE_THROWS(Allocator::Deallocate(entry)));
         entry->Free(5);
         Allocator::Deallocate(entry);
      }
   }

   REQUIRE(memoryState.Assert());
   REQUIRE_FALSE(Allocator::CollectGarbage());
}
