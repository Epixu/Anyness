///                                                                           
/// Langulus::Fractalloc                                                      
/// Copyright (c) 2015 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "TestAllocatorCommon.hpp"
#include <Langulus/MetaOf.hpp>

#if not LANGULUS_FEATURE(MANAGED_MEMORY)
   #error "This file shouldn't be included if MANAGED_MEMORY is disabled"
#endif


SCENARIO("Testing FastLog2 calls", "[fractalloc]") {
   const size_t numbers[]{
      0, 1, 2, 3, 4, 5, 6, 11, 16, 64, 99, 120, 128
   };
   const size_t results[]{
      0, 0, 1, 1, 2, 2, 2,  3,  4,  6,  6,   6,   7
   };
   static_assert(sizeof(numbers) == sizeof(results), "Oops");

   for (unsigned i = 0; i < sizeof(numbers) / sizeof(size_t); ++i) {
      REQUIRE(Fractalloc::FastLog2(numbers[i]) == results[i]);
   }
}

TEMPLATE_TEST_CASE("Testing pool functions", "[fractalloc]",
   Type1,
   Type2,
   Type3,
   Type4,
   Type8,
   TypeBig,
   TypeVeryBig,
   TypeVeryBigAligned,
   TypeVeryBigPacked
) {
   using Fractalloc::Pool;
   const auto meta = MetaDataOf<TestType>();
   constexpr size_t default_size = CT::GetMinPool<TestType>();
   constexpr size_t min_alloc = CT::GetMinAlloc<TestType>();

   REQUIRE(meta);
   IF_SAFE(REQUIRE_THROWS(Allocator::AllocatePool(nullptr)));
   IF_SAFE(REQUIRE_THROWS(Allocator::AllocatePool(meta, 0)));
   IF_SAFE(REQUIRE_THROWS(Allocator::AllocatePool(meta, sizeof(TestType))));

   GIVEN("A default-sized pool") {
      Pool* pool = Allocator::AllocatePool(meta);
      REQUIRE(pool);

      const auto originPtr = pool->GetPoolStart();
      const auto smallest = pool->GetMinAllocation();
      const auto origin = reinterpret_cast<uintptr_t>(originPtr);
      const auto full = pool->GetAllocatedByBackend();
      const auto half = full / 2;
      const auto quarter = half / 2;

      REQUIRE(::std::has_single_bit(pool->GetAllocatedByBackend()));
      REQUIRE(::std::has_single_bit(pool->GetMinAllocation()));
      REQUIRE(::std::has_single_bit(pool->GetMaxEntries()));
      REQUIRE(IsAligned(pool->GetPoolStart()));
      REQUIRE(pool->GetAllocatedByBackend() == default_size);
      REQUIRE(reinterpret_cast<uintptr_t>(pool->AllocationFromIndex(0)) == origin);
      REQUIRE(reinterpret_cast<uintptr_t>(pool->AllocationFromIndex(1)) == origin + half);
      REQUIRE(reinterpret_cast<uintptr_t>(pool->AllocationFromIndex(2)) == origin + quarter);
      REQUIRE(reinterpret_cast<uintptr_t>(pool->AllocationFromIndex(3)) == origin + quarter + half);
      REQUIRE(pool->ThresholdFromIndex(1) == half);
      REQUIRE(pool->ThresholdFromIndex(2) == quarter);
      REQUIRE(pool->ThresholdFromIndex(3) == quarter);
      REQUIRE(pool->ThresholdFromIndex(4) == quarter / 2);
      REQUIRE(pool->ThresholdFromIndex(5) == quarter / 2);
      REQUIRE(pool->ThresholdFromIndex(6) == quarter / 2);
      REQUIRE(pool->ThresholdFromIndex(7) == quarter / 2);
      REQUIRE(pool->ThresholdFromIndex(8) == quarter / 4);
      REQUIRE(pool->ThresholdFromIndex(pool->GetMaxEntries() - 1) == smallest);
      REQUIRE(pool->ThresholdFromIndex(pool->GetMaxEntries()) == smallest / 2);
      REQUIRE(pool->CanContain(1));
      REQUIRE(pool->CanContain(Alignment));
      REQUIRE(pool->CanContain(smallest));
      REQUIRE(pool->CanContain(half));
      REQUIRE(pool->CanContain(full));
      REQUIRE_FALSE(pool->CanContain(full + 1));
      REQUIRE(pool->GetAllocatedByFrontend() == 0);
      REQUIRE(pool->GetMaxEntries() == full / smallest);
      REQUIRE(pool->Contains(originPtr));
      REQUIRE(pool->Contains(originPtr + half));
      REQUIRE(pool->Contains(originPtr + half * 2 - 1));
      REQUIRE_FALSE(pool->Contains(originPtr + half * 2));
      REQUIRE_FALSE(pool->Contains(nullptr));
      REQUIRE_FALSE(pool->IsInUse());

      WHEN("Small entry is allocated") {
         auto entry = pool->Allocate(sizeof(TestType));
         const auto full = pool->GetAllocatedByBackend();
         const auto smallest = pool->GetMinAllocation();

         REQUIRE(pool->GetAllocatedByFrontend() == entry->GetBackendSize());
         REQUIRE(pool->GetMaxEntries() == full / smallest);
         REQUIRE(pool->Contains(entry));
         REQUIRE(pool->IsInUse());
      }

      WHEN("Filled with all possible small entries") {
         // Fill up                                                     
         for (size_t i = 0; i < pool->GetMaxEntries(); ++i) {
            auto entry = pool->Allocate(sizeof(TestType));
            REQUIRE(entry);
            REQUIRE(entry->GetFrontendSize() == min_alloc);
            entry->Keep(i);

            // Fill the entire entry to check for heap corruptions      
            for (size_t i2 = 0; i2 < entry->GetFrontendSize(); ++i2) {
               entry->GetBlockStart()[i2] = 66;
            }
         }

         // Fail to add more                                            
         for (int i = 0; i < 5; ++i) {
            auto entry = pool->Allocate(1);
            REQUIRE(entry == nullptr);
         }

         const auto full = pool->GetAllocatedByBackend();
         const auto smallest = pool->GetMinAllocation();

         REQUIRE(pool->GetAllocatedByFrontend() == pool->GetAllocatedByBackend());
         REQUIRE(pool->GetAllocatedByFrontend() == pool->GetMaxEntries() * (Align(sizeof(Allocation), alignof(TestType)) + min_alloc));
         REQUIRE(pool->GetMaxEntries() == full / smallest);

         for (size_t i = 0; i < pool->GetMaxEntries(); ++i) {
            auto entry = pool->AllocationFromIndex(i);
            REQUIRE(pool->Contains(entry));
            REQUIRE(entry->GetUses() == 1 + i);

            for (size_t i2 = 0; i2 < entry->GetFrontendSize(); ++i2) {
               REQUIRE(entry->GetBlockStart()[i2] == 66);
            }
         }
      }

      WHEN("An entry larger than the minimum is allocated") {
         auto entry = pool->Allocate(min_alloc * 2);
         REQUIRE(entry);

         REQUIRE(pool->GetAllocatedByFrontend() == entry->GetBackendSize());
         REQUIRE(pool->GetMinAllocation() == Roof2(entry->GetBackendSize()));
         REQUIRE(pool->GetMaxEntries() == pool->GetAllocatedByBackend() / pool->GetMinAllocation());
         REQUIRE(pool->Contains(entry));
         REQUIRE(pool->IsInUse());
      }

      WHEN("An entry larger than the pool itself is allocated") {
         auto entry = pool->Allocate(default_size + 1);

         REQUIRE(entry == nullptr);
         REQUIRE(pool->GetAllocatedByFrontend() == 0);
         REQUIRE_FALSE(pool->IsInUse());
      }

      Allocator::DeallocatePool(pool);
   }
   
   GIVEN("A custom huge pool") {
      Pool* pool = Allocator::AllocatePool(meta, default_size * 1024);
      REQUIRE(pool);

      auto entry = pool->Allocate(5);
      const auto full = pool->GetAllocatedByBackend();
      const auto smallest = pool->GetMinAllocation();

      REQUIRE(pool->GetAllocatedByFrontend() == entry->GetBackendSize());
      REQUIRE(pool->GetMaxEntries() == full / smallest);
      REQUIRE(pool->Contains(entry));
      REQUIRE(pool->IsInUse());

      #ifdef LANGULUS_STD_BENCHMARK // Last result: 
         BENCHMARK_ADVANCED("Pool::Allocate(5)") (timer meter) {
            std::vector<Allocation*> storage(meter.runs());
            meter.measure([&](int i) {
               return storage[i] = pool->Allocate(5);
               });

            for (auto& i : storage) {
               if (i)
                  pool->Deallocate(i);
               else {
                  LANGULUS_THROW(Deallocate,
                     "The test is invalid, because the pool got full - use a bigger pool");
               }
            }
         };

         BENCHMARK_ADVANCED("std::malloc(5)") (timer meter) {
            std::vector<void*> storage(meter.runs());
            meter.measure([&](int i) {
               return storage[i] = ::std::malloc(5);
               });

            for (auto& i : storage) {
               if (i)
                  ::std::free(i);
               else {
                  LANGULUS_THROW(Deallocate,
                     "The test is invalid, because malloc returned a zero");
               }
            }
         };

         BENCHMARK_ADVANCED("Pool::Allocate(32)") (timer meter) {
            std::vector<Allocation*> storage(meter.runs());
            meter.measure([&](int i) {
               return storage[i] = pool->Allocate(32);
               });

            for (auto& i : storage) {
               if (i)
                  pool->Deallocate(i);
               else {
                  LANGULUS_THROW(Deallocate,
                     "The test is invalid, because the pool got full - use a bigger pool");
               }
            }
         };

         BENCHMARK_ADVANCED("std::malloc(32)") (timer meter) {
            std::vector<void*> storage(meter.runs());
            meter.measure([&](int i) {
               return storage[i] = ::std::malloc(32);
               });

            for (auto& i : storage) {
               if (i)
                  ::std::free(i);
               else {
                  LANGULUS_THROW(Deallocate,
                     "The test is invalid, because malloc returned a zero");
               }
            }
         };

         BENCHMARK_ADVANCED("Pool::Reallocate(32 -> 5)") (timer meter) {
            std::vector<Allocation*> storage(meter.runs());
            for (auto& i : storage) {
               i = pool->Allocate(32);
               if (!i) {
                  LANGULUS_THROW(Deallocate,
                     "The test is invalid, because the pool got full - use a bigger pool");
               }
            }

            meter.measure([&](int i) {
               const auto r = pool->Reallocate(storage[i], 5);
               if (r)
                  storage[i] = storage[i];
               return r;
               });

            for (auto& i : storage)
               pool->Deallocate(i);
         };

         BENCHMARK_ADVANCED("std::realloc(32 -> 5)") (timer meter) {
            std::vector<void*> storage(meter.runs());
            for (auto& i : storage) {
               i = ::std::malloc(32);
               if (!i) {
                  LANGULUS_THROW(Deallocate,
                     "The test is invalid, because malloc returned a zero");
               }
            }

            meter.measure([&](int i) {
               const auto r = ::std::realloc(storage[i], 5);
               if (r)
                  storage[i] = r;
               return r;
               });

            for (auto& i : storage)
               ::std::free(i);
         };

         BENCHMARK_ADVANCED("Pool::Reallocate(5 -> 32)") (timer meter) {
            std::vector<Allocation*> storage(meter.runs());
            for (auto& i : storage) {
               i = pool->Allocate(5);
               if (!i) {
                  LANGULUS_THROW(Deallocate,
                     "The test is invalid, because the pool got full - use a bigger pool");
               }
            }

            meter.measure([&](int i) {
               const auto r = pool->Reallocate(storage[i], 32);
               if (r)
                  storage[i] = storage[i];
               return r;
               });

            for (auto& i : storage)
               pool->Deallocate(i);
         };

         BENCHMARK_ADVANCED("std::realloc(5 -> 32)") (timer meter) {
            std::vector<void*> storage(meter.runs());
            for (auto& i : storage) {
               i = ::std::malloc(5);
               if (!i) {
                  LANGULUS_THROW(Deallocate,
                     "The test is invalid, because malloc returned a zero");
               }
            }

            meter.measure([&](int i) {
               const auto r = ::std::realloc(storage[i], 32);
               if (r)
                  storage[i] = r;
               return r;
               });

            for (auto& i : storage)
               ::std::free(i);
         };
      #endif

      Allocator::DeallocatePool(pool);
   }
}

TEMPLATE_TEST_CASE("Testing allocator functions", "[fractalloc]",
   Type1,
   Type2,
   Type3,
   Type4,
   Type8,
   TypeBig,
   TypeVeryBig,
   TypeVeryBigAligned,
   TypeVeryBigPacked
) {
   constexpr size_t data_offset = Align(sizeof(Allocation), alignof(TestType));

   GIVEN("An allocation") {
      Allocation* entry = Allocator::Allocate(nullptr, 512);
      REQUIRE(entry);

      WHEN("Memory is allocated on the heap") {
         entry = Allocator::Allocate(nullptr, 512);

         REQUIRE(entry->GetBlockStart() != nullptr);
         REQUIRE(entry->GetBlockStart() != reinterpret_cast<uint8_t*>(entry));
         REQUIRE(IsAligned(entry, alignof(Allocation)));
         REQUIRE(IsAligned(entry->GetBlockStart(), alignof(TestType)));
         REQUIRE(entry->GetBackendSize() == data_offset + 512);
         REQUIRE(entry->GetFrontendSize() == 512);
         REQUIRE(entry->GetBlockStart() == reinterpret_cast<uint8_t*>(entry) + data_offset);
         REQUIRE(entry->GetBlockEnd() == entry->GetBlockStart() + 512);
         REQUIRE(entry->GetUses() == 1);

         for (size_t i = 0; i < 512; ++i) {
            auto p1 = entry->GetBlockStart() + i;
            auto p2 = entry->GetBlockStart() - (i+1);
            REQUIRE(entry->Contains(p1));
            REQUIRE_FALSE(entry->Contains(p2));
         }

         for (size_t i = 512; i < 513; ++i) {
            auto p = entry->GetBlockStart() + i;
            REQUIRE_FALSE(entry->Contains(p));
         }

         Allocator::Deallocate(entry);

         #ifdef LANGULUS_STD_BENCHMARK // Last result: 
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
         entry->Keep();

         REQUIRE(entry->GetUses() == 2);
         REQUIRE(Allocator::CheckAuthority(nullptr, entry));
         REQUIRE(Allocator::Find(nullptr, entry->GetBlockStart()));
         REQUIRE_FALSE(Allocator::Find(nullptr, entry));

         IF_SAFE(REQUIRE_THROWS(Allocator::Deallocate(entry)));
         entry->Free();
         Allocator::Deallocate(entry);
      }

      WHEN("Referenced multiple times") {
         entry->Keep(5);

         REQUIRE(entry->GetUses() == 6);
         REQUIRE(Allocator::CheckAuthority(nullptr, entry));
         REQUIRE(Allocator::Find(nullptr, entry->GetBlockStart()));
         REQUIRE_FALSE(Allocator::Find(nullptr, entry));

         IF_SAFE(REQUIRE_THROWS(Allocator::Deallocate(entry)));
         entry->Free(5);
         Allocator::Deallocate(entry);
      }

      WHEN("Dereferenced once without deletion") {
         entry->Keep();
         entry->Free();

         REQUIRE(entry->GetUses() == 1);
         REQUIRE(Allocator::CheckAuthority(nullptr, entry));
         REQUIRE(Allocator::Find(nullptr, entry->GetBlockStart()));
         REQUIRE_FALSE(Allocator::Find(nullptr, entry));

         Allocator::Deallocate(entry);
      }

      WHEN("Dereferenced multiple times without deletion") {
         entry->Keep(5);
         entry->Free(4);

         REQUIRE(entry->GetUses() == 2);
         REQUIRE(Allocator::CheckAuthority(nullptr, entry));
         REQUIRE(Allocator::Find(nullptr, entry->GetBlockStart()));
         REQUIRE_FALSE(Allocator::Find(nullptr, entry));

         IF_SAFE(REQUIRE_THROWS(Allocator::Deallocate(entry)));
         entry->Free(1);
         Allocator::Deallocate(entry);
      }

      WHEN("Dereferenced once with deletion") {
         Allocator::Deallocate(entry);

         REQUIRE(Allocator::CheckAuthority(nullptr, entry));
         REQUIRE_FALSE(Allocator::Find(nullptr, entry->GetBlockStart()));
         REQUIRE_FALSE(Allocator::Find(nullptr, entry));
      }

      WHEN("Dereferenced multiple times with deletion") {
         entry->Keep(5);

         IF_SAFE(REQUIRE_THROWS(Allocator::Deallocate(entry)));
         entry->Free(5);
         Allocator::Deallocate(entry);

         REQUIRE(Allocator::CheckAuthority(nullptr, entry));
         REQUIRE_FALSE(Allocator::Find(nullptr, entry->GetBlockStart()));
         REQUIRE_FALSE(Allocator::Find(nullptr, entry));
      }
   }
}
