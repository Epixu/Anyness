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
   TypeVeryBigAligned,
   TypeVeryBigPacked,
   Type1,
   Type2,
   Type3,
   Type4,
   Type8,
   TypeBig,
   TypeVeryBig
) {
   static Allocator::State memoryState;
   
   using Fractalloc::Pool;
   const auto meta = MetaDataOf<TestType>();
   constexpr size_t default_size = CT::GetMinPool<TestType>();
   constexpr size_t min_alloc = CT::GetMinAlloc<TestType>();
   constexpr size_t testAlignment = alignof(TestType);

   REQUIRE(meta);
   //IF_SAFE(REQUIRE_THROWS(Allocator::AllocatePool(nullptr)));
   IF_SAFE(REQUIRE_THROWS(Allocator::AllocatePool(meta, 0_pot)));
   IF_SAFE(REQUIRE_THROWS(Allocator::AllocatePool(meta, 3_pot)));

   GIVEN("A default-sized pool") {
      Pool* pool = Allocator::AllocatePool(meta, pot_t(default_size));
      REQUIRE(pool);

      const auto allocData = pool->GetAllocationData();
      const auto origin = pool->GetClientData();
      const auto smallest = static_cast<size_t>(pool->GetMinAllocation());
      const auto full = static_cast<size_t>(pool->GetAllocatedByBackend());
      const auto half = full / 2;
      const auto quarter = half / 2;

      REQUIRE(smallest == min_alloc);
      REQUIRE(IsAligned(pool->GetClientData(), testAlignment));
      REQUIRE(pool->GetAllocatedByBackend() == default_size);
      REQUIRE(pool->AllocationFromIndex(0) == allocData);
      REQUIRE(pool->AllocationFromIndex(1) == allocData + half/smallest);
      REQUIRE(pool->AllocationFromIndex(2) == allocData + quarter/smallest);
      REQUIRE(pool->AllocationFromIndex(3) == allocData + (quarter + half)/smallest);
      REQUIRE(pool->ThresholdFromIndex(0) == full);
      REQUIRE(pool->ThresholdFromIndex(1) == half);
      REQUIRE(pool->ThresholdFromIndex(2) == quarter);
      REQUIRE(pool->ThresholdFromIndex(3) == quarter);
      REQUIRE(pool->ThresholdFromIndex(4) == quarter / 2);
      REQUIRE(pool->ThresholdFromIndex(5) == quarter / 2);
      REQUIRE(pool->ThresholdFromIndex(6) == quarter / 2);
      REQUIRE(pool->ThresholdFromIndex(7) == quarter / 2);
      REQUIRE(pool->ThresholdFromIndex(8) == quarter / 4);
      REQUIRE(pool->ThresholdFromIndex(pool->GetMaxEntries() - 1u) == smallest);
      REQUIRE(pool->ThresholdFromIndex(pool->GetMaxEntries() - 0u) == smallest / 2);
      REQUIRE(pool->CanContain(1_pot));
      REQUIRE(pool->CanContain(pot_t(Alignment)));
      REQUIRE(pool->CanContain(pot_t(smallest)));
      REQUIRE(pool->CanContain(pot_t(half)));
      REQUIRE(pool->CanContain(pot_t(full)));
      REQUIRE(pool->GetAllocatedByFrontend() == 0);
      REQUIRE(pool->GetMaxEntries() == full / smallest);
      REQUIRE(pool->ContainsData(origin));
      REQUIRE(pool->ContainsData(origin + half));
      REQUIRE(pool->ContainsData(origin + half * 2 - 1));
      REQUIRE_FALSE(pool->ContainsData(origin + half * 2));
      REQUIRE_FALSE(pool->ContainsData(nullptr));
      REQUIRE_FALSE(pool->IsInUse());

      WHEN("Small entry is allocated") {
         auto entry = pool->Allocate(pot_t(Roof2(sizeof(TestType))));

         REQUIRE(pool->GetAllocatedByFrontend() == entry->GetSize());
         REQUIRE(pool->GetMaxEntries() == full / smallest);
         REQUIRE(pool->ContainsAllocation(entry));
         REQUIRE(pool->ContainsData(entry->GetBlockStart()));
         REQUIRE(pool->IsInUse());
      }

      WHEN("Filled with all possible small entries") {
         // Fill up                                                     
         Logger::Special("Filling up pool...");
         for (size_t i = 0; i < pool->GetMaxEntries(); ++i) {
            auto entry = pool->Allocate(pot_t(Roof2(sizeof(TestType))));
            REQUIRE(entry);
            REQUIRE(entry->GetSize() == min_alloc);
            REQUIRE(IsAligned(entry->GetBlockStart(), testAlignment));

            entry->Keep(i);

            // Fill the entire entry to check for heap corruptions      
            for (size_t i2 = 0; i2 < entry->GetSize(); ++i2) {
               entry->GetBlockStart()[i2] = 66;
            }
         }

         // Fail to add more                                            
         Logger::Special("Overflowing pool...");
         for (int i = 0; i < 5; ++i) {
            auto entry = pool->Allocate(1_pot);
            REQUIRE(entry == nullptr);
         }

         REQUIRE(pool->GetAllocatedByFrontend() == pool->GetAllocatedByBackend());
         REQUIRE(pool->GetAllocatedByFrontend() == static_cast<size_t>(pool->GetMaxEntries()) * min_alloc);
         REQUIRE(pool->GetMaxEntries() == full / smallest);
         REQUIRE(pool->AllocationFromIndex(0)->GetBlockStart() == origin);
         REQUIRE(pool->AllocationFromIndex(1)->GetBlockStart() == origin + half);
         REQUIRE(pool->AllocationFromIndex(2)->GetBlockStart() == origin + quarter);
         REQUIRE(pool->AllocationFromIndex(3)->GetBlockStart() == origin + quarter + half);
         REQUIRE(pool->AllocationFromIndex(static_cast<size_t>(pool->GetMaxEntries()) - 1)->GetBlockStart() == origin + half + half - min_alloc);

         Logger::Special("Checking integrity of pool...");
         for (size_t i = 0; i < pool->GetMaxEntries(); ++i) {
            auto entry = pool->AllocationFromIndex(i);
            REQUIRE(pool->ContainsAllocation(entry));
            REQUIRE(pool->ContainsData(entry->GetBlockStart()));
            REQUIRE(entry->GetUses() == static_cast<int32_t>(1 + i));

            for (size_t i2 = 0; i2 < entry->GetSize(); ++i2) {
               REQUIRE(entry->GetBlockStart()[i2] == 66);
            }
         }

         // Deallocate N random entries                                 
         Logger::Special("Deallocating random entries in pool...");
         Allocation* prev_entry = nullptr;
         for (size_t i = 0; i < pool->GetMaxEntries()/20u; ++i) {
            auto entry = pool->AllocationFromIndex(i*20);
            REQUIRE(pool->ContainsAllocation(entry));
            pool->Deallocate(entry);
            REQUIRE(entry->GetUses() == 0);
            REQUIRE(entry->GetNextFreeEntry() == prev_entry);
            prev_entry = entry;
         }
         REQUIRE(pool->CanContain(pool->GetMinAllocation()));
         REQUIRE_FALSE(pool->CanContain(pot_t(pool->GetMinAllocation()*2u)));

         // Deallocate right half of entries                            
         Logger::Special("Deallocating right half of entries in pool...");
         for (auto entry = pool->GetAllocationData() + pool->GetMaxEntries()/1u - 1; entry >= pool->GetAllocationData() + pool->GetMaxEntries()/2u; --entry) {
            if (not entry->GetUses())
               continue;
            pool->Deallocate(entry);
            REQUIRE(entry->GetUses() == 0);
         }
         REQUIRE_FALSE(pool->CanContain(pot_t(pool->GetMinAllocation()*2u)));

         // Test the integrity of the free entry chain                  
         Logger::Special("Testing free chain integrity...");
         prev_entry = pool->GetLastFreedEntry();
         size_t chain_counter = 0;
         while (prev_entry) {
            REQUIRE(prev_entry->GetUses() == 0);
            ++chain_counter;
            prev_entry = prev_entry->GetNextFreeEntry();
         }
         REQUIRE(chain_counter == pool->GetCurrentEntries() - pool->GetValidEntries());

         // Deallocate more entries to enforce shrinking                
         Logger::Special("Deallocate more entries to enforce shrinking of pool...");
         for (size_t i = 16; i < static_cast<size_t>(pool->GetMaxEntries()); ++i) {
            auto entry = pool->AllocationFromIndex(i);
            if (entry->GetUses() == 0)
               continue;
            pool->Deallocate(entry);
         }
         REQUIRE(pool->CanContain(pot_t(pool->GetMinAllocation()*2u)));

         // Allocate a new one, should reuse prev_entry                 
         Logger::Special("Allocating a new entry...");
         prev_entry = pool->GetLastFreedEntry();
         auto new_entry = pool->Allocate(pot_t(pool->GetMinAllocation()*2u));
         REQUIRE(new_entry);
         REQUIRE(new_entry == prev_entry);
      }

      WHEN("An entry larger than the minimum is allocated") {
         auto entry = pool->Allocate(pot_t(min_alloc * 2));
         REQUIRE(entry);

         REQUIRE(pool->GetAllocatedByFrontend() == entry->GetSize());
         REQUIRE(pool->GetMaxEntries() == pool->GetAllocatedByBackend() / smallest);
         REQUIRE(pool->ContainsAllocation(entry));
         REQUIRE(pool->ContainsData(entry->GetBlockStart()));
         REQUIRE(pool->IsInUse());
      }

      WHEN("An entry larger than the pool itself is allocated") {
         auto entry = pool->Allocate(pot_t(default_size * 2));

         REQUIRE(entry == nullptr);
         REQUIRE(pool->GetAllocatedByFrontend() == 0);
         REQUIRE_FALSE(pool->IsInUse());
      }

      Allocator::DeallocatePool(pool);
   }
   
   GIVEN("A custom huge pool") {
      Pool* pool = Allocator::AllocatePool(meta, pot_t(default_size * 512));
      REQUIRE(pool);

      auto entry = pool->Allocate(1_pot);
      const auto full = pool->GetAllocatedByBackend();
      const auto smallest = pool->GetMinAllocation();

      REQUIRE(pool->GetAllocatedByFrontend() == entry->GetSize());
      REQUIRE(pool->GetMaxEntries() == full / smallest);
      REQUIRE(pool->ContainsAllocation(entry));
      REQUIRE(pool->ContainsData(entry->GetBlockStart()));
      REQUIRE(pool->IsInUse());

      #ifdef LANGULUS_STD_BENCHMARK 
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

   REQUIRE(memoryState.Assert());
   REQUIRE_FALSE(Allocator::CollectGarbage());
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
   static Allocator::State memoryState;

   const auto meta = MetaDataOf<TestType>();
   IF_SAFE(REQUIRE_THROWS(Allocator::Allocate(meta, 511_pot)));
   constexpr size_t testAlignment = alignof(TestType);
   constexpr size_t min_alloc = CT::GetMinAlloc<TestType>();

   GIVEN("A small allocation") {
      auto s = GENERATE(pot_t(Roof2(sizeof(TestType))),
                        pot_t(Roof2(sizeof(TestType)*2)),
                        pot_t(Roof2(sizeof(TestType)*16)));
      auto rounded_s = ::std::max(static_cast<size_t>(s), min_alloc);
      Allocation* entry = Allocator::Allocate(meta, s);
      REQUIRE(entry);

      WHEN("Memory is allocated on the heap") {
         REQUIRE(IsAligned(entry, alignof(Allocation)));
         REQUIRE(IsAligned(entry->GetBlockStart(), testAlignment));
         REQUIRE(entry->GetSize() == rounded_s);
         REQUIRE(entry->GetUses() == 1);

         size_t matches = 0;
         size_t mismatches = 0;
         for (size_t i = 0; i < rounded_s; ++i) {
            if (entry->Contains(entry->GetBlockStart() + i))
               ++matches;
            if (not entry->Contains(entry->GetBlockStart() - (i+1)))
               ++mismatches;
         }
         REQUIRE(matches == rounded_s);
         REQUIRE(mismatches == rounded_s);

         REQUIRE_FALSE(entry->Contains(entry->GetBlockStart() + rounded_s + 1));

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
         entry->Keep();

         REQUIRE(entry->GetUses() == 2);
         REQUIRE(Allocator::CheckAuthority(nullptr, entry->GetBlockStart()));
         REQUIRE_FALSE(Allocator::CheckAuthority(nullptr, entry));
         REQUIRE(Allocator::CheckAuthority(meta, entry->GetBlockStart()));
         REQUIRE_FALSE(Allocator::CheckAuthority(meta, entry));
         REQUIRE(Allocator::Find(nullptr, entry->GetBlockStart()));
         REQUIRE(Allocator::Find(meta, entry->GetBlockStart()));
         REQUIRE_FALSE(Allocator::Find(nullptr, entry));
         REQUIRE_FALSE(Allocator::Find(meta, entry));

         IF_SAFE(REQUIRE_THROWS(Allocator::Deallocate(entry)));
         entry->Free();
         Allocator::Deallocate(entry);
      }

      WHEN("Referenced multiple times") {
         entry->Keep(5);

         REQUIRE(entry->GetUses() == 6);
         IF_SAFE(REQUIRE_THROWS(Allocator::Deallocate(entry)));
         entry->Free(5);
         Allocator::Deallocate(entry);
      }

      WHEN("Dereferenced once without deletion") {
         entry->Keep();
         entry->Free();

         REQUIRE(entry->GetUses() == 1);
         Allocator::Deallocate(entry);
      }

      WHEN("Dereferenced multiple times without deletion") {
         entry->Keep(5);
         entry->Free(4);

         REQUIRE(entry->GetUses() == 2);
         IF_SAFE(REQUIRE_THROWS(Allocator::Deallocate(entry)));
         entry->Free(1);
         Allocator::Deallocate(entry);
      }

      WHEN("Dereferenced once with deletion") {
         Allocator::Deallocate(entry);

         REQUIRE_FALSE(Allocator::CheckAuthority(nullptr, entry));
         REQUIRE(Allocator::CheckAuthority(nullptr, entry->GetBlockStart()));
         REQUIRE_FALSE(Allocator::Find(nullptr, entry->GetBlockStart()));
         REQUIRE_FALSE(Allocator::Find(nullptr, entry));
      }
   }
   
   GIVEN("A large allocation") {
      pot_t s = 1024_pot * 1024_pot;
      Allocation* entry = Allocator::Allocate(meta, s);
      REQUIRE(entry);

      WHEN("Memory is allocated on the heap") {
         REQUIRE(IsAligned(entry, alignof(Allocation)));
         REQUIRE(IsAligned(entry->GetBlockStart(), alignof(TestType)));
         REQUIRE(entry->GetSize() == s);
         REQUIRE(entry->GetUses() == 1);

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

      Allocator::Deallocate(entry);
   }

   REQUIRE(memoryState.Assert());
   REQUIRE_FALSE(Allocator::CollectGarbage());
}
