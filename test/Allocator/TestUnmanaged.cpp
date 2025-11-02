///                                                                           
/// Langulus::Fractalloc                                                      
/// Copyright (c) 2015 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "TestAllocatorCommon.hpp"

#if LANGULUS_FEATURE(MANAGED_MEMORY)
   #error "This file shouldn't be included if MANAGED_MEMORY is enabled"
#endif


TEMPLATE_TEST_CASE("Testing allocator functions", "[allocator]",
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
   IF_SAFE(REQUIRE_THROWS(Allocator::Allocate(pot_t(alignof(TestType)), 511_pot)));
   constexpr size_t testAlignment = alignof(TestType);
   
   GIVEN("A small allocation") {
      auto s = GENERATE(1_pot, 2_pot, 512_pot);
      Allocation* entry = Allocator::Allocate(pot_t(alignof(TestType)), s);
      REQUIRE(entry);

      WHEN("Memory is allocated on the heap") {
         REQUIRE(entry->GetBlockStart() == Align(reinterpret_cast<uint8_t*>(entry) + sizeof(Allocation), testAlignment));
         REQUIRE(IsAligned(entry, alignof(Allocation)));
         REQUIRE(IsAligned(entry->GetBlockStart(), testAlignment));
         REQUIRE(entry->GetSize() == s);
         REQUIRE(entry->GetUses() == 1);

         size_t matches = 0;
         size_t mismatches = 0;
         for (size_t i = 0; i < s; ++i) {
            if (entry->Contains(entry->GetBlockStart() + i))
               ++matches;
            if (not entry->Contains(entry->GetBlockStart() - (i+1)))
               ++mismatches;
         }
         REQUIRE(matches == s);
         REQUIRE(mismatches == s);

         REQUIRE_FALSE(entry->Contains(entry->GetBlockStart() + static_cast<size_t>(s)));

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
         IF_SAFE(REQUIRE_THROWS(Allocator::Deallocate(entry)));
         IF_SAFE(REQUIRE(entry->GetUses() == 2));
         
         entry->Free();
         Allocator::Deallocate(entry);
      }

      WHEN("Referenced multiple times") {
         entry->Keep(5);

         REQUIRE(entry->GetUses() == 6);

         IF_SAFE(REQUIRE_THROWS(Allocator::Deallocate(entry)));
         IF_SAFE(REQUIRE(entry->GetUses() == 6));

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
         IF_SAFE(REQUIRE(entry->GetUses() == 2));

         entry->Free(1);
         Allocator::Deallocate(entry);
      }

      WHEN("Dereferenced once with deletion") {
         Allocator::Deallocate(entry);
      }

      WHEN("Dereferenced multiple times with deletion") {
         entry->Keep(5);

         IF_SAFE(REQUIRE_THROWS(Allocator::Deallocate(entry)));
         IF_SAFE(REQUIRE(entry->GetUses() == 6));

         entry->Free(5);
         Allocator::Deallocate(entry);
      }
   }
   
   GIVEN("A large allocation") {
      auto s = 4096_pot*1024_pot;
      Allocation* entry = Allocator::Allocate(pot_t(alignof(TestType)), s);
      REQUIRE(entry);

      WHEN("Memory is allocated on the heap") {
         REQUIRE(entry->GetBlockStart() == reinterpret_cast<uint8_t*>(Align(entry + 1, alignof(TestType))));
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
}
