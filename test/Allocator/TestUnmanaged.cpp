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
   constexpr size_t testAlignment = ::std::max(alignof(TestType), Alignment);
   
   GIVEN("A small allocation") {
      auto s = GENERATE(1_pot, 2_pot, 512_pot);
      Allocation* entry = Allocator::Allocate(pot_t(alignof(TestType)), s);
      REQUIRE(entry);

      WHEN("Memory is allocated on the heap") {
         REQUIRE(entry->GetBlockStart() == Align(reinterpret_cast<uint8_t*>(entry) + sizeof(Allocation), testAlignment));
         REQUIRE(IsAligned(entry, alignof(Allocation)));
         REQUIRE(IsAligned(entry->GetBlockStart(), testAlignment));
         REQUIRE(entry->GetSize() == pot_t(Align(static_cast<size_t>(s), testAlignment)));
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

         REQUIRE_FALSE(entry->Contains(entry->GetBlockStart() + Align(static_cast<size_t>(s), testAlignment)));

         Allocator::Deallocate(entry);

         #if LANGULUS(BENCHMARK)
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
         entry->AddRef(1);

         REQUIRE(entry->GetUses() == 2);
         IF_SAFE(REQUIRE_THROWS(Allocator::Deallocate(entry)));
         IF_SAFE(REQUIRE(entry->GetUses() == 2));
         
         entry->AddRef(-1);
         Allocator::Deallocate(entry);
      }

      WHEN("Referenced multiple times") {
         entry->AddRef(5);

         REQUIRE(entry->GetUses() == 6);

         IF_SAFE(REQUIRE_THROWS(Allocator::Deallocate(entry)));
         IF_SAFE(REQUIRE(entry->GetUses() == 6));

         entry->AddRef(-5);
         Allocator::Deallocate(entry);
      }

      WHEN("Dereferenced once without deletion") {
         entry->AddRef(1);
         entry->AddRef(-1);

         REQUIRE(entry->GetUses() == 1);

         Allocator::Deallocate(entry);
      }

      WHEN("Dereferenced multiple times without deletion") {
         entry->AddRef(5);
         entry->AddRef(-4);

         REQUIRE(entry->GetUses() == 2);

         IF_SAFE(REQUIRE_THROWS(Allocator::Deallocate(entry)));
         IF_SAFE(REQUIRE(entry->GetUses() == 2));

         entry->AddRef(-1);
         Allocator::Deallocate(entry);
      }

      WHEN("Dereferenced once with deletion") {
         Allocator::Deallocate(entry);
      }

      WHEN("Dereferenced multiple times with deletion") {
         entry->AddRef(5);

         IF_SAFE(REQUIRE_THROWS(Allocator::Deallocate(entry)));
         IF_SAFE(REQUIRE(entry->GetUses() == 6));

         entry->AddRef(-5);
         Allocator::Deallocate(entry);
      }
   }
   
   GIVEN("A large allocation") {
      auto s = 4096_pot*1024_pot;
      Allocation* entry = Allocator::Allocate(pot_t(alignof(TestType)), s);
      REQUIRE(entry);

      WHEN("Memory is allocated on the heap") {
         REQUIRE(entry->GetBlockStart() == Align(reinterpret_cast<uint8_t*>(entry) + sizeof(Allocation), testAlignment));
         REQUIRE(IsAligned(entry, alignof(Allocation)));
         REQUIRE(IsAligned(entry->GetBlockStart(), alignof(TestType)));
         REQUIRE(entry->GetSize() == pot_t(Align(static_cast<size_t>(s), testAlignment)));
         REQUIRE(entry->GetUses() == 1);

         #if LANGULUS(BENCHMARK)
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
