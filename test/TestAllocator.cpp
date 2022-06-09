///																									
/// Langulus::Anyness																			
/// Copyright(C) 2012 Dimo Markov <langulusteam@gmail.com>							
///																									
/// Distributed under GNU General Public License v3+									
/// See LICENSE file, or https://www.gnu.org/licenses									
///																									
#include "Main.hpp"
#include <catch2/catch.hpp>

using Type1 = uint8_t;
using Type2 = uint16_t;
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


SCENARIO("Testing CountLeadingZeroes calls", "[allocator]") {
	const Size numbers[] {
		0, 1, 2, 3, 4, 5, 6, 11, 16, 64, 99, 120, 128
	};

	#if LANGULUS(BITNESS) == 32
		const Size results[] {
			32, 31, 30, 30, 29, 29, 29, 28, 27, 25, 25, 25, 24
		};
	#elif LANGULUS(BITNESS) == 64
		const Size results[] {
			64, 63, 62, 62, 61, 61, 61, 60, 59, 57, 57, 57, 56
		};
	#endif

	static_assert(sizeof(numbers) == sizeof(results), "Oops");

	WHEN("CountLeadingZeroes is executed") {
		THEN("Results should be correct") {
			for (int i = 0; i < sizeof(numbers) / sizeof(Size); ++i) {
				REQUIRE(CountLeadingZeroes(numbers[i]) == results[i]);
			}
		}
	}
}

SCENARIO("Testing CountTrailingZeroes calls", "[allocator]") {
	const Size numbers[] {
		0, 1, 2, 3, 4, 5, 6, 11, 16, 64, 99, 120, 128
	};

	#if LANGULUS(BITNESS) == 32
		const Size results[] {
			32, 0, 1, 0, 2, 0, 1, 0, 4, 6, 0, 3, 7
		};
	#elif LANGULUS(BITNESS) == 64
		const Size results[] {
			64, 0, 1, 0, 2, 0, 1, 0, 4, 6, 0, 3, 7
		};
	#endif

	static_assert(sizeof(numbers) == sizeof(results), "Oops");

	WHEN("CountTrailingZeroes is executed") {
		THEN("Results should be correct") {
			for (int i = 0; i < sizeof(numbers) / sizeof(Size); ++i) {
				REQUIRE(CountTrailingZeroes(numbers[i]) == results[i]);
			}
		}
	}
}

TEMPLATE_TEST_CASE("Testing IsPowerOfTwo calls", "[allocator]", uint8_t, uint16_t, uint32_t, uint64_t) {
	using T = TestType;
	const T numbers[] {
		0, 1, 2, 3, 4, 5, 6, 11, 16, 64, 99, 120, 128
	};
	const bool results[] {
		false, true, true, false, true, false, false, false, true, true, false, false, true
	};
	static_assert(sizeof(numbers)/sizeof(T) == sizeof(results)/sizeof(bool), "Oops");

	WHEN("IsPowerOfTwo is executed") {
		THEN("Results should be correct") {
			for (int i = 0; i < sizeof(numbers) / sizeof(T); ++i) {
				REQUIRE(IsPowerOfTwo(numbers[i]) == results[i]);
			}
		}
	}
}

TEMPLATE_TEST_CASE("Testing Roof2 calls", "[allocator]", uint8_t, uint16_t, uint32_t, uint64_t) {
	using T = TestType;
	const T numbers[] {
		0, 1, 2, 3, 4, 5, 6, 11, 16, 64, 99, 120, 128
	};
	const T results[] {
		0, 1, 2, 4, 4, 8, 8, 16, 16, 64, 128, 128, 128
	};
	static_assert(sizeof(numbers) == sizeof(results), "Oops");

	WHEN("Roof2 is executed") {
		THEN("Results should be correct") {
			for (int i = 0; i < sizeof(numbers) / sizeof(T); ++i) {
				if (numbers[i] <= 128 || sizeof(T) > 1)
					REQUIRE(Roof2<true>(numbers[i]) == results[i]);
				else
					REQUIRE_THROWS_AS(Roof2<true>(numbers[i]), Except::Overflow);
			}
		}
	}
}

SCENARIO("Testing FastLog2 calls", "[allocator]") {
	const Size numbers[] {
		0, 1, 2, 3, 4, 5, 6, 11, 16, 64, 99, 120, 128
	};
	const Size results[] {
		0, 0, 1, 1, 2, 2, 2,  3,  4,  6,  6,   6,   7
	};
	static_assert(sizeof(numbers) == sizeof(results), "Oops");

	WHEN("FastLog2 is executed") {
		THEN("Results should be correct") {
			for (int i = 0; i < sizeof(numbers) / sizeof(Size); ++i) {
				REQUIRE(Anyness::Inner::FastLog2(numbers[i]) == results[i]);
			}
		}
	}
}

TEMPLATE_TEST_CASE("Testing GetAllocationPageOf<T> calls", "[allocator]", Type1, Type2, Type4, Type8, TypeBig, TypeVeryBig) {
	WHEN("GetAllocationPageOf<T> is executed") {
		THEN("Results should be correct") {
			REQUIRE(IsPowerOfTwo(GetAllocationPageOf<TestType>()));
			REQUIRE(GetAllocationPageOf<TestType>() >= sizeof(TestType));
		}
	}
}

SCENARIO("Testing pool functions", "[allocator]") {
	GIVEN("A pool") {
		Pool* pool = nullptr;

		WHEN("Default pool size is allocated on the pool") {
			pool = Allocator::AllocatePool(Pool::DefaultPoolSize);
			const auto originPtr = pool->GetPoolStart<Byte>();
			const Pointer origin = reinterpret_cast<Pointer>(originPtr);
			const Pointer half = Pool::DefaultPoolSize / 2;
			const Pointer quarter = Pool::DefaultPoolSize / 4;

			THEN("Requirements should be met") {
				REQUIRE(pool->GetAllocatedByBackend() == Pool::DefaultPoolSize);
				REQUIRE(reinterpret_cast<Pointer>(pool->AllocationFromIndex(0)) == origin);
				REQUIRE(reinterpret_cast<Pointer>(pool->AllocationFromIndex(1)) == origin + half);
				REQUIRE(reinterpret_cast<Pointer>(pool->AllocationFromIndex(2)) == origin + quarter);
				REQUIRE(reinterpret_cast<Pointer>(pool->AllocationFromIndex(3)) == origin + quarter + half);
				REQUIRE(pool->ThresholdFromIndex(0) == pool->GetAllocatedByBackend());
				REQUIRE(pool->ThresholdFromIndex(1) == half);
				REQUIRE(pool->ThresholdFromIndex(2) == quarter);
				REQUIRE(pool->ThresholdFromIndex(3) == quarter);
				REQUIRE(pool->ThresholdFromIndex(4) == quarter / 2);
				REQUIRE(pool->ThresholdFromIndex(5) == quarter / 2);
				REQUIRE(pool->ThresholdFromIndex(6) == quarter / 2);
				REQUIRE(pool->ThresholdFromIndex(7) == quarter / 2);
				REQUIRE(pool->ThresholdFromIndex(8) == quarter / 4);
				REQUIRE(pool->ThresholdFromIndex(pool->GetMaxEntries() - 1) == pool->GetMinAllocation());
				REQUIRE(pool->ThresholdFromIndex(pool->GetMaxEntries()) == pool->GetMinAllocation() / 2);
				REQUIRE(pool->CanContain(1));
				REQUIRE(pool->CanContain(Alignment));
				REQUIRE(pool->CanContain(pool->GetMinAllocation()));
				REQUIRE(pool->CanContain(Pool::DefaultPoolSize / 2));
				REQUIRE(pool->CanContain(Pool::DefaultPoolSize));
				REQUIRE_FALSE(pool->CanContain(Pool::DefaultPoolSize + 1));
				REQUIRE(pool->GetAllocatedByBackend() == Pool::DefaultPoolSize);
				REQUIRE(pool->GetAllocatedByFrontend() == 0);
				REQUIRE(pool->GetMaxEntries() == Pool::DefaultPoolSize / pool->GetMinAllocation());
				REQUIRE(pool->Contains(originPtr));
				REQUIRE(pool->Contains(originPtr + half));
				REQUIRE(pool->Contains(originPtr + half * 2 - 1));
				REQUIRE_FALSE(pool->Contains(originPtr + half * 2));
				REQUIRE_FALSE(pool->Contains(nullptr));
				REQUIRE_FALSE(pool->IsInUse());
			}

			Allocator::DeallocatePool(pool);
		}

		WHEN("A small entry is allocated inside a new default-sized pool") {
			pool = Allocator::AllocatePool(Pool::DefaultPoolSize);
			auto entry = pool->CreateEntry(5);

			THEN("Requirements should be met") {
				REQUIRE(pool->GetAllocatedByFrontend() == entry->GetTotalSize());
				REQUIRE(pool->GetMinAllocation() == Pool::DefaultMinAllocation);
				REQUIRE(pool->GetMaxEntries() == Pool::DefaultPoolSize / pool->GetMinAllocation());
				REQUIRE(pool->Contains(entry));
				REQUIRE(pool->IsInUse());
			}

			Allocator::DeallocatePool(pool);
		}

		WHEN("A new default-sized pool is filled with all possible small entries") {
			pool = Allocator::AllocatePool(Pool::DefaultPoolSize);

			// Fill up
			for (int i = 0; i < pool->GetMaxEntries(); ++i) {
				auto entry = pool->CreateEntry(5);
				entry->Keep(i);
			}

			// Add more
			for (int i = 0; i < 5; ++i) {
				auto entry = pool->CreateEntry(5);
				REQUIRE(entry == nullptr);
			}

			THEN("Requirements should be met") {
				REQUIRE(pool->GetAllocatedByFrontend() == pool->GetMaxEntries() * (Allocation::GetSize() + 5));
				REQUIRE(pool->GetMinAllocation() == Pool::DefaultMinAllocation);
				REQUIRE(pool->GetMaxEntries() == Pool::DefaultPoolSize / pool->GetMinAllocation());
				for (int i = 0; i < pool->GetMaxEntries(); ++i) {
					auto entry = pool->AllocationFromIndex(i);
					REQUIRE(pool->Contains(entry));
					REQUIRE(entry->GetUses() == 1 + i);
					REQUIRE(entry->GetAllocatedSize() == 5);
				}
			}

			Allocator::DeallocatePool(pool);
		}

		WHEN("An entry larger than the minimum is allocated inside a new default-sized pool") {
			pool = Allocator::AllocatePool(Pool::DefaultPoolSize);
			auto entry = pool->CreateEntry(Pool::DefaultMinAllocation);

			THEN("Requirements should be met") {
				REQUIRE(pool->GetAllocatedByFrontend() == entry->GetTotalSize());
				REQUIRE(pool->GetMinAllocation() == Roof2(entry->GetTotalSize()));
				REQUIRE(pool->GetMaxEntries() == Pool::DefaultPoolSize / pool->GetMinAllocation());
				REQUIRE(pool->Contains(entry));
				REQUIRE(pool->IsInUse());
			}

			Allocator::DeallocatePool(pool);
		}

		WHEN("An entry larger than the pool itself is allocated inside a new default-sized pool") {
			pool = Allocator::AllocatePool(Pool::DefaultPoolSize);
			auto entry = pool->CreateEntry(Pool::DefaultPoolSize);

			THEN("The resulting allocation should be invalid") {
				REQUIRE(entry == nullptr);
				REQUIRE(pool->GetAllocatedByFrontend() == 0);
				REQUIRE(pool->GetMinAllocation() == Pool::DefaultMinAllocation);
				REQUIRE_FALSE(pool->IsInUse());
			}

			Allocator::DeallocatePool(pool);
		}
	}
}

SCENARIO("Testing allocator functions", "[allocator]") {
	GIVEN("An allocation") {
		Allocation* entry = nullptr;

		WHEN("Memory is allocated on the heap") {
			entry = Allocator::Allocate(512);

			THEN("Requirements should be met") {
				REQUIRE(entry->GetBlockStart() != nullptr);
				REQUIRE(entry->GetBlockStart() != reinterpret_cast<Byte*>(entry));
				REQUIRE(reinterpret_cast<Pointer>(entry) % Alignment == 0);
				REQUIRE(reinterpret_cast<Pointer>(entry->GetBlockStart()) % Alignment == 0);
				REQUIRE(entry->GetAllocatedSize() >= 512);
				REQUIRE(entry->GetBlockEnd() == entry->GetBlockStart() + entry->GetAllocatedSize());
				REQUIRE(entry->GetSize() % Alignment == 0);
				REQUIRE(entry->GetBlockStart() == reinterpret_cast<Byte*>(entry) + entry->GetSize());
				REQUIRE(entry->GetUses() == 1);
				for (Size i = 0; i < 512; ++i) {
					auto p = entry->GetBlockStart() + i;
					REQUIRE(entry->Contains(p));
				}
				for (Size i = 512; i < 513; ++i) {
					auto p = entry->GetBlockStart() + i;
					REQUIRE_FALSE(entry->Contains(p));
				}
			}

			Allocator::Deallocate(entry);

			//#ifdef LANGULUS_STD_BENCHMARK // Last result: 
				BENCHMARK_ADVANCED("Allocator::Allocate(5)") (Catch::Benchmark::Chronometer meter) {
 					std::vector<Allocation*> storage(meter.runs());
					meter.measure([&](int i) {
						return storage[i] = Allocator::Allocate(5);
					});

					for (auto& i : storage)
						Allocator::Deallocate(i);
				};

				BENCHMARK_ADVANCED("malloc(5)") (Catch::Benchmark::Chronometer meter) {
					std::vector<void*> storage(meter.runs());
					meter.measure([&](int i) {
						return storage[i] = ::std::malloc(5);
					});

					for (auto& i : storage)
						::std::free(i);
				};

				BENCHMARK_ADVANCED("Allocator::Allocate(512)") (Catch::Benchmark::Chronometer meter) {
 					std::vector<Allocation*> storage(meter.runs());
					meter.measure([&](int i) {
						return storage[i] = Allocator::Allocate(512);
					});

					for (auto& i : storage)
						Allocator::Deallocate(i);
				};

				BENCHMARK_ADVANCED("malloc(512)") (Catch::Benchmark::Chronometer meter) {
					std::vector<void*> storage(meter.runs());
					meter.measure([&](int i) {
						return storage[i] = ::std::malloc(512);
					});

					for (auto& i : storage)
						::std::free(i);
				};

				BENCHMARK_ADVANCED("Allocator::Allocate(Pool::DefaultPoolSize)") (Catch::Benchmark::Chronometer meter) {
 					std::vector<Allocation*> storage(meter.runs());
					meter.measure([&](int i) {
						return storage[i] = Allocator::Allocate(Pool::DefaultPoolSize);
					});

					for (auto& i : storage)
						Allocator::Deallocate(i);
				};

				BENCHMARK_ADVANCED("malloc(Pool::DefaultPoolSize)") (Catch::Benchmark::Chronometer meter) {
					std::vector<void*> storage(meter.runs());
					meter.measure([&](int i) {
						return storage[i] = ::std::malloc(Pool::DefaultPoolSize);
					});

					for (auto& i : storage)
						::std::free(i);
				};
			//#endif
		}

		WHEN("Referenced once") {
			entry = Allocator::Allocate(512);
			entry->Keep();

			THEN("Requirements should be met") {
				REQUIRE(entry->GetUses() == 2);
				REQUIRE(Allocator::CheckAuthority(nullptr, entry));
				REQUIRE(Allocator::Find(nullptr, entry->GetBlockStart()));
				REQUIRE_FALSE(Allocator::Find(nullptr, entry));
			}

			Allocator::Deallocate(entry);
		}

		WHEN("Referenced multiple times") {
			entry = Allocator::Allocate(512);
			entry->Keep(5);

			THEN("Requirements should be met") {
				REQUIRE(entry->GetUses() == 6);
				REQUIRE(Allocator::CheckAuthority(nullptr, entry));
				REQUIRE(Allocator::Find(nullptr, entry->GetBlockStart()));
				REQUIRE_FALSE(Allocator::Find(nullptr, entry));
			}

			Allocator::Deallocate(entry);
		}

		WHEN("Dereferenced once without deletion") {
			entry = Allocator::Allocate(512);
			entry->Keep();
			entry->Free();

			THEN("Requirements should be met") {
				REQUIRE(entry->GetUses() == 1);
				REQUIRE(Allocator::CheckAuthority(nullptr, entry));
				REQUIRE(Allocator::Find(nullptr, entry->GetBlockStart()));
				REQUIRE_FALSE(Allocator::Find(nullptr, entry));
			}

			Allocator::Deallocate(entry);
		}

		WHEN("Dereferenced multiple times without deletion") {
			entry = Allocator::Allocate(512);
			entry->Keep(5);
			entry->Free(4);

			THEN("Requirements should be met") {
				REQUIRE(entry->GetUses() == 2);
				REQUIRE(Allocator::CheckAuthority(nullptr, entry));
				REQUIRE(Allocator::Find(nullptr, entry->GetBlockStart()));
				REQUIRE_FALSE(Allocator::Find(nullptr, entry));
			}

			Allocator::Deallocate(entry);
		}

		WHEN("Dereferenced once with deletion") {
			entry = Allocator::Allocate(512);
			Allocator::Deallocate(entry);

			THEN("We shouldn't be able to access the memory any longer, but it is still under jurisdiction") {
				REQUIRE(Allocator::CheckAuthority(nullptr, entry));
				REQUIRE_FALSE(Allocator::Find(nullptr, entry->GetBlockStart()));
				REQUIRE_FALSE(Allocator::Find(nullptr, entry));
			}
		}

		WHEN("Dereferenced multiple times with deletion") {
			entry = Allocator::Allocate(512);
			entry->Keep(5);
			Allocator::Deallocate(entry);

			THEN("We shouldn't be able to access the memory any longer, but it is still under jurisdiction") {
				REQUIRE(Allocator::CheckAuthority(nullptr, entry));
				REQUIRE_FALSE(Allocator::Find(nullptr, entry->GetBlockStart()));
				REQUIRE_FALSE(Allocator::Find(nullptr, entry));
			}
		}
	}
}
