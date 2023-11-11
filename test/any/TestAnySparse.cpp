///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// Distributed under GNU General Public License v3+                          
/// See LICENSE file, or https://www.gnu.org/licenses                         
///                                                                           
#include "TestAnyCommon.hpp"


/// The main test for Any/TAny containers, with all kinds of items, from      
/// sparse to dense, from trivial to complex, from flat to deep               
TEMPLATE_TEST_CASE("Sparse Any/TAny", "[any]", 
   (TypePair<TAny<int*>, int*>),
   (TypePair<TAny<Trait*>, Trait*>),
   (TypePair<TAny<Traits::Count*>, Traits::Count*>),
   (TypePair<TAny<Any*>, Any*>),
   (TypePair<TAny<Text*>, Text*>),

   (TypePair<Any, int*>),
   (TypePair<Any, Trait*>),
   (TypePair<Any, Traits::Count*>),
   (TypePair<Any, Any*>),
   (TypePair<Any, Text*>),

   (TypePair<Traits::Name, Text*>)
) {
   using T = typename TestType::LHS;
   using E = typename TestType::RHS;
   using DenseE = Decay<E>;
   using SparseE = E*;
      
   E element = CreateElement<E>(555);
   const DenseE& denseValue {DenseCast(element)};
   const DenseE* const sparseValue {SparseCast(element)};

   const E darray1[5] {
      CreateElement<E>(1),
      CreateElement<E>(2),
      CreateElement<E>(3),
      CreateElement<E>(4),
      CreateElement<E>(5)
   };
   const E darray2[5] {
      CreateElement<E>(6),
      CreateElement<E>(7),
      CreateElement<E>(8),
      CreateElement<E>(9),
      CreateElement<E>(10)
   };

   GIVEN("Default constructed container") {
      T pack;

      CheckState_Default<E>(pack);

      #ifdef LANGULUS_STD_BENCHMARK
         BENCHMARK_ADVANCED("default construction") (timer meter) {
            IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
            some<uninitialized<T>> storage(meter.runs());
            meter.measure([&](int i) {
               return storage[i].construct();
            });
         };

         BENCHMARK_ADVANCED("std::vector::default construction") (timer meter) {
            IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
            some<uninitialized<StdT>> storage(meter.runs());
            meter.measure([&](int i) {
               return storage[i].construct();
            });
         };

         BENCHMARK_ADVANCED("std::any::default construction") (timer meter) {
            IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
            some<uninitialized<std::any>> storage(meter.runs());
            meter.measure([&](int i) {
               return storage[i].construct();
            });
         };
      #endif

      WHEN("Assigned value by copy") {
         pack = element;

         CheckState_OwnedFull<E>(pack);
         REQUIRE(&pack.template As<DenseE>() == sparseValue);
         REQUIRE(pack.template As<DenseE>() == denseValue);
         REQUIRE(*pack.template As<DenseE*>() == denseValue);
         REQUIRE(pack.GetUses() == 1);
         REQUIRE(*pack.GetRawSparse() == asbytes(sparseValue));
         IF_LANGULUS_MANAGED_MEMORY(REQUIRE(*pack.GetEntries() == nullptr));
         REQUIRE_THROWS(pack.template As<float>() == 0.0f);
         REQUIRE_THROWS(pack.template As<float*>() == nullptr);

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("operator = (single value copy)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               some<T> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = value;
               });
            };

            BENCHMARK_ADVANCED("std::vector::operator = (single value copy)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               some<StdT> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = {value};
               });
            };

            BENCHMARK_ADVANCED("std::any::operator = (single value copy)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               some<std::any> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = value;
               });
            };
         #endif
      }
      
      WHEN("Assigned value by move") {
         auto movable = element;
         pack = ::std::move(movable);

         CheckState_OwnedFull<E>(pack);
         REQUIRE(&pack.template As<DenseE>() == sparseValue);
         REQUIRE(pack.template As<DenseE>() == denseValue);
         REQUIRE(*pack.template As<DenseE*>() == denseValue);
         REQUIRE(pack.GetUses() == 1);
         REQUIRE(*pack.GetRawSparse() == asbytes(sparseValue));
         IF_LANGULUS_MANAGED_MEMORY(REQUIRE(*pack.GetEntries() == nullptr));
         REQUIRE_THROWS(pack.template As<float>() == 0.0f);
         REQUIRE_THROWS(pack.template As<float*>() == nullptr);

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("operator = (single value move)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               some<T> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = ::std::move(value);
               });
            };

            BENCHMARK_ADVANCED("std::vector::operator = (single value move)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               some<StdT> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = {::std::move(value)};
               });
            };

            BENCHMARK_ADVANCED("std::any::operator = (single value move)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               some<std::any> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = ::std::move(value);
               });
            };
         #endif
      }

      WHEN("Assigned disowned value") {
         pack = Disown(element);

         CheckState_OwnedFull<E>(pack);
         REQUIRE(pack.template As<DenseE>() == denseValue);
         REQUIRE(*pack.template As<DenseE*>() == denseValue);
         REQUIRE(pack.GetUses() == 1);
         REQUIRE(*pack.GetRawSparse() == asbytes(sparseValue));
         IF_LANGULUS_MANAGED_MEMORY(REQUIRE(*pack.GetEntries() == nullptr));
         REQUIRE_THROWS(pack.template As<float>() == 0.0f);
         REQUIRE_THROWS(pack.template As<float*>() == nullptr);

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("operator = (single disowned value)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               some<T> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = Disown(value);
               });
            };

            BENCHMARK_ADVANCED("std::vector::operator = (single value copy)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               some<StdT> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = {value};
               });
            };

            BENCHMARK_ADVANCED("std::any::operator = (single value copy)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               some<std::any> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = value;
               });
            };
         #endif
      }
      
      WHEN("Assigned abandoned value") {
         auto movable = element;
         pack = Abandon(movable);

         CheckState_OwnedFull<E>(pack);
         REQUIRE(pack.template As<DenseE>() == denseValue);
         REQUIRE(*pack.template As<DenseE*>() == denseValue);
         REQUIRE(pack.GetUses() == 1);
         REQUIRE(*pack.GetRawSparse() == asbytes(sparseValue));
         IF_LANGULUS_MANAGED_MEMORY(REQUIRE(*pack.GetEntries() == nullptr));
         REQUIRE_THROWS(pack.template As<float>() == 0.0f);
         REQUIRE_THROWS(pack.template As<float*>() == nullptr);

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("operator = (single abandoned value)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               some<T> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = Abandon(value);
               });
            };

            BENCHMARK_ADVANCED("std::vector::operator = (single value move)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               some<StdT> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = {::std::move(value)};
               });
            };

            BENCHMARK_ADVANCED("std::any::operator = (single value move)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               some<std::any> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = ::std::move(value);
               });
            };
         #endif
      }

      WHEN("Assigned empty self") {
         pack = pack;

         CheckState_Default<E>(pack);

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("operator = (self)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               some<T> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = storage[i];
               });
            };

            BENCHMARK_ADVANCED("std::vector::operator = (self)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               some<StdT> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = storage[i];
               });
            };

            BENCHMARK_ADVANCED("std::any::operator = (self)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               some<std::any> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = storage[i];
               });
            };
         #endif
      }

      WHEN("Populated using Any::New") {
         if constexpr (not CT::Typed<T>) {
            if constexpr (CT::Trait<T>)
               pack = T::template From<Traits::Count, E>();
            else
               pack = T::template From<E>();
         }

         const auto created = pack.New(3, darray2[0]);

         CheckState_OwnedFull<E>(pack);

         REQUIRE(pack.GetCount() == 3);
         REQUIRE(created == 3);
         REQUIRE(pack.GetUses() == 1);
         for (auto it : pack)
            REQUIRE(it == darray2[0]);
      }

      WHEN("Shallow-copy more of the same stuff to the back (<<)") {
         pack << darray2[0] << darray2[1] << darray2[2] << darray2[3] << darray2[4];

         CheckState_OwnedFull<E>(pack);

         REQUIRE(pack.GetCount() == 5);
         REQUIRE(pack.GetReserved() >= 5);
         for (unsigned i = 0; i < 5; ++i)
            REQUIRE(pack[i] == darray2[i]);
         
         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("Anyness::TAny::operator << (5 consecutive trivial copies)") (timer meter) {
               some<T> storage(meter.runs());

               meter.measure([&](int i) {
                  return storage[i] << darray2[0] << darray2[1] << darray2[2] << darray2[3] << darray2[4];
               });
            };

            BENCHMARK_ADVANCED("std::vector::push_back(5 consecutive trivial copies)") (timer meter) {
               some<StdT> storage(meter.runs());

               meter.measure([&](int i) {
                  auto& s = storage[i];
                  s.push_back(darray2[0]);
                  s.push_back(darray2[1]);
                  s.push_back(darray2[2]);
                  s.push_back(darray2[3]);
                  return s.push_back(darray2[4]);
               });
            };
         #endif
      }

      WHEN("Shallow-copy more of the same stuff to the front (>>)") {
         pack >> darray2[0] >> darray2[1] >> darray2[2] >> darray2[3] >> darray2[4];

         CheckState_OwnedFull<E>(pack);

         REQUIRE(pack.GetCount() == 5);
         REQUIRE(pack.GetReserved() >= 5);
         for (unsigned i = 0; i < 5; ++i)
            REQUIRE(pack[i] == darray2[4-i]);
         
         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("Anyness::TAny::operator >> (5 consecutive trivial copies)") (timer meter) {
               some<T> storage(meter.runs());

               meter.measure([&](int i) {
                  return storage[i] >> darray2[0] >> darray2[1] >> darray2[2] >> darray2[3] >> darray2[4];
               });
            };

            BENCHMARK_ADVANCED("std::vector::push_front(5 consecutive trivial copies)") (timer meter) {
               some<StdT> storage(meter.runs());

               meter.measure([&](int i) {
                  auto& s = storage[i];
                  s.push_front(darray2[0]);
                  s.push_front(darray2[1]);
                  s.push_front(darray2[2]);
                  s.push_front(darray2[3]);
                  return s.push_front(darray2[4]);
               });
            };
         #endif
      }

      WHEN("Shallow-copy an array to the back") {
         pack.template Insert<IndexBack>(darray2, darray2 + 5);

         CheckState_OwnedFull<E>(pack);

         REQUIRE(pack.GetCount() == 5);
         REQUIRE(pack.GetReserved() >= 5);
         for (unsigned i = 0; i < 5; ++i)
            REQUIRE(pack[i] == darray2[i]);
         
         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("Anyness::TAny::Insert<IndexBack> (5 trivial copies)") (timer meter) {
               some<T> storage(meter.runs());

               meter.measure([&](int i) {
                  return storage[i].template Insert<IndexBack>(darray2, darray2 + 5);
               });
            };

            BENCHMARK_ADVANCED("std::vector::insert to back (5 trivial copies)") (timer meter) {
               some<StdT> storage(meter.runs());

               meter.measure([&](int i) {
                  return storage[i].insert(storage[i].end(), darray2, darray2 + 5);
               });
            };
         #endif
      }

      WHEN("Shallow-copy an array to the front") {
         pack.template Insert<IndexFront>(darray2, darray2 + 5);

         CheckState_OwnedFull<E>(pack);

         REQUIRE(pack.GetCount() == 5);
         REQUIRE(pack.GetReserved() >= 5);
         for (unsigned i = 0; i < 5; ++i)
            REQUIRE(pack[i] == darray2[i]);
         
         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("Anyness::TAny::Insert<IndexFront> (5 trivial copies)") (timer meter) {
               some<T> storage(meter.runs());

               meter.measure([&](int i) {
                  return storage[i].template Insert<IndexFront>(darray2, darray2 + 5);
               });
            };

            BENCHMARK_ADVANCED("std::vector::insert to front (5 trivial copies)") (timer meter) {
               some<StdT> storage(meter.runs());

               meter.measure([&](int i) {
                  return storage[i].insert(storage[i].begin(), darray2, darray2 + 5);
               });
            };
         #endif
      }

      WHEN("Move more of the same stuff to the back (<<)") {
         E darray3[5] {
            CreateElement<E>(6),
            CreateElement<E>(7),
            CreateElement<E>(8),
            CreateElement<E>(9),
            CreateElement<E>(10)
         };

         const E darray3backup[5] {
            darray3[0],
            darray3[1],
            darray3[2],
            darray3[3],
            darray3[4],
         };

         pack
            << ::std::move(darray3[0])
            << ::std::move(darray3[1])
            << ::std::move(darray3[2])
            << ::std::move(darray3[3])
            << ::std::move(darray3[4]);

         CheckState_OwnedFull<E>(pack);

         REQUIRE(pack.GetCount() == 5);
         REQUIRE(pack.GetReserved() >= 5);
         for (unsigned i = 0; i < 5; ++i)
            REQUIRE(pack[i] == darray3backup[i]);
         
         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("Anyness::TAny::operator << (5 consecutive trivial moves)") (timer meter) {
               some<T> storage(meter.runs());

               meter.measure([&](int i) {
                  return storage[i]
                     << ::std::move(darray2[0])
                     << ::std::move(darray2[1])
                     << ::std::move(darray2[2])
                     << ::std::move(darray2[3])
                     << ::std::move(darray2[4]);
               });
            };

            BENCHMARK_ADVANCED("std::vector::emplace_back(5 consecutive trivial moves)") (timer meter) {
               some<StdT> storage(meter.runs());

               meter.measure([&](int i) {
                  auto& s = storage[i];
                  s.emplace_back(::std::move(darray2[0]));
                  s.emplace_back(::std::move(darray2[1]));
                  s.emplace_back(::std::move(darray2[2]));
                  s.emplace_back(::std::move(darray2[3]));
                  return s.emplace_back(::std::move(darray2[4]));
               });
            };
         #endif
      }

      WHEN("Move more of the same stuff to the front (>>)") {
         E darray3[5] {
            CreateElement<E>(6),
            CreateElement<E>(7),
            CreateElement<E>(8),
            CreateElement<E>(9),
            CreateElement<E>(10)
         };

         const E darray3backup[5] {
            darray3[0],
            darray3[1],
            darray3[2],
            darray3[3],
            darray3[4],
         };

         pack
            >> ::std::move(darray3[0])
            >> ::std::move(darray3[1])
            >> ::std::move(darray3[2])
            >> ::std::move(darray3[3])
            >> ::std::move(darray3[4]);

         CheckState_OwnedFull<E>(pack);

         REQUIRE(pack.GetCount() == 5);
         REQUIRE(pack.GetReserved() >= 5);
         for (unsigned i = 0; i < 5; ++i)
            REQUIRE(pack[i] == darray3backup[4 - i]);
         
         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("Anyness::TAny::operator >> (5 consecutive trivial moves)") (timer meter) {
               some<T> storage(meter.runs());

               meter.measure([&](int i) {
                  return storage[i]
                     >> ::std::move(darray2[0])
                     >> ::std::move(darray2[1])
                     >> ::std::move(darray2[2])
                     >> ::std::move(darray2[3])
                     >> ::std::move(darray2[4]);
               });
            };

            BENCHMARK_ADVANCED("std::vector::emplace_front(5 consecutive trivial moves)") (timer meter) {
               some<StdT> storage(meter.runs());

               meter.measure([&](int i) {
                  auto& s = storage[i];
                  s.emplace_front(::std::move(darray2[0]));
                  s.emplace_front(::std::move(darray2[1]));
                  s.emplace_front(::std::move(darray2[2]));
                  s.emplace_front(::std::move(darray2[3]));
                  return s.emplace_front(::std::move(darray2[4]));
               });
            };
         #endif
      }
      
      WHEN("Insert single item at a specific place by shallow-copy") {
         const auto i666 = CreateElement<E>(666);
         REQUIRE_THROWS(pack.InsertAt(i666, 0));
         CheckState_Default<E>(pack);
      }

      WHEN("Insert multiple items at a specific place by shallow-copy") {
         REQUIRE_THROWS(pack.InsertAt(darray2, darray2 + 5, 0));
         CheckState_Default<E>(pack);
      }

      WHEN("Insert single item at a specific place by move") {
         auto i666 = CreateElement<E>(666);
         REQUIRE_THROWS(pack.InsertAt(::std::move(i666), 0));
         CheckState_Default<E>(pack);
      }

      WHEN("Emplace item at a specific place") {
         auto i666 = CreateElement<E>(666);
         REQUIRE_THROWS(pack.EmplaceAt(0, ::std::move(i666)));
         CheckState_Default<E>(pack);
      }

      WHEN("Emplace item at the front") {
         auto i666 = CreateElement<E>(666);
         const auto i666backup = i666;

         if constexpr (CT::Typed<T>) {
            pack.template Emplace<IndexFront>(::std::move(i666));

            CheckState_OwnedFull<E>(pack);
            REQUIRE(pack.GetCount() == 1);
            REQUIRE(pack.GetReserved() >= 1);
            REQUIRE(pack[0] == i666backup);

            #ifdef LANGULUS_STD_BENCHMARK
               BENCHMARK_ADVANCED("Anyness::TAny::Emplace(single move at the front)") (timer meter) {
                  some<T> storage(meter.runs());
                  for (auto&& o : storage)
                     o << darray1[0] << darray1[1] << darray1[2] << darray1[3] << darray1[4];

                  meter.measure([&](int i) {
                     return storage[i].template Emplace<IndexFront>(::std::move(i666d));
                  });
               };

               BENCHMARK_ADVANCED("std::vector::emplace_front(single move)") (timer meter) {
                  some<StdT> storage(meter.runs());
                  for (auto&& o : storage)
                     o = { darray1[0], darray1[1], darray1[2], darray1[3], darray1[4] };

                  meter.measure([&](int i) {
                     return storage[i].emplace_front(::std::move(i666d));
                  });
               };
            #endif
         }
         else {
            REQUIRE_THROWS(pack.template Emplace<IndexFront>(::std::move(i666)));
            CheckState_Default<E>(pack);
         }
      }
      
      WHEN("Emplace item at the back") {
         auto i666 = CreateElement<E>(666);
         const auto i666backup = i666;

         if constexpr (CT::Typed<T>) {
            pack.template Emplace<IndexBack>(::std::move(i666));

            CheckState_OwnedFull<E>(pack);
            REQUIRE(pack.GetCount() == 1);
            REQUIRE(pack.GetReserved() >= 1);
            REQUIRE(pack[0] == i666backup);

            #ifdef LANGULUS_STD_BENCHMARK
               BENCHMARK_ADVANCED("Anyness::TAny::Emplace(single move at the back)") (timer meter) {
                  some<T> storage(meter.runs());
                  for (auto&& o : storage)
                     o << darray1[0] << darray1[1] << darray1[2] << darray1[3] << darray1[4];

                  meter.measure([&](int i) {
                     return storage[i].template Emplace<IndexBack>(::std::move(i666d));
                  });
               };

               BENCHMARK_ADVANCED("std::vector::emplace_back(single move)") (timer meter) {
                  some<StdT> storage(meter.runs());
                  for (auto&& o : storage)
                     o = { darray1[0], darray1[1], darray1[2], darray1[3], darray1[4] };

                  meter.measure([&](int i) {
                     return storage[i].emplace_back(::std::move(i666d));
                  });
               };
            #endif
         }
         else {
            REQUIRE_THROWS(pack.template Emplace<IndexBack>(::std::move(i666)));
            CheckState_Default<E>(pack);
         }
      }

      WHEN("Removing non-available elements") {
         const auto removed9 = pack.Remove(darray2[3]);

         REQUIRE(removed9 == 0);
         CheckState_Default<E>(pack);
      }    

      WHEN("More capacity is reserved in an empty container") {
         if constexpr (CT::Typed<T>) {
            pack.Reserve(20);

            CheckState_OwnedEmpty<E>(pack);
            REQUIRE(pack.GetCount() == 0);
            REQUIRE(pack.GetReserved() >= 20);
         }
         else {
            REQUIRE_THROWS(pack.Reserve(20));
            CheckState_Default<E>(pack);
         }
      }

      WHEN("Empty pack is cleared") {
         pack.Clear();

         CheckState_Default<E>(pack);
      }

      WHEN("Empty pack is reset") {
         pack.Reset();

         CheckState_Default<E>(pack);
      }

      WHEN("Empty pack is shallow-copied") {
         pack.MakeOr();
         auto copy = pack;

         Helper_TestSame(copy, pack);
         REQUIRE(copy.GetState() == pack.GetState());
         REQUIRE(copy.GetUses() == 0);
      }

      WHEN("Empty pack is cloned") {
         pack.MakeOr();
         T clone = Clone(pack);

         Helper_TestSame(clone, pack);
         REQUIRE(clone.GetState() == pack.GetState());
         REQUIRE(clone.GetUses() == 0);
      }

      WHEN("Empty pack is moved") {
         pack.MakeOr();
         T movable = pack;
         const T moved = ::std::move(movable);

         Helper_TestSame(moved, pack);
         CheckState_Default<E>(movable);
      }

      WHEN("Packs are compared") {
         T another_pack1;
         another_pack1  << CreateElement<E>(1) 
                        << CreateElement<E>(2)
                        << CreateElement<E>(3)
                        << CreateElement<E>(4)
                        << CreateElement<E>(5);

         T another_pack2;
         another_pack2  << CreateElement<E>(2)
                        << CreateElement<E>(2)
                        << CreateElement<E>(3)
                        << CreateElement<E>(4)
                        << CreateElement<E>(5);

         T another_pack3;
         another_pack3  << CreateElement<E>(1)
                        << CreateElement<E>(2)
                        << CreateElement<E>(3)
                        << CreateElement<E>(4)
                        << CreateElement<E>(5)
                        << CreateElement<E>(6);
         T defaulted_pack1;

         TAny<uint> another_pack4;
         another_pack4  << uint(1) << uint(2) << uint(3) << uint(4) << uint(5);

         Any another_pack5;
         another_pack5  << CreateElement<E>(1)
                        << CreateElement<E>(2)
                        << CreateElement<E>(3)
                        << CreateElement<E>(4)
                        << CreateElement<E>(5);

         Any defaulted_pack2;

         REQUIRE(pack != another_pack1);
         REQUIRE(pack != another_pack2);
         REQUIRE(pack != another_pack3);
         //REQUIRE(pack != another_pack4);
         REQUIRE(pack != another_pack5);
         REQUIRE(pack == defaulted_pack1);
         REQUIRE(pack == defaulted_pack2);
      }

      WHEN("A forward value-based search is performed on non-exitent value") {
         const auto found = pack.Find(darray2[2]);

         REQUIRE(found == IndexNone);
         REQUIRE_FALSE(found);
      }

      WHEN("A backward value-based search is performed on non-exitent value") {
         const auto found = pack.template Find<true>(darray2[2]);

         REQUIRE(found == IndexNone);
         REQUIRE_FALSE(found);
      }
      
      WHEN("Merge-copy an element to the back, if not found (<<=)") {
         pack <<= darray2[3];

         CheckState_OwnedFull<E>(pack);

         REQUIRE(pack.GetCount() == 1);
         REQUIRE(pack.GetReserved() >= 1);
         REQUIRE(pack[0] == darray2[3]);
         
         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("Anyness::TAny::operator <<= (merge copy to the back)") (timer meter) {
               some<T> storage(meter.runs());

               meter.measure([&](int i) {
                  return storage[i] <<= darray2[3];
               });
            };

            BENCHMARK_ADVANCED("std::vector::find & push_back (merge copy to the back)") (timer meter) {
               some<StdT> storage(meter.runs());

               meter.measure([&](int i) {
                  auto& s = storage[i];
                  if (std::find(s.begin(), s.end(), darray2[3]) == s.end())
                     s.push_back(darray2[3]);
               });
            };
         #endif
      }

      WHEN("Merge-copy an element to the front, if not found (>>=)") {
         pack >>= darray2[3];

         CheckState_OwnedFull<E>(pack);

         REQUIRE(pack.GetCount() == 1);
         REQUIRE(pack.GetReserved() >= 1);
         REQUIRE(pack[0] == darray2[3]);
         
         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("Anyness::TAny::operator >> (merge copy to the front)") (timer meter) {
               some<T> storage(meter.runs());

               meter.measure([&](int i) {
                  return storage[i] >>= darray2[3];
               });
            };

            BENCHMARK_ADVANCED("std::vector::find & push_front (merge copy to the front)") (timer meter) {
               some<StdT> storage(meter.runs());

               meter.measure([&](int i) {
                  auto& s = storage[i];
                  if (std::find(s.begin(), s.end(), darray2[3]) == s.end())
                     s.push_front(darray2[3]);
               });
            };
         #endif
      }

      WHEN("Merge-move an element to the back, if not found (<<=)") {
         auto moved = darray2[3];
         pack <<= ::std::move(moved);

         CheckState_OwnedFull<E>(pack);

         REQUIRE(pack.GetCount() == 1);
         REQUIRE(pack.GetReserved() >= 1);
         REQUIRE(pack[0] == darray2[3]);
         
         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("Anyness::TAny::operator <<= (merge move to the back)") (timer meter) {
               some<T> storage(meter.runs());

               meter.measure([&](int i) {
                  return storage[i] <<= ::std::move(moved);
               });
            };

            BENCHMARK_ADVANCED("std::vector::find & push_back (merge move to the back)") (timer meter) {
               some<StdT> storage(meter.runs());

               meter.measure([&](int i) {
                  auto& s = storage[i];
                  if (std::find(s.begin(), s.end(), darray2[3]) == s.end())
                     s.push_back(::std::move(moved));
               });
            };
         #endif
      }

      WHEN("Merge-move an element to the front, if not found (>>=)") {
         auto moved = darray2[3];
         pack >>= ::std::move(moved);

         CheckState_OwnedFull<E>(pack);

         REQUIRE(pack.GetCount() == 1);
         REQUIRE(pack.GetReserved() >= 1);
         REQUIRE(pack[0] == darray2[3]);
         
         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("Anyness::TAny::operator >>= (merge move to the front)") (timer meter) {
               some<T> storage(meter.runs());

               meter.measure([&](int i) {
                  return storage[i] >>= ::std::move(moved);
               });
            };

            BENCHMARK_ADVANCED("std::vector::find & push_front (merge move to the front)") (timer meter) {
               some<StdT> storage(meter.runs());

               meter.measure([&](int i) {
                  auto& s = storage[i];
                  if (std::find(s.begin(), s.end(), darray2[3]) == s.end())
                     s.push_front(::std::move(moved));
               });
            };
         #endif
      }

      WHEN("ForEach flat dense element (immutable)") {
         const auto foreachit = const_cast<const T&>(pack).ForEach(
            [&](const int&)    {FAIL();},
            [&](const Trait&)  {FAIL();},
            [&](const Any&)    {FAIL();}
         );

         REQUIRE(0 == foreachit);
      }

      WHEN("ForEach flat dense element (mutable)") {
         const auto foreachit = const_cast<T&>(pack).ForEach(
            [&](int&)         {FAIL(); },
            [&](Trait&)       {FAIL(); },
            [&](Any&)         {FAIL(); }
         );

         REQUIRE(0 == foreachit);
      }

      WHEN("ForEach flat sparse element (immutable)") {
         const auto foreachit = const_cast<const T&>(pack).ForEach(
            [&](const int*)   {FAIL(); },
            [&](const Trait*) {FAIL(); },
            [&](const Any*)   {FAIL(); }
         );

         REQUIRE(0 == foreachit);
      }

      WHEN("ForEach flat sparse element (mutable)") {
         const auto foreachit = const_cast<T&>(pack).ForEach(
            [&](int*)      {FAIL(); },
            [&](Trait*)    {FAIL(); },
            [&](Any*)      {FAIL(); }
         );

         REQUIRE(0 == foreachit);
      }

      WHEN("ForEachRev flat dense element (immutable)") {
         const auto foreachit = const_cast<const T&>(pack).ForEachRev(
            [&](const int&)   {FAIL(); },
            [&](const Trait&) {FAIL(); },
            [&](const Any&)   {FAIL(); }
         );

         REQUIRE(0 == foreachit);
      }

      WHEN("ForEachRev flat dense element (mutable)") {
         const auto foreachit = pack.ForEachRev(
            [&](const int&)   {FAIL(); },
            [&](const Trait&) {FAIL(); },
            [&](const Any&)   {FAIL(); }
         );

         REQUIRE(0 == foreachit);
      }

      WHEN("ForEachRev flat sparse element (immutable)") {
         const auto foreachit = const_cast<const T&>(pack).ForEachRev(
            [&](const int*)   {FAIL(); },
            [&](const Trait*) {FAIL(); },
            [&](const Any*)   {FAIL(); }
         );

         REQUIRE(0 == foreachit);
      }

      WHEN("ForEachRev flat sparse element (mutable)") {
         const auto foreachit = pack.ForEachRev(
            [&](const int*)   {FAIL(); },
            [&](const Trait*) {FAIL(); },
            [&](const Any*)   {FAIL(); }
         );

         REQUIRE(0 == foreachit);
      }
   }

   GIVEN("Container constructed by same container copy") {
      if constexpr (CT::Deep<E> && CT::Typed<T>)
         REQUIRE_THROWS(T {element});
      else {
         const T source {element};
         T pack {source};

         REQUIRE(pack.template As<DenseE>() == denseValue);
         REQUIRE(*pack.template As<DenseE*>() == denseValue);
         REQUIRE(pack.GetUses() == 2);
         CheckState_OwnedFull<E>(pack);
         REQUIRE_THROWS(pack.template As<float>() == 0.0f);
         REQUIRE_THROWS(pack.template As<float*>() == nullptr);

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("construction (single container copy)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               some<uninitialized<T>> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i].construct(source);
               });
            };

            BENCHMARK_ADVANCED("std::vector::construction (single container copy)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               StdT source {1, 555};
               some<uninitialized<StdT>> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i].construct(source);
               });
            };

            BENCHMARK_ADVANCED("std::any::construction (single container copy)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               std::any source {555};
               some<uninitialized<std::any>> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i].construct(source);
               });
            };
         #endif
      }
   }

   GIVEN("Container constructed by value copy") {
      if constexpr (CT::Deep<E> && CT::Typed<T>)
         REQUIRE_THROWS(T {element});
      else {
         T pack {element};

         CheckState_OwnedFull<E>(pack);
         REQUIRE(pack.template As<DenseE>() == denseValue);
         REQUIRE(*pack.template As<DenseE*>() == denseValue);
         REQUIRE(pack.GetUses() == 1);
         REQUIRE_THROWS(pack.template As<float>() == 0.0f);
         REQUIRE_THROWS(pack.template As<float*>() == nullptr);

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("construction (single value copy)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               some<uninitialized<T>> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i].construct(value);
               });
            };

            BENCHMARK_ADVANCED("std::vector::construction (single value copy)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               some<uninitialized<StdT>> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i].construct(1, value);
               });
            };

            BENCHMARK_ADVANCED("std::any::construction (single value copy)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               some<uninitialized<std::any>> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i].construct(value);
               });
            };
         #endif

         WHEN("Assigned compatible value by copy") {
            pack = element;

            CheckState_OwnedFull<E>(pack);
            REQUIRE(&pack.template As<DenseE>() == sparseValue);
            REQUIRE(pack.template As<DenseE>() == denseValue);
            REQUIRE(*pack.template As<DenseE*>() == denseValue);
            REQUIRE(pack.GetUses() == 1);
            REQUIRE(*pack.GetRawSparse() == asbytes(sparseValue));
            IF_LANGULUS_MANAGED_MEMORY(REQUIRE(*pack.GetEntries() == nullptr));
            REQUIRE_THROWS(pack.template As<float>() == 0.0f);
            REQUIRE_THROWS(pack.template As<float*>() == nullptr);

            #ifdef LANGULUS_STD_BENCHMARK
               BENCHMARK_ADVANCED("operator = (single value copy)") (timer meter) {
                  IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
                  some<T> storage(meter.runs(), element);
                  meter.measure([&](int i) {
                     return storage[i] = value;
                  });
               };

               BENCHMARK_ADVANCED("std::vector::operator = (single value copy)") (timer meter) {
                  IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
                  some<StdT> storage(meter.runs(), element);
                  meter.measure([&](int i) {
                     return storage[i] = {value};
                  });
               };

               BENCHMARK_ADVANCED("std::any::operator = (single value copy)") (timer meter) {
                  IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
                  some<std::any> storage(meter.runs(), element);
                  meter.measure([&](int i) {
                     return storage[i] = value;
                  });
               };
            #endif
         }
      
         WHEN("Assigned compatible value by move") {
            auto movable = element;
            pack = ::std::move(movable);

            CheckState_OwnedFull<E>(pack);
            REQUIRE(&pack.template As<DenseE>() == sparseValue);
            REQUIRE(pack.template As<DenseE>() == denseValue);
            REQUIRE(*pack.template As<DenseE*>() == denseValue);
            REQUIRE(pack.GetUses() == 1);
            REQUIRE(*pack.GetRawSparse() == asbytes(sparseValue));
            IF_LANGULUS_MANAGED_MEMORY(REQUIRE(*pack.GetEntries() == nullptr));
            REQUIRE_THROWS(pack.template As<float>() == 0.0f);
            REQUIRE_THROWS(pack.template As<float*>() == nullptr);

            #ifdef LANGULUS_STD_BENCHMARK
               BENCHMARK_ADVANCED("operator = (single value move)") (timer meter) {
                  IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
                  some<T> storage(meter.runs(), element);
                  meter.measure([&](int i) {
                     return storage[i] = ::std::move(value);
                  });
               };

               BENCHMARK_ADVANCED("std::vector::operator = (single value move)") (timer meter) {
                  IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
                  some<StdT> storage(meter.runs(), element);
                  meter.measure([&](int i) {
                     return storage[i] = {::std::move(value)};
                  });
               };

               BENCHMARK_ADVANCED("std::any::operator = (single value move)") (timer meter) {
                  IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
                  some<std::any> storage(meter.runs(), element);
                  meter.measure([&](int i) {
                     return storage[i] = ::std::move(value);
                  });
               };
            #endif
         }

         WHEN("Assigned compatible disowned value") {
            pack = Disown(element);

            CheckState_OwnedFull<E>(pack);
            REQUIRE(&pack.template As<DenseE>() == sparseValue);
            REQUIRE(pack.template As<DenseE>() == denseValue);
            REQUIRE(*pack.template As<DenseE*>() == denseValue);
            REQUIRE(pack.GetUses() == 1);
            REQUIRE(*pack.GetRawSparse() == asbytes(sparseValue));
            IF_LANGULUS_MANAGED_MEMORY(REQUIRE(*pack.GetEntries() == nullptr));
            REQUIRE_THROWS(pack.template As<float>() == 0.0f);
            REQUIRE_THROWS(pack.template As<float*>() == nullptr);

            #ifdef LANGULUS_STD_BENCHMARK
               BENCHMARK_ADVANCED("operator = (single disowned value)") (timer meter) {
                  IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
                  some<T> storage(meter.runs(), element);
                  meter.measure([&](int i) {
                     return storage[i] = Disown(value);
                  });
               };

               BENCHMARK_ADVANCED("std::vector::operator = (single value copy)") (timer meter) {
                  IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
                  some<StdT> storage(meter.runs(), element);
                  meter.measure([&](int i) {
                     return storage[i] = {value};
                  });
               };

               BENCHMARK_ADVANCED("std::any::operator = (single value copy)") (timer meter) {
                  IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
                  some<std::any> storage(meter.runs(), element);
                  meter.measure([&](int i) {
                     return storage[i] = value;
                  });
               };
            #endif
         }
      
         WHEN("Assigned compatible abandoned value") {
            auto movable = element;
            pack = Abandon(movable);

            CheckState_OwnedFull<E>(pack);
            REQUIRE(&pack.template As<DenseE>() == sparseValue);
            REQUIRE(pack.template As<DenseE>() == denseValue);
            REQUIRE(*pack.template As<DenseE*>() == denseValue);
            REQUIRE(pack.GetUses() == 1);
            REQUIRE(*pack.GetRawSparse() == asbytes(sparseValue));
            IF_LANGULUS_MANAGED_MEMORY(REQUIRE(*pack.GetEntries() == nullptr));
            REQUIRE_THROWS(pack.template As<float>() == 0.0f);
            REQUIRE_THROWS(pack.template As<float*>() == nullptr);

            #ifdef LANGULUS_STD_BENCHMARK
               BENCHMARK_ADVANCED("operator = (single abandoned value)") (timer meter) {
                  IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
                  some<T> storage(meter.runs(), element);
                  meter.measure([&](int i) {
                     return storage[i] = Abandon(value);
                  });
               };

               BENCHMARK_ADVANCED("std::vector::operator = (single value move)") (timer meter) {
                  IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
                  some<StdT> storage(meter.runs(), element);
                  meter.measure([&](int i) {
                     return storage[i] = {::std::move(value)};
                  });
               };

               BENCHMARK_ADVANCED("std::any::operator = (single value move)") (timer meter) {
                  IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
                  some<std::any> storage(meter.runs(), element);
                  meter.measure([&](int i) {
                     return storage[i] = ::std::move(value);
                  });
               };
            #endif
         }

         WHEN("Assigned compatible empty self") {
            pack = T {};

            CheckState_Default<E>(pack);

            #ifdef LANGULUS_STD_BENCHMARK
               BENCHMARK_ADVANCED("operator = (self)") (timer meter) {
                  IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
                  some<T> storage(meter.runs(), element);
                  meter.measure([&](int i) {
                     return storage[i] = storage[i];
                  });
               };

               BENCHMARK_ADVANCED("std::vector::operator = (self)") (timer meter) {
                  IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
                  some<StdT> storage(meter.runs(), element);
                  meter.measure([&](int i) {
                     return storage[i] = storage[i];
                  });
               };

               BENCHMARK_ADVANCED("std::any::operator = (self)") (timer meter) {
                  IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
                  some<std::any> storage(meter.runs(), element);
                  meter.measure([&](int i) {
                     return storage[i] = storage[i];
                  });
               };
            #endif
         }

         WHEN("Assigned compatible full self") {
            pack = element;
            pack = pack;

            CheckState_OwnedFull<E>(pack);
            REQUIRE(pack.GetUses() == 1);

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("operator = (self)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               some<T> storage(meter.runs(), element);
               meter.measure([&](int i) {
                  return storage[i] = storage[i];
               });
            };

            BENCHMARK_ADVANCED("std::vector::operator = (self)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               some<StdT> storage(meter.runs(), element);
               meter.measure([&](int i) {
                  return storage[i] = storage[i];
               });
            };

            BENCHMARK_ADVANCED("std::any::operator = (self)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               some<std::any> storage(meter.runs(), element);
               meter.measure([&](int i) {
                  return storage[i] = storage[i];
               });
            };
         #endif
         }
      }
   }

   GIVEN("Container constructed by value move") {
      if constexpr (CT::Deep<E> && CT::Typed<T>) {
         E movable = element;
         REQUIRE_THROWS(T {::std::move(movable)});
         CheckState_OwnedFull<E>(movable);
      }
      else {
         E movable = element;
         T pack {::std::move(movable)};

         CheckState_OwnedFull<E>(pack);
         REQUIRE(&pack.template As<DenseE>() == sparseValue);
         REQUIRE(pack.template As<DenseE>() == denseValue);
         REQUIRE(*pack.template As<DenseE*>() == denseValue);
         REQUIRE(pack.GetUses() == 1);
         REQUIRE(*pack.GetRawSparse() == asbytes(sparseValue));
         IF_LANGULUS_MANAGED_MEMORY(REQUIRE(*pack.GetEntries() == nullptr));
         REQUIRE_THROWS(pack.template As<float>() == 0.0f);
         REQUIRE_THROWS(pack.template As<float*>() == nullptr);

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("construction (single value move)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               some<uninitialized<T>> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i].construct(::std::move(value));
               });
            };

            BENCHMARK_ADVANCED("std::vector::construction (single value move)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               some<uninitialized<StdT>> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i].construct(1, ::std::move(value));
               });
            };

            BENCHMARK_ADVANCED("std::any::construction (single value move)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               some<uninitialized<std::any>> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i].construct(::std::move(value));
               });
            };
         #endif
      }
   }

   GIVEN("Container constructed by disowned value") {
      if constexpr (CT::Deep<E> && CT::Typed<T>)
         REQUIRE_THROWS(T {Disown(element)});
      else {
         T pack {Disown(element)};

         CheckState_OwnedFull<E>(pack);
         REQUIRE(&pack.template As<DenseE>() == sparseValue);
         REQUIRE(pack.template As<DenseE>() == denseValue);
         REQUIRE(*pack.template As<DenseE*>() == denseValue);
         REQUIRE(pack.GetUses() == 1);
         REQUIRE(*pack.GetRawSparse() == asbytes(sparseValue));
         IF_LANGULUS_MANAGED_MEMORY(REQUIRE(*pack.GetEntries() == nullptr));
         REQUIRE_THROWS(pack.template As<float>() == 0.0f);
         REQUIRE_THROWS(pack.template As<float*>() == nullptr);

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("construction (single disowned value)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               some<uninitialized<T>> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i].construct(Disowned(value));
               });
            };

            BENCHMARK_ADVANCED("std::vector::construction (single value copy)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               some<uninitialized<StdT>> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i].construct(1, value);
               });
            };

            BENCHMARK_ADVANCED("std::any::construction (single value copy)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               some<uninitialized<std::any>> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i].construct(value);
               });
            };
         #endif
      }
   }
    
   GIVEN("Container constructed by abandoned value") {
      if constexpr (CT::Deep<E> && CT::Typed<T>) {
         E movable = element;
         REQUIRE_THROWS(T {Abandon(movable)});
      }
      else {
         E movable = element;
         T pack {Abandon(movable)};

         CheckState_OwnedFull<E>(pack);
         REQUIRE(&pack.template As<DenseE>() == sparseValue);
         REQUIRE(pack.template As<DenseE>() == denseValue);
         REQUIRE(*pack.template As<DenseE*>() == denseValue);
         REQUIRE(pack.GetUses() == 1);
         REQUIRE(*pack.GetRawSparse() == asbytes(sparseValue));
         IF_LANGULUS_MANAGED_MEMORY(REQUIRE(*pack.GetEntries() == nullptr));
         REQUIRE_THROWS(pack.template As<float>() == 0.0f);
         REQUIRE_THROWS(pack.template As<float*>() == nullptr);

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("construction (single abandoned value)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               some<uninitialized<T>> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i].construct(Abandon(value));
               });
            };

            BENCHMARK_ADVANCED("std::vector::construction (single value move)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               some<uninitialized<StdT>> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i].construct(1, ::std::move(value));
               });
            };

            BENCHMARK_ADVANCED("std::any::construction (single value move)") (timer meter) {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               some<uninitialized<std::any>> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i].construct(::std::move(value));
               });
            };
         #endif
      }
   }

   GIVEN("Container constructed by static list of exactly the same shallow-copied elements") {
      if constexpr (not CT::Typed<T>) {
         const T pack {element, element};

         CheckState_OwnedFull<E>(pack);
         REQUIRE(pack.GetCount() == 2);
         REQUIRE(pack.GetReserved() >= 2);
         for (auto& e : pack) {
            REQUIRE(e == element);
         }
      }
   }

   GIVEN("Container constructed by static list of somewhat different shallow-copied elements") {
      if constexpr (not CT::Typed<T>) {
         const T pack {denseValue, sparseValue};

         CheckState_OwnedFull<Any>(pack);
         REQUIRE(pack.GetCount() == 2);
         REQUIRE(pack.GetReserved() >= 2);
         REQUIRE(pack[0] == Any {denseValue});
         REQUIRE(pack[1] == Any {sparseValue});
      }
   }

   GIVEN("Container with some items") {
      T pack {};
      pack << darray1[0] << darray1[1] << darray1[2] << darray1[3] << darray1[4];
      const auto previousReserved = pack.GetReserved();
      const auto memory = pack.GetRaw();
      
      WHEN("Given a preinitialized container with 5 elements") {
         REQUIRE(pack.GetCount() == 5);
         REQUIRE(pack.GetReserved() >= 5);
         REQUIRE(pack.template IsExact<E>());
         REQUIRE(pack.GetRaw());
         for (unsigned i = 0; i < pack.GetCount(); ++i)
            REQUIRE(pack[i] == darray1[i]);
         REQUIRE_FALSE(pack.IsConstant());
      }

      WHEN("Shallow-copy more of the same stuff to the back (<<)") {
         pack << darray2[0] << darray2[1] << darray2[2] << darray2[3] << darray2[4];

         REQUIRE(pack.GetCount() == 10);
         REQUIRE(pack.GetReserved() >= 10);
         REQUIRE(pack.template IsExact<E>());

         for (unsigned i = 0; i < 5; ++i)
            REQUIRE(pack[i] == darray1[i]);
         for (unsigned i = 5; i < pack.GetCount(); ++i)
            REQUIRE(pack[i] == darray2[i-5]);

         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            if constexpr (CT::Same<E, int>) {
               REQUIRE(pack.GetRaw() == memory);
            }
         #endif
         
         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("Anyness::TAny::operator << (5 consecutive trivial copies)") (timer meter) {
               some<T> storage(meter.runs());

               meter.measure([&](int i) {
                  return storage[i] << darray2[0] << darray2[1] << darray2[2] << darray2[3] << darray2[4];
               });
            };

            BENCHMARK_ADVANCED("std::vector::push_back(5 consecutive trivial copies)") (timer meter) {
               some<StdT> storage(meter.runs());

               meter.measure([&](int i) {
                  auto& s = storage[i];
                  s.push_back(darray2[0]);
                  s.push_back(darray2[1]);
                  s.push_back(darray2[2]);
                  s.push_back(darray2[3]);
                  return s.push_back(darray2[4]);
               });
            };
         #endif
      }

      WHEN("Shallow-copy more of the same stuff to the front (>>)") {
         pack >> darray2[0] >> darray2[1] >> darray2[2] >> darray2[3] >> darray2[4];

         REQUIRE(pack.GetCount() == 10);
         REQUIRE(pack.GetReserved() >= 10);
         REQUIRE(pack.template IsExact<E>());

         for (unsigned i = 5; i > 0; --i)
            REQUIRE(pack[5 - i] == darray2[i - 1]);
         for (unsigned i = 5; i < pack.GetCount(); ++i)
            REQUIRE(pack[i] == darray1[i-5]);

         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            if constexpr (CT::Same<E, int>) {
               REQUIRE(pack.GetRaw() == memory);
            }
         #endif
         
         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("Anyness::TAny::operator >> (5 consecutive trivial copies)") (timer meter) {
               some<T> storage(meter.runs());

               meter.measure([&](int i) {
                  return storage[i] >> darray2[0] >> darray2[1] >> darray2[2] >> darray2[3] >> darray2[4];
               });
            };

            BENCHMARK_ADVANCED("std::vector::push_front(5 consecutive trivial copies)") (timer meter) {
               some<StdT> storage(meter.runs());

               meter.measure([&](int i) {
                  auto& s = storage[i];
                  s.push_front(darray2[0]);
                  s.push_front(darray2[1]);
                  s.push_front(darray2[2]);
                  s.push_front(darray2[3]);
                  return s.push_front(darray2[4]);
               });
            };
         #endif
      }

      WHEN("Shallow-copy an array to the back") {
         pack.template Insert<IndexBack>(darray2, darray2 + 5);

         REQUIRE(pack.GetCount() == 10);
         REQUIRE(pack.GetReserved() >= 10);
         REQUIRE(pack.template IsExact<E>());

         for (unsigned i = 0; i < 5; ++i)
            REQUIRE(pack[i] == darray1[i]);
         for (unsigned i = 5; i < pack.GetCount(); ++i)
            REQUIRE(pack[i] == darray2[i-5]);

         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            if constexpr (CT::Same<E, int>) {
               REQUIRE(pack.GetRaw() == memory);
            }
         #endif
         
         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("Anyness::TAny::Insert<IndexBack> (5 trivial copies)") (timer meter) {
               some<T> storage(meter.runs());

               meter.measure([&](int i) {
                  return storage[i].template Insert<IndexBack>(darray2, darray2 + 5);
               });
            };

            BENCHMARK_ADVANCED("std::vector::insert to back (5 trivial copies)") (timer meter) {
               some<StdT> storage(meter.runs());

               meter.measure([&](int i) {
                  return storage[i].insert(storage[i].end(), darray2, darray2 + 5);
               });
            };
         #endif
      }

      WHEN("Shallow-copy an array to the front") {
         pack.template Insert<IndexFront>(darray2, darray2 + 5);

         REQUIRE(pack.GetCount() == 10);
         REQUIRE(pack.GetReserved() >= 10);
         REQUIRE(pack.template IsExact<E>());

         for (unsigned i = 0; i < 5; ++i)
            REQUIRE(pack[i] == darray2[i]);
         for (unsigned i = 5; i < pack.GetCount(); ++i)
            REQUIRE(pack[i] == darray1[i-5]);

         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            if constexpr (CT::Same<E, int>) {
               REQUIRE(pack.GetRaw() == memory);
            }
         #endif
         
         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("Anyness::TAny::Insert<IndexFront> (5 trivial copies)") (timer meter) {
               some<T> storage(meter.runs());

               meter.measure([&](int i) {
                  return storage[i].template Insert<IndexFront>(darray2, darray2 + 5);
               });
            };

            BENCHMARK_ADVANCED("std::vector::insert to front (5 trivial copies)") (timer meter) {
               some<StdT> storage(meter.runs());

               meter.measure([&](int i) {
                  return storage[i].insert(storage[i].begin(), darray2, darray2 + 5);
               });
            };
         #endif
      }

      WHEN("Move more of the same stuff to the back (<<)") {
         E darray3[5] {
            CreateElement<E>(6),
            CreateElement<E>(7),
            CreateElement<E>(8),
            CreateElement<E>(9),
            CreateElement<E>(10)
         };

         const E darray3backup[5] {
            darray3[0],
            darray3[1],
            darray3[2],
            darray3[3],
            darray3[4],
         };

         pack
            << ::std::move(darray3[0])
            << ::std::move(darray3[1])
            << ::std::move(darray3[2])
            << ::std::move(darray3[3])
            << ::std::move(darray3[4]);

         REQUIRE(pack.GetCount() == 10);
         REQUIRE(pack.GetReserved() >= 10);
         REQUIRE(pack.template IsExact<E>());

         for (unsigned i = 0; i < 5; ++i)
            REQUIRE(pack[i] == darray1[i]);

         for (unsigned i = 5; i < pack.GetCount(); ++i)
            REQUIRE(pack[i] == darray3backup[i - 5]);

         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            REQUIRE(pack.GetRaw() == memory);
         #endif
         
         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("Anyness::TAny::operator << (5 consecutive trivial moves)") (timer meter) {
               some<T> storage(meter.runs());

               meter.measure([&](int i) {
                  return storage[i]
                     << ::std::move(darray2[0])
                     << ::std::move(darray2[1])
                     << ::std::move(darray2[2])
                     << ::std::move(darray2[3])
                     << ::std::move(darray2[4]);
               });
            };

            BENCHMARK_ADVANCED("std::vector::emplace_back(5 consecutive trivial moves)") (timer meter) {
               some<StdT> storage(meter.runs());

               meter.measure([&](int i) {
                  auto& s = storage[i];
                  s.emplace_back(::std::move(darray2[0]));
                  s.emplace_back(::std::move(darray2[1]));
                  s.emplace_back(::std::move(darray2[2]));
                  s.emplace_back(::std::move(darray2[3]));
                  return s.emplace_back(::std::move(darray2[4]));
               });
            };
         #endif
      }

      WHEN("Move more of the same stuff to the front (>>)") {
         E darray3[5] {
            CreateElement<E>(6),
            CreateElement<E>(7),
            CreateElement<E>(8),
            CreateElement<E>(9),
            CreateElement<E>(10)
         };

         const E darray3backup[5] {
            darray3[0],
            darray3[1],
            darray3[2],
            darray3[3],
            darray3[4],
         };

         pack
            >> ::std::move(darray3[0])
            >> ::std::move(darray3[1])
            >> ::std::move(darray3[2])
            >> ::std::move(darray3[3])
            >> ::std::move(darray3[4]);

         REQUIRE(pack.GetCount() == 10);
         REQUIRE(pack.GetReserved() >= 10);
         REQUIRE(pack.template IsExact<E>());

         for (unsigned i = 5; i > 0; --i)
            REQUIRE(pack[5 - i] == darray3backup[i - 1]);

         for (unsigned i = 5; i < pack.GetCount(); ++i)
            REQUIRE(pack[i] == darray1[i - 5]);

         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            REQUIRE(pack.GetRaw() == memory);
         #endif
         
         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("Anyness::TAny::operator >> (5 consecutive trivial moves)") (timer meter) {
               some<T> storage(meter.runs());

               meter.measure([&](int i) {
                  return storage[i]
                     >> ::std::move(darray2[0])
                     >> ::std::move(darray2[1])
                     >> ::std::move(darray2[2])
                     >> ::std::move(darray2[3])
                     >> ::std::move(darray2[4]);
               });
            };

            BENCHMARK_ADVANCED("std::vector::emplace_front(5 consecutive trivial moves)") (timer meter) {
               some<StdT> storage(meter.runs());

               meter.measure([&](int i) {
                  auto& s = storage[i];
                  s.emplace_front(::std::move(darray2[0]));
                  s.emplace_front(::std::move(darray2[1]));
                  s.emplace_front(::std::move(darray2[2]));
                  s.emplace_front(::std::move(darray2[3]));
                  return s.emplace_front(::std::move(darray2[4]));
               });
            };
         #endif
      }
      
      WHEN("Insert single item at a specific place by shallow-copy") {
         const auto i666 = CreateElement<E>(666);
         pack.InsertAt(i666, 3);

         REQUIRE(pack.GetCount() == 6);
         REQUIRE(pack.GetReserved() >= 6);
         REQUIRE(pack.template IsExact<E>());
         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            REQUIRE(pack.GetRaw() == memory);
         #endif
         REQUIRE(pack[0] == darray1[0]);
         REQUIRE(pack[1] == darray1[1]);
         REQUIRE(pack[2] == darray1[2]);
         REQUIRE(pack[3] == i666);
         REQUIRE(pack[4] == darray1[3]);
         REQUIRE(pack[5] == darray1[4]);

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("Anyness::TAny::InsertAt(single copy in middle)") (timer meter) {
               some<T> storage(meter.runs());
               for (auto&& o : storage)
                  o << darray1[0] << darray1[1] << darray1[2] << darray1[3] << darray1[4];

               meter.measure([&](int i) {
                  return storage[i].InsertAt(&i666, &i666 + 1, 3);
               });
            };

            BENCHMARK_ADVANCED("std::vector::insert(single copy in middle)") (timer meter) {
               some<StdT> storage(meter.runs());
               for (auto&& o : storage)
                  o = { darray1[0], darray1[1], darray1[2], darray1[3], darray1[4] };

               meter.measure([&](int i) {
                  return storage[i].insert(storage[i].begin() + 3, i666d);
               });
            };
         #endif
      }

      WHEN("Insert multiple items at a specific place by shallow-copy") {
         pack.InsertAt(darray2, darray2 + 5, 3);

         REQUIRE(pack.GetCount() == 10);
         REQUIRE(pack.GetReserved() >= 10);
         REQUIRE(pack.template IsExact<E>());
         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            REQUIRE(pack.GetRaw() == memory);
         #endif
         REQUIRE(pack[0] == darray1[0]);
         REQUIRE(pack[1] == darray1[1]);
         REQUIRE(pack[2] == darray1[2]);
         REQUIRE(pack[3] == darray2[0]);
         REQUIRE(pack[4] == darray2[1]);
         REQUIRE(pack[5] == darray2[2]);
         REQUIRE(pack[6] == darray2[3]);
         REQUIRE(pack[7] == darray2[4]);
         REQUIRE(pack[8] == darray1[3]);
         REQUIRE(pack[9] == darray1[4]);

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("Anyness::TAny::InsertAt(5 copies in the middle)") (timer meter) {
               some<T> storage(meter.runs());
               for (auto&& o : storage)
                  o << darray1[0] << darray1[1] << darray1[2] << darray1[3] << darray1[4];

               meter.measure([&](int i) {
                  return storage[i].InsertAt(darray2, darray2 + 5, 3);
               });
            };

            BENCHMARK_ADVANCED("std::vector::insert(5 copies in the middle)") (timer meter) {
               some<StdT> storage(meter.runs());
               for (auto&& o : storage)
                  o = { darray1[0], darray1[1], darray1[2], darray1[3], darray1[4] };

               meter.measure([&](int i) {
                  return storage[i].insert(storage[i].begin() + 3, darray2, darray2+5);
               });
            };
         #endif
      }

      WHEN("Insert single item at a specific place by move") {
         auto i666 = CreateElement<E>(666);
         const auto i666backup = i666;
         pack.InsertAt(::std::move(i666), 3);

         REQUIRE(pack.GetCount() == 6);
         REQUIRE(pack.GetReserved() >= 6);
         REQUIRE(pack.template IsExact<E>());
         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            REQUIRE(pack.GetRaw() == memory);
         #endif
         REQUIRE(pack[0] == darray1[0]);
         REQUIRE(pack[1] == darray1[1]);
         REQUIRE(pack[2] == darray1[2]);
         REQUIRE(pack[3] == i666backup);
         REQUIRE(pack[4] == darray1[3]);
         REQUIRE(pack[5] == darray1[4]);

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("Anyness::TAny::Emplace(single move in middle)") (timer meter) {
               some<T> storage(meter.runs());
               for (auto&& o : storage)
                  o << darray1[0] << darray1[1] << darray1[2] << darray1[3] << darray1[4];

               meter.measure([&](int i) {
                  return storage[i].InsertAt(::std::move(i666d), 3);
               });
            };

            BENCHMARK_ADVANCED("std::vector::insert(single move in middle)") (timer meter) {
               some<StdT> storage(meter.runs());
               for (auto&& o : storage)
                  o = { darray1[0], darray1[1], darray1[2], darray1[3], darray1[4] };

               meter.measure([&](int i) {
                  return storage[i].insert(storage[i].begin() + 3, ::std::move(i666d));
               });
            };
         #endif
      }

      WHEN("Emplace item at a specific place") {
         auto i666 = CreateElement<E>(666);
         const auto i666backup = i666;
         pack.EmplaceAt(3, ::std::move(i666));

         REQUIRE(pack.GetCount() == 6);
         REQUIRE(pack.GetReserved() >= 6);
         REQUIRE(pack.template IsExact<E>());
         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            REQUIRE(pack.GetRaw() == memory);
         #endif
         REQUIRE(pack[0] == darray1[0]);
         REQUIRE(pack[1] == darray1[1]);
         REQUIRE(pack[2] == darray1[2]);
         REQUIRE(pack[3] == i666backup);
         REQUIRE(pack[4] == darray1[3]);
         REQUIRE(pack[5] == darray1[4]);

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("Anyness::TAny::Emplace(single move in middle)") (timer meter) {
               some<T> storage(meter.runs());
               for (auto&& o : storage)
                  o << darray1[0] << darray1[1] << darray1[2] << darray1[3] << darray1[4];

               meter.measure([&](int i) {
                  return storage[i].EmplaceAt(3, ::std::move(i666d));
               });
            };

            BENCHMARK_ADVANCED("std::vector::insert(single move in middle)") (timer meter) {
               some<StdT> storage(meter.runs());
               for (auto&& o : storage)
                  o = { darray1[0], darray1[1], darray1[2], darray1[3], darray1[4] };

               meter.measure([&](int i) {
                  return storage[i].insert(storage[i].begin() + 3, ::std::move(i666d));
               });
            };
         #endif
      }

      WHEN("Emplace item at the front") {
         auto i666 = CreateElement<E>(666);
         const auto i666backup = i666;
         pack.template Emplace<IndexFront>(::std::move(i666));

         REQUIRE(pack.GetCount() == 6);
         REQUIRE(pack.GetReserved() >= 6);
         REQUIRE(pack.template IsExact<E>());
         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            REQUIRE(pack.GetRaw() == memory);
         #endif
         REQUIRE(pack[0] == i666backup);
         REQUIRE(pack[1] == darray1[0]);
         REQUIRE(pack[2] == darray1[1]);
         REQUIRE(pack[3] == darray1[2]);
         REQUIRE(pack[4] == darray1[3]);
         REQUIRE(pack[5] == darray1[4]);

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("Anyness::TAny::Emplace(single move at the front)") (timer meter) {
               some<T> storage(meter.runs());
               for (auto&& o : storage)
                  o << darray1[0] << darray1[1] << darray1[2] << darray1[3] << darray1[4];

               meter.measure([&](int i) {
                  return storage[i].template Emplace<IndexFront>(::std::move(i666d));
               });
            };

            BENCHMARK_ADVANCED("std::vector::emplace_front(single move)") (timer meter) {
               some<StdT> storage(meter.runs());
               for (auto&& o : storage)
                  o = { darray1[0], darray1[1], darray1[2], darray1[3], darray1[4] };

               meter.measure([&](int i) {
                  return storage[i].emplace_front(::std::move(i666d));
               });
            };
         #endif
      }
      
      WHEN("Emplace item at the back") {
         auto i666 = CreateElement<E>(666);
         const auto i666backup = i666;
         pack.template Emplace<IndexBack>(::std::move(i666));

         REQUIRE(pack.GetCount() == 6);
         REQUIRE(pack.GetReserved() >= 6);
         REQUIRE(pack.template IsExact<E>());
         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            REQUIRE(pack.GetRaw() == memory);
         #endif
         REQUIRE(pack[0] == darray1[0]);
         REQUIRE(pack[1] == darray1[1]);
         REQUIRE(pack[2] == darray1[2]);
         REQUIRE(pack[3] == darray1[3]);
         REQUIRE(pack[4] == darray1[4]);
         REQUIRE(pack[5] == i666backup);

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("Anyness::TAny::Emplace(single move at the back)") (timer meter) {
               some<T> storage(meter.runs());
               for (auto&& o : storage)
                  o << darray1[0] << darray1[1] << darray1[2] << darray1[3] << darray1[4];

               meter.measure([&](int i) {
                  return storage[i].template Emplace<IndexBack>(::std::move(i666d));
               });
            };

            BENCHMARK_ADVANCED("std::vector::emplace_back(single move)") (timer meter) {
               some<StdT> storage(meter.runs());
               for (auto&& o : storage)
                  o = { darray1[0], darray1[1], darray1[2], darray1[3], darray1[4] };

               meter.measure([&](int i) {
                  return storage[i].emplace_back(::std::move(i666d));
               });
            };
         #endif
      }

      WHEN("The size is reduced by finding and removing elements, but reserved memory should remain the same on shrinking") {
         const auto removed2 = pack.Remove(darray1[1]);
         const auto removed4 = pack.Remove(darray1[3]);

         REQUIRE(removed2 == 1);
         REQUIRE(removed4 == 1);
         REQUIRE(pack[0] == darray1[0]);
         REQUIRE(pack[1] == darray1[2]);
         REQUIRE(pack[2] == darray1[4]);
         REQUIRE_THROWS(pack[3] == CreateElement<E>(666));
         REQUIRE(pack.GetCount() == 3);
         REQUIRE(pack.GetReserved() >= 5);
         REQUIRE(pack.GetRaw() == memory);

         #ifdef LANGULUS_STD_BENCHMARK // Last result: 2:1 performance - needs more optimizations in Index handling
            BENCHMARK_ADVANCED("Anyness::TAny::Remove(single element by value)") (timer meter) {
               some<T> storage(meter.runs());
               for (auto&& o : storage)
                  o << darray1[0] << darray1[1] << darray1[2] << darray1[3] << darray1[4];

               meter.measure([&](int i) {
                  return storage[i].Remove(2);
               });
            };

            BENCHMARK_ADVANCED("Anyness::vector::erase-remove(single element by value)") (timer meter) {
               some<StdT> storage(meter.runs());
               for (auto&& o : storage)
                  o = { darray1[0], darray1[1], darray1[2], darray1[3], darray1[4] };

               meter.measure([&](int i) {
                  // Erase-remove idiom											
                  return storage[i].erase(std::remove(storage[i].begin(), storage[i].end(), 2), storage[i].end());
               });
            };
         #endif
      }

      WHEN("Removing non-available elements") {
         const auto removed9 = pack.Remove(darray2[3]);

         REQUIRE(removed9 == 0);
         REQUIRE(pack[0] == darray1[0]);
         REQUIRE(pack[1] == darray1[1]);
         REQUIRE(pack[2] == darray1[2]);
         REQUIRE(pack[3] == darray1[3]);
         REQUIRE(pack[4] == darray1[4]);
         REQUIRE(pack.GetCount() == 5);
         REQUIRE(pack.GetReserved() >= 5);
         REQUIRE(pack.GetRaw() == memory);
      }    

      WHEN("More capacity is reserved") {
         pack.Reserve(20);

         REQUIRE(pack.GetCount() == 5);
         REQUIRE(pack.GetReserved() >= 20);
         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            if constexpr (CT::POD<E>) {
               // Test works only for POD types, because containers shift entries around
               REQUIRE(pack.GetRaw() == memory);
            }
         #endif
      }

      WHEN("Less capacity is reserved") {
         pack.Reserve(2);

         REQUIRE(pack.GetCount() == 2);
         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            REQUIRE(pack.GetReserved() <= previousReserved);
         #else
            REQUIRE(pack.GetReserved() == previousReserved);
         #endif
         REQUIRE(pack.GetRaw() == memory);
      }

      WHEN("Pack is cleared") {
         pack.Clear();

         REQUIRE(pack.GetCount() == 0);
         REQUIRE(pack.GetReserved() == previousReserved);
         REQUIRE(pack.GetRaw() == memory);
         REQUIRE(pack.template IsExact<E>());
      }

      WHEN("Pack is reset") {
         pack.Reset();

         REQUIRE(pack.GetCount() == 0);
         REQUIRE(pack.GetReserved() == 0);
         REQUIRE(pack.GetRaw() == nullptr);
         REQUIRE(pack.template IsExact<E>() == CT::Typed<T>);
      }

      #if LANGULUS_FEATURE(MANAGED_MEMORY)
         if constexpr (CT::Same<E, int>) {
            // Works only if E doesn't move entries around
            WHEN("Pack is reset, then immediately allocated again") {
               IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());
               pack.Reset();
               pack  << CreateElement<E>(6) 
                     << CreateElement<E>(7) 
                     << CreateElement<E>(8) 
                     << CreateElement<E>(9) 
                     << CreateElement<E>(10);
               REQUIRE(pack.GetRaw() == memory);
            }
         }
      #endif

      WHEN("Pack is shallow-copied") {
         pack.MakeOr();
         auto copy = pack;

         REQUIRE(copy.GetRaw() == pack.GetRaw());
         REQUIRE(copy.GetCount() == pack.GetCount());
         REQUIRE(copy.GetReserved() == pack.GetReserved());
         REQUIRE(copy.GetState() == pack.GetState());
         REQUIRE(copy.GetType() == pack.GetType());
         REQUIRE(copy.GetUses() == 2);
      }

      WHEN("Pack is cloned") {
         pack.MakeOr();
         T clone = Langulus::Clone(pack);

         REQUIRE(clone.GetRaw() != pack.GetRaw());
         REQUIRE(clone.GetCount() == pack.GetCount());
         REQUIRE(clone.GetReserved() >= clone.GetCount());
         REQUIRE(clone.GetState() == pack.GetState());
         REQUIRE(clone.GetType() == pack.GetType());
         REQUIRE(clone.GetUses() == 1);
         REQUIRE(pack.GetUses() == 1);

         for (unsigned i = 0; i < 5; ++i) {
            REQUIRE(pack[i] == darray1[i]);
            REQUIRE(clone[i] != darray1[i]);
            if constexpr (CT::Typed<T>)
               REQUIRE(DenseCast(clone[i]) == DenseCast(darray1[i]));
            else
               REQUIRE(DenseCast(clone[i].template Get<E>()) == DenseCast(darray1[i]));
         }
      }

      WHEN("Pack is moved") {
         T movable = pack;
         movable.MakeOr();
         const T moved = ::std::move(movable);

         REQUIRE(movable.GetRaw() == nullptr);
         REQUIRE(movable.GetCount() == 0);
         REQUIRE(movable.GetReserved() == 0);
         REQUIRE(movable.IsTypeConstrained() == CT::Typed<T>);
         REQUIRE(pack.GetRaw() == moved.GetRaw());
         REQUIRE(pack.GetCount() == moved.GetCount());
         REQUIRE(pack.GetReserved() == moved.GetReserved());
         REQUIRE(pack.GetState() + DataState::Or == moved.GetState());
         REQUIRE(pack.GetType() == moved.GetType());
      }

      WHEN("Packs are compared") {
         T another_pack1;
         another_pack1 << CreateElement<E>(1)
                       << CreateElement<E>(2)
                       << CreateElement<E>(3)
                       << CreateElement<E>(4)
                       << CreateElement<E>(5);
         T another_pack2;
         another_pack2 << CreateElement<E>(2)
                       << CreateElement<E>(2)
                       << CreateElement<E>(3)
                       << CreateElement<E>(4)
                       << CreateElement<E>(5);
         T another_pack3;
         another_pack3 << CreateElement<E>(1)
                       << CreateElement<E>(2)
                       << CreateElement<E>(3)
                       << CreateElement<E>(4)
                       << CreateElement<E>(5)
                       << CreateElement<E>(6);
         TAny<uint> another_pack4;
         another_pack4 << uint(1) << uint(2) << uint(3) << uint(4) << uint(5);
         Any another_pack5;
         another_pack5 << CreateElement<E>(1)
                       << CreateElement<E>(2)
                       << CreateElement<E>(3)
                       << CreateElement<E>(4)
                       << CreateElement<E>(5);

         REQUIRE(pack != another_pack1);
         REQUIRE(pack != another_pack2);
         REQUIRE(pack != another_pack3);
         //REQUIRE(pack != another_pack4);
         REQUIRE(pack != another_pack5);
      }

      WHEN("A forward value-based search is performed on existent value") {
         const auto found = pack.Find(darray1[2]);

         REQUIRE(found);
         REQUIRE(found == 2);
      }

      WHEN("A forward value-based search is performed on non-exitent value") {
         const auto found = pack.Find(darray2[2]);

         REQUIRE(found == IndexNone);
         REQUIRE_FALSE(found);
      }

      WHEN("A backward value-based search is performed on existent value") {
         const auto found = pack.template Find<true>(darray1[2]);

         REQUIRE(found);
         REQUIRE(found == 2);
      }

      WHEN("A backward value-based search is performed on non-exitent value") {
         const auto found = pack.template Find<true>(darray2[2]);

         REQUIRE(found == IndexNone);
         REQUIRE_FALSE(found);
      }
      
      WHEN("Merge-copy an element to the back, if not found (<<=)") {
         pack <<= darray2[3];

         REQUIRE(pack.GetCount() == 6);
         REQUIRE(pack.GetReserved() >= 6);
         REQUIRE(pack.template IsExact<E>());
         for (unsigned i = 0; i < 5; ++i)
            REQUIRE(pack[i] == darray1[i]);
         REQUIRE(pack[5] == darray2[3]);

         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            if constexpr (CT::Same<E, int>) {
               REQUIRE(pack.GetRaw() == memory);
            }
         #endif
         
         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("Anyness::TAny::operator <<= (merge copy to the back)") (timer meter) {
               some<T> storage(meter.runs());

               meter.measure([&](int i) {
                  return storage[i] <<= darray2[3];
               });
            };

            BENCHMARK_ADVANCED("std::vector::find & push_back (merge copy to the back)") (timer meter) {
               some<StdT> storage(meter.runs());

               meter.measure([&](int i) {
                  auto& s = storage[i];
                  if (std::find(s.begin(), s.end(), darray2[3]) == s.end())
                     s.push_back(darray2[3]);
               });
            };
         #endif
      }

      WHEN("Merge-copy an element to the front, if not found (>>=)") {
         pack >>= darray2[3];

         REQUIRE(pack.GetCount() == 6);
         REQUIRE(pack.GetReserved() >= 6);
         REQUIRE(pack.template IsExact<E>());
         REQUIRE(pack[0] == darray2[3]);
         for (unsigned i = 1; i < 6; ++i)
            REQUIRE(pack[i] == darray1[i-1]);

         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            if constexpr (CT::Same<E, int>) {
               REQUIRE(pack.GetRaw() == memory);
            }
         #endif
         
         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("Anyness::TAny::operator >> (merge copy to the front)") (timer meter) {
               some<T> storage(meter.runs());

               meter.measure([&](int i) {
                  return storage[i] >>= darray2[3];
               });
            };

            BENCHMARK_ADVANCED("std::vector::find & push_front (merge copy to the front)") (timer meter) {
               some<StdT> storage(meter.runs());

               meter.measure([&](int i) {
                  auto& s = storage[i];
                  if (std::find(s.begin(), s.end(), darray2[3]) == s.end())
                     s.push_front(darray2[3]);
               });
            };
         #endif
      }

      WHEN("Merge-move an element to the back, if not found (<<=)") {
         auto moved = darray2[3];
         pack <<= ::std::move(moved);

         REQUIRE(pack.GetCount() == 6);
         REQUIRE(pack.GetReserved() >= 6);
         REQUIRE(pack.template IsExact<E>());
         for (unsigned i = 0; i < 5; ++i)
            REQUIRE(pack[i] == darray1[i]);
         REQUIRE(pack[5] == darray2[3]);

         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            if constexpr (CT::Same<E, int>) {
               REQUIRE(pack.GetRaw() == memory);
            }
         #endif
         
         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("Anyness::TAny::operator <<= (merge move to the back)") (timer meter) {
               some<T> storage(meter.runs());

               meter.measure([&](int i) {
                  return storage[i] <<= ::std::move(moved);
               });
            };

            BENCHMARK_ADVANCED("std::vector::find & push_back (merge move to the back)") (timer meter) {
               some<StdT> storage(meter.runs());

               meter.measure([&](int i) {
                  auto& s = storage[i];
                  if (std::find(s.begin(), s.end(), darray2[3]) == s.end())
                     s.push_back(::std::move(moved));
               });
            };
         #endif
      }

      WHEN("Merge-move an element to the front, if not found (>>=)") {
         auto moved = darray2[3];
         pack >>= ::std::move(moved);

         REQUIRE(pack.GetCount() == 6);
         REQUIRE(pack.GetReserved() >= 6);
         REQUIRE(pack.template IsExact<E>());
         REQUIRE(pack[0] == darray2[3]);
         for (unsigned i = 1; i < 6; ++i)
            REQUIRE(pack[i] == darray1[i-1]);

         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            if constexpr (CT::Same<E, int>) {
               REQUIRE(pack.GetRaw() == memory);
            }
         #endif
         
         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("Anyness::TAny::operator >>= (merge move to the front)") (timer meter) {
               some<T> storage(meter.runs());

               meter.measure([&](int i) {
                  return storage[i] >>= ::std::move(moved);
               });
            };

            BENCHMARK_ADVANCED("std::vector::find & push_front (merge move to the front)") (timer meter) {
               some<StdT> storage(meter.runs());

               meter.measure([&](int i) {
                  auto& s = storage[i];
                  if (std::find(s.begin(), s.end(), darray2[3]) == s.end())
                     s.push_front(::std::move(moved));
               });
            };
         #endif
      }

      WHEN("ForEach flat dense element (immutable)") {
         int it = 0;
         const auto foreachit = const_cast<const T&>(pack).ForEach(
            [&](const int& i) {
               REQUIRE(i == it + 1);
               ++it;
            },
            [&](const Trait& i) {
               REQUIRE(i == it + 1);
               ++it;
            },
            [&](const Any& i) {
               const auto temp = CreateElement<DenseE>(it + 1);
               REQUIRE(i == static_cast<const Any&>(temp));
               ++it;
            }
         );

         REQUIRE(static_cast<unsigned>(it) == foreachit);
         if constexpr (CT::Text<E>)
            REQUIRE(it == 0);
         else
            REQUIRE(static_cast<unsigned>(it) == pack.GetCount());
      }

      WHEN("ForEach flat dense element (mutable)") {
         int it = 0;
         const auto foreachit = const_cast<T&>(pack).ForEach(
            [&](int& i) {
               REQUIRE(i == it + 1);
               ++it;
            },
            [&](const Trait& i) {
               REQUIRE(i == it + 1);
               ++it;
            },
            [&](const Any& i) {
               const auto temp = CreateElement<DenseE>(it + 1);
               REQUIRE(i == static_cast<const Any&>(temp));
               ++it;
            }
         );

         REQUIRE(static_cast<unsigned>(it) == foreachit);
         if constexpr (CT::Same<E, Text>)
            REQUIRE(it == 0);
         else
            REQUIRE(static_cast<unsigned>(it) == pack.GetCount());
      }

      WHEN("ForEach flat sparse element (immutable)") {
         int it = 0;
         const auto foreachit = const_cast<const T&>(pack).ForEach(
            [&](const int* i) {
               REQUIRE(*i == it + 1);
               ++it;
            },
            [&](const Trait* i) {
               REQUIRE(*i == it + 1);
               ++it;
            },
            [&](const Any* i) {
               const auto temp = CreateElement<DenseE>(it + 1);
               REQUIRE(*i == static_cast<const Any&>(temp));
               ++it;
            }
         );

         REQUIRE(static_cast<unsigned>(it) == foreachit);
         if constexpr (CT::Same<E, Text>)
            REQUIRE(it == 0);
         else
            REQUIRE(static_cast<unsigned>(it) == pack.GetCount());
      }

      WHEN("ForEach flat sparse element (mutable)") {
         int it = 0;
         const auto foreachit = const_cast<T&>(pack).ForEach(
            [&](int* i) {
               REQUIRE(*i == it + 1);
               ++it;
            },
            [&](const Trait* i) {
               REQUIRE(*i == it + 1);
               ++it;
            },
            [&](const Any* i) {
               const auto temp = CreateElement<DenseE>(it + 1);
               REQUIRE(*i == static_cast<const Any&>(temp));
               ++it;
            }
         );

         REQUIRE(static_cast<unsigned>(it) == foreachit);
         if constexpr (CT::Same<E, Text>)
            REQUIRE(it == 0);
         else
            REQUIRE(static_cast<unsigned>(it) == pack.GetCount());
      }

      WHEN("ForEachRev flat dense element (immutable)") {
         int it = 0;
         const auto foreachit = const_cast<const T&>(pack).template ForEach<true>(
            [&](const int& i) {
               REQUIRE(i == 5 - it);
               ++it;
            },
            [&](const Trait& i) {
               REQUIRE(i == 5 - it);
               ++it;
            },
            [&](const Any& i) {
               const auto temp = CreateElement<DenseE>(5 - it);
               REQUIRE(i == static_cast<const Any&>(temp));
               ++it;
            }
         );

         REQUIRE(static_cast<unsigned>(it) == foreachit);
         if constexpr (CT::Same<E, Text>)
            REQUIRE(it == 0);
         else
            REQUIRE(static_cast<unsigned>(it) == pack.GetCount());
      }

      WHEN("ForEachRev flat dense element (mutable)") {
         int it = 0;
         const auto foreachit = pack.template ForEach<true>(
            [&](int& i) {
               REQUIRE(i == 5 - it);
               ++it;
            },
            [&](const Trait& i) {
               REQUIRE(i == 5 - it);
               ++it;
            },
            [&](const Any& i) {
               const auto temp = CreateElement<DenseE>(5 - it);
               REQUIRE(i == static_cast<const Any&>(temp));
               ++it;
            }
         );

         REQUIRE(static_cast<unsigned>(it) == foreachit);
         if constexpr (CT::Same<E, Text>)
            REQUIRE(it == 0);
         else
            REQUIRE(static_cast<unsigned>(it) == pack.GetCount());
      }

      WHEN("ForEachRev flat sparse element (immutable)") {
         int it = 0;
         const auto foreachit = const_cast<const T&>(pack).template ForEach<true>(
            [&](const int* i) {
               REQUIRE(*i == 5 - it);
               ++it;
            },
            [&](const Trait* i) {
               REQUIRE(*i == 5 - it);
               ++it;
            },
            [&](const Any* i) {
               const auto temp = CreateElement<DenseE>(5 - it);
               REQUIRE(*i == static_cast<const Any&>(temp));
               ++it;
            }
         );

         REQUIRE(static_cast<unsigned>(it) == foreachit);
         if constexpr (CT::Same<E, Text>)
            REQUIRE(it == 0);
         else
            REQUIRE(static_cast<unsigned>(it) == pack.GetCount());
      }

      WHEN("ForEachRev flat sparse element (mutable)") {
         int it = 0;
         const auto foreachit = pack.template ForEach<true>(
            [&](int* i) {
               REQUIRE(*i == 5 - it);
               ++it;
            },
            [&](const Trait* i) {
               REQUIRE(*i == 5 - it);
               ++it;
            },
            [&](const Any* i) {
               const auto temp = CreateElement<DenseE>(5 - it);
               REQUIRE(*i == static_cast<const Any&>(temp));
               ++it;
            }
         );

         REQUIRE(static_cast<unsigned>(it) == foreachit);
         if constexpr (CT::Same<E, Text>)
            REQUIRE(it == 0);
         else
            REQUIRE(static_cast<unsigned>(it) == pack.GetCount());
      }
   }

   GIVEN("Two containers with some items") {
      IF_LANGULUS_MANAGED_MEMORY(Allocator::CollectGarbage());

      T pack1 {darray1[0], darray1[1], darray1[2], darray1[3], darray1[4]};
      T pack2 {darray2[0], darray2[1], darray2[2], darray2[3], darray2[4]};
      const T memory1 = pack1;
      const T memory2 = pack2;

      WHEN("Copy-assign pack1 in pack2") {
         pack2 = pack1;

         REQUIRE(pack1.GetUses() == 3);
         REQUIRE(pack2.GetUses() == 3);
         REQUIRE(memory2.GetUses() == 1);
         REQUIRE(pack1 == pack2);
         REQUIRE(pack2 == memory1);
         REQUIRE(pack2 != memory2);
         for (int i = 0; i < 5; ++i)
            REQUIRE(pack2[i] == darray1[i]);
      }

      WHEN("Move-assign pack1 in pack2") {
         auto movable = pack1;
         pack2 = ::std::move(movable);

         REQUIRE(pack1.GetUses() == 3);
         REQUIRE(pack2.GetUses() == 3);
         REQUIRE(memory2.GetUses() == 1);
         REQUIRE(pack1 == pack2);
         REQUIRE(movable != pack1);
         REQUIRE(movable == T {});
      }

      WHEN("Disown-assign pack1 in pack2") {
         pack2 = Disown(pack1);

         REQUIRE(pack1.GetUses() == 2);
         REQUIRE(pack2.GetUses() == 0);
         REQUIRE(memory2.GetUses() == 1);
         REQUIRE(pack1 == pack2);
         REQUIRE(pack2 == memory1);
         REQUIRE(pack2 != memory2);
         REQUIRE(pack2.mEntry == nullptr);
         for (int i = 0; i < 5; ++i)
            REQUIRE(pack2[i] == darray1[i]);
      }

      WHEN("Abandon-assign pack1 in pack2") {
         auto movable = pack1;
         pack2 = Abandon(movable);

         REQUIRE(pack1.GetUses() == 3);
         REQUIRE(pack2.GetUses() == 3);
         REQUIRE(memory2.GetUses() == 1);
         REQUIRE(pack1 == pack2);
         REQUIRE(movable.mEntry == nullptr);
      }

      WHEN("Copy-assign pack1 in pack2, then reset pack1") {
         pack2 = pack1;
         pack1.Reset();

         REQUIRE_FALSE(pack1.HasAuthority());
         REQUIRE(pack2.GetUses() == 2);
         REQUIRE_FALSE(pack1.GetRaw());
         REQUIRE(pack1.GetReserved() == 0);
         REQUIRE(pack2 == memory1);
      }

      WHEN("Clone-assign pack1 in pack2") {
         pack2 = Langulus::Clone(pack1);

         REQUIRE(pack1.GetUses() == 2);
         REQUIRE(pack2.GetUses() == 1);
         REQUIRE(pack1 != pack2);
         REQUIRE(pack2 != memory1);
         REQUIRE(pack2 != memory2);
      }

      WHEN("Clone-assign pack1 in pack2, then reset pack1") {
         pack2 = Langulus::Clone(pack1);
         const T memory3 = pack2;
         pack1.Reset();

         REQUIRE_FALSE(pack1.HasAuthority());
         REQUIRE(pack2.GetUses() == 2);
         REQUIRE(memory3.GetUses() == 2);
      }

      WHEN("Concatenate both packs to a third pack") {
         const auto pack3 = pack1 + pack2;

         for (int i = 0; i < 5; ++i)
            REQUIRE(pack3[i] == darray1[i]);
         for (int i = 5; i < 10; ++i)
            REQUIRE(pack3[i] == darray2[i - 5]);
      }
   }

   delete element;
}
