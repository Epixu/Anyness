///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#include "TestAnyCommon.hpp"


TEMPLATE_TEST_CASE("Dense Any/TAny", "[any]",
(Types<TAny<Any>, Any>),
   (Types<TAny<int>, int>),
   
   (Types<TAny<Text>, Text>),

   (Types<Any, int>),
   (Types<Any, Any>),
   (Types<Any, Text>)
) {
   static Allocator::State memoryState;
   using T = typename TestType::First;
   using E = typename TestType::Second;

   if constexpr (CT::Untyped<T>) {
      // All type-erased containers should have all intent              
      // constructors and assigners available, and errors will instead  
      // be thrown as exceptions at runtime                             
      static_assert(CT::CopyConstructible<T>);
      static_assert(CT::ReferConstructible<T>);
      static_assert(CT::AbandonConstructible<T>);
      static_assert(CT::MoveConstructible<T>);
      static_assert(CT::CloneConstructible<T>);
      static_assert(CT::DisownConstructible<T>);

      static_assert(CT::CopyAssignable<T>);
      static_assert(CT::ReferAssignable<T>);
      static_assert(CT::AbandonAssignable<T>);
      static_assert(CT::MoveAssignable<T>);
      static_assert(CT::CloneAssignable<T>);
      static_assert(CT::DisownAssignable<T>);
   }
   else {
      // Statically-typed containers behave the same as their inner     
      // type                                                           
      static_assert(CT::CopyConstructible<T>    == CT::CopyConstructible<E>);
      static_assert(CT::ReferConstructible<T>   == CT::ReferConstructible<E>);
      static_assert(CT::AbandonConstructible<T> == CT::AbandonConstructible<E>);
      static_assert(CT::MoveConstructible<T>    == CT::MoveConstructible<E>);
      static_assert(CT::CloneConstructible<T>   == CT::CloneConstructible<E>);
      static_assert(CT::DisownConstructible<T>  == CT::DisownConstructible<E>);

      static_assert(CT::CopyAssignable<T>       == CT::CopyAssignable<E>);
      static_assert(CT::ReferAssignable<T>      == CT::ReferAssignable<E>);
      static_assert(CT::AbandonAssignable<T>    == CT::AbandonAssignable<E>);
      static_assert(CT::MoveAssignable<T>       == CT::MoveAssignable<E>);
      static_assert(CT::CloneAssignable<T>      == CT::CloneAssignable<E>);
      static_assert(CT::DisownAssignable<T>     == CT::DisownAssignable<E>);      
   }
   
   static_assert(not requires (T pack, E item) { pack.operator +   (item); });
   static_assert(not requires (T pack, E item) { pack.operator +=  (item); });
   static_assert(not requires (T pack, E item) { pack.operator <<  (item); });
   static_assert(not requires (T pack, E item) { pack.operator >>  (item); });
   static_assert(not requires (T pack, E item) { pack.operator <<= (item); });
   static_assert(not requires (T pack, E item) { pack.operator >>= (item); });
   static_assert(not requires (T pack, E item) { pack.InsertAt (Index::Back, item); });
   static_assert(not requires (T pack, E item) { pack.EmplaceAt(Index::Back, item); });
   static_assert(not requires (T pack, E item) { pack.Remove(item); });
   static_assert(not requires (T pack, E item) { pack.RemoveAt(Index::Front); });
   static_assert(not requires (T pack, E item) { pack.Reserve(20); });
   static_assert(not requires (T pack, E item) { pack.EnableOr(); });
   static_assert(not requires (T pack, E item) { pack.IsOr(); });
   static_assert(not requires (T pack, E item) { pack.Find(item); });
   static_assert(not requires (T pack, E item) { pack.ForEach([](const int&){}); });
   static_assert(not requires (T pack, E item) { pack.ForEachRev([](const int&){}); });
      
   GIVEN("Default-constructed container") {
      const ScopedElement<E> element {555};
      T pack;

      Any_CheckState_Default<E>(pack);
      
      #ifdef LANGULUS_STD_BENCHMARK
         BENCHMARK_ADVANCED("default construction") (timer meter) {
            some<uninitialized<T>> storage(meter.runs());
            meter.measure([&](int i) {
               return storage[i].construct();
            });
         };

         BENCHMARK_ADVANCED("std::vector::default construction") (timer meter) {
            some<uninitialized<StdT>> storage(meter.runs());
            meter.measure([&](int i) {
               return storage[i].construct();
            });
         };

         BENCHMARK_ADVANCED("std::any::default construction") (timer meter) {
            some<uninitialized<std::any>> storage(meter.runs());
            meter.measure([&](int i) {
               return storage[i].construct();
            });
         };
      #endif

      WHEN("Assigned value by copy") {
         pack.Assign(*element);
         
         if constexpr (CT::Typed<T> and CT::Deep<E> and LANGULUS(SAFE))
            REQUIRE_THROWS(pack = *element);

         if constexpr (CT::Flat<E>) {
            Any_CheckState_OwnedFull<E>(pack);

            REQUIRE(pack.template As<E>() == *element);
            REQUIRE(*pack.template As<E*>() == *element);
            REQUIRE(pack.GetUses() == 1);
         }
         else if constexpr (CT::Same<E, T>) {
            Any_Helper_TestSame(pack, *element);
            
            REQUIRE(pack.GetUses() == element->GetUses());
            REQUIRE(pack.GetUses() == 2);
            REQUIRE(pack.IsStatic() == element->IsStatic());
            REQUIRE(pack.GetAllocation() == element->GetAllocation());
         }

         if constexpr (not CT::Typed<T>) {
            REQUIRE_THROWS(pack.template As<float>() == 0.0f);
            REQUIRE_THROWS(pack.template As<float*>() == nullptr);
         }

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("operator = (single value copy)") (timer meter) {
               some<T> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = value;
               });
            };

            BENCHMARK_ADVANCED("std::vector::operator = (single value copy)") (timer meter) {
               some<StdT> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = {value};
               });
            };

            BENCHMARK_ADVANCED("std::any::operator = (single value copy)") (timer meter) {
               some<std::any> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = value;
               });
            };
         #endif
      }
      
      WHEN("Assigned value by move") {
         auto movable = *element;
         pack.Assign(::std::move(movable));

         if constexpr (CT::Typed<T> and CT::Deep<E> and LANGULUS(SAFE))
            REQUIRE_THROWS(pack = ::std::move(movable));

         if constexpr (CT::Container<E>)
            Any_CheckState_Default<TypeOf<E>>(movable);

         if constexpr (CT::Flat<E>) {
            Any_CheckState_OwnedFull<E>(pack);

            REQUIRE(pack.template As<E>() == *element);
            REQUIRE(*pack.template As<E*>() == *element);
            REQUIRE(pack.GetUses() == 1);
         }
         else if constexpr (CT::Same<E, T>) {
            Any_Helper_TestSame(pack, *element);

            REQUIRE(pack.GetUses() == element->GetUses());
            REQUIRE(pack.GetUses() == 2);
            REQUIRE(pack.IsStatic() == element->IsStatic());
            REQUIRE(pack.GetAllocation() == element->GetAllocation());
         }

         if constexpr (not CT::Typed<T>) {
            REQUIRE_THROWS(pack.template As<float>() == 0.0f);
            REQUIRE_THROWS(pack.template As<float*>() == nullptr);
         }

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("operator = (single value move)") (timer meter) {
               some<T> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = ::std::move(value);
               });
            };

            BENCHMARK_ADVANCED("std::vector::operator = (single value move)") (timer meter) {
               some<StdT> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = {::std::move(value)};
               });
            };

            BENCHMARK_ADVANCED("std::any::operator = (single value move)") (timer meter) {
               some<std::any> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = ::std::move(value);
               });
            };
         #endif
      }

      WHEN("Assigned disowned value") {
         pack.Assign(Disown(*element));

         if constexpr (CT::Typed<T> and CT::Deep<E> and LANGULUS(SAFE))
            REQUIRE_THROWS(pack = Disown(*element));

         if constexpr (CT::Flat<E>) {
            Any_CheckState_OwnedFull<E>(pack);

            REQUIRE(pack.template As<E>() == *element);
            REQUIRE(*pack.template As<E*>() == *element);
            REQUIRE(pack.GetUses() == 1);
         }
         else if constexpr (CT::Same<E, T>) {
            Any_Helper_TestSame(pack, *element);

            REQUIRE(pack.GetUses() == 0);
            REQUIRE(pack.IsStatic());
            REQUIRE_FALSE(pack.GetAllocation());
         }

         if constexpr (not CT::Typed<T>) {
            REQUIRE_THROWS(pack.template As<float>() == 0.0f);
            REQUIRE_THROWS(pack.template As<float*>() == nullptr);
         }

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("operator = (single disowned value)") (timer meter) {
               some<T> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = Disown(value);
               });
            };

            BENCHMARK_ADVANCED("std::vector::operator = (single value copy)") (timer meter) {
               some<StdT> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = {value};
               });
            };

            BENCHMARK_ADVANCED("std::any::operator = (single value copy)") (timer meter) {
               some<std::any> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = value;
               });
            };
         #endif
      }
      
      WHEN("Assigned abandoned value") {
         auto movable = *element;
         pack.Assign(Abandon(movable));

         if constexpr (CT::Typed<T> and CT::Deep<E> and LANGULUS(SAFE))
            REQUIRE_THROWS(pack = Abandon(movable));

         if constexpr (CT::Container<E>)
            Any_CheckState_Abandoned<E>(movable);

         if constexpr (CT::Flat<E>) {
            Any_CheckState_OwnedFull<E>(pack);

            REQUIRE(pack.template As<E>() == *element);
            REQUIRE(*pack.template As<E*>() == *element);
            REQUIRE(pack.GetUses() == 1);
         }
         else if constexpr (CT::Same<E, T>) {
            Any_Helper_TestSame(pack, *element);

            REQUIRE(pack.GetUses() == 2);
            REQUIRE(pack.IsStatic() == element->IsStatic());
            REQUIRE(pack.GetAllocation() == element->GetAllocation());
         }

         if constexpr (not CT::Typed<T>) {
            REQUIRE_THROWS(pack.template As<float>() == 0.0f);
            REQUIRE_THROWS(pack.template As<float*>() == nullptr);
         }

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("operator = (single abandoned value)") (timer meter) {
               some<T> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = Abandon(value);
               });
            };

            BENCHMARK_ADVANCED("std::vector::operator = (single value move)") (timer meter) {
               some<StdT> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = {::std::move(value)};
               });
            };

            BENCHMARK_ADVANCED("std::any::operator = (single value move)") (timer meter) {
               some<std::any> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = ::std::move(value);
               });
            };
         #endif
      }

      WHEN("Assigned empty self") {
         LglsDisableWarningPush
         LglsDisableWarning_SelfAssign
         // ReSharper disable once CppIdenticalOperandsInBinaryExpression
         pack = pack;
         LglsDisableWarningPop

         Any_CheckState_Default<E>(pack);

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("operator = (self)") (timer meter) {
               some<T> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = storage[i];
               });
            };

            BENCHMARK_ADVANCED("std::vector::operator = (self)") (timer meter) {
               some<StdT> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = storage[i];
               });
            };

            BENCHMARK_ADVANCED("std::any::operator = (self)") (timer meter) {
               some<std::any> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = storage[i];
               });
            };
         #endif
      }

      WHEN("Emplace") {
         ScopedElement<E> i666 {666};
         const auto i666backup = *i666;
         if constexpr (CT::Typed<T>) {
            decltype(auto) instance = pack.Emplace(::std::move(*i666));

            Any_CheckState_OwnedFull<E>(pack);
            
            REQUIRE(pack.GetCount() == 1);
            REQUIRE(pack.GetReserved() >= 1);
            REQUIRE(*pack == i666backup);
            REQUIRE(&*pack == &*instance);

            #ifdef LANGULUS_STD_BENCHMARK
               BENCHMARK_ADVANCED("Anyness::TMany::Emplace(single move at the back)") (timer meter) {
                  some<T> storage(meter.runs());
                  for (auto&& o : storage)
                     o << darray1[0] << darray1[1] << darray1[2] << darray1[3] << darray1[4];

                  meter.measure([&](int i) {
                     return storage[i].Emplace(IndexBack, ::std::move(i666d));
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
            /*const Many descriptor {::std::move(*i666)};
            REQUIRE_THROWS(pack.Emplace(Describe(descriptor)));
            
            Any_CheckState_Default<E>(pack);*/ //TODO
         }
      }

      WHEN("Cleared") {
         pack.Clear();

         Any_CheckState_Default<E>(pack);
      }

      WHEN("Reset") {
         pack.Reset();

         Any_CheckState_Default<E>(pack);
      }

      WHEN("Shallow-copied empty") {
         auto copy = pack;

         Any_Helper_TestSame(copy, pack);
         Any_CheckState_Default<E>(copy);
         Any_CheckState_Default<E>(pack);
      }

      WHEN("Cloned empty") {
         T clone = Clone(pack);

         Any_Helper_TestSame(clone, pack);
         Any_CheckState_Default<E>(clone);
         Any_CheckState_Default<E>(pack);
      }

      WHEN("Moved empty") {
         T movable = pack;
         const T moved = ::std::move(movable);

         Any_CheckState_Default<E>(movable);
         Any_Helper_TestSame(moved, pack);
         Any_CheckState_Default<E>(moved);
         Any_CheckState_Default<E>(pack);
      }

      WHEN("Compared") {
         ScopedElement<E> e1 {1};
         ScopedElement<E> e2 {2};
         T another_pack1 {*e1};
         T another_pack2 {*e2};
         T defaulted_pack1;

         REQUIRE(pack != another_pack1);
         REQUIRE(pack != another_pack2);
         REQUIRE(pack == defaulted_pack1);
      }

      WHEN("Contains when empty") {
         REQUIRE_FALSE(pack.Contains(*element));
      }

      if constexpr (CT::Exact<E, Text>) {
         WHEN("Given text that will be destroyed before the pack") {
            Text owned_text = "666";
            pack = Text(owned_text.operator Token());
         }
      }
   }

   GIVEN("Container constructed by same container copy") {
      const ScopedElement<E> element {555};
      const T source {*element};
      T pack {source};

      if constexpr (CT::Flat<E>) {
         Any_CheckState_OwnedFull<E>(pack);
         
         REQUIRE(pack.template As<E>() == *element);
         REQUIRE(*pack.template As<E*>() == *element);
         REQUIRE(pack.GetUses() == 2);
      }
      else if constexpr (CT::Same<E, T>) {
         Any_Helper_TestSame(pack, *element);
         Any_Helper_TestSame(pack, source);
         
         REQUIRE(pack.GetUses() == 3);
      }

      if constexpr (not CT::Typed<T>) {
         REQUIRE_THROWS(pack.template As<float>() == 0.0f);
         REQUIRE_THROWS(pack.template As<float*>() == nullptr);
      }

      #ifdef LANGULUS_STD_BENCHMARK
         BENCHMARK_ADVANCED("construction (single container copy)") (timer meter) {
            some<uninitialized<T>> storage(meter.runs());
            meter.measure([&](int i) {
               return storage[i].construct(source);
            });
         };

         BENCHMARK_ADVANCED("std::vector::construction (single container copy)") (timer meter) {
            StdT source {1, 555};
            some<uninitialized<StdT>> storage(meter.runs());
            meter.measure([&](int i) {
               return storage[i].construct(source);
            });
         };

         BENCHMARK_ADVANCED("std::any::construction (single container copy)") (timer meter) {
            std::any source {555};
            some<uninitialized<std::any>> storage(meter.runs());
            meter.measure([&](int i) {
               return storage[i].construct(source);
            });
         };
      #endif
   }

   GIVEN("Container constructed by value copy") {
      const ScopedElement<E> element {555};
      T pack {*element};

      if constexpr (CT::Flat<E>) {
         Any_CheckState_OwnedFull<E>(pack);
         
         REQUIRE(pack.template As<E>() == *element);
         REQUIRE(*pack.template As<E*>() == *element);
         REQUIRE(pack.GetUses() == 1);
      }
      else if constexpr (CT::Same<E, T>) {
         Any_Helper_TestSame(pack, *element);
         
         REQUIRE(pack.GetUses() == 2);
      }

      if constexpr (not CT::Typed<T>) {
         REQUIRE_THROWS(pack.template As<float>() == 0.0f);
         REQUIRE_THROWS(pack.template As<float*>() == nullptr);
      }

      #ifdef LANGULUS_STD_BENCHMARK
         BENCHMARK_ADVANCED("construction (single value copy)") (timer meter) {
            some<uninitialized<T>> storage(meter.runs());
            meter.measure([&](int i) {
               return storage[i].construct(value);
            });
         };

         BENCHMARK_ADVANCED("std::vector::construction (single value copy)") (timer meter) {
            some<uninitialized<StdT>> storage(meter.runs());
            meter.measure([&](int i) {
               return storage[i].construct(1, value);
            });
         };

         BENCHMARK_ADVANCED("std::any::construction (single value copy)") (timer meter) {
            some<uninitialized<std::any>> storage(meter.runs());
            meter.measure([&](int i) {
               return storage[i].construct(value);
            });
         };
      #endif

      WHEN("Assigned compatible value by copy") {
         pack = *element;

         if constexpr (CT::Flat<E>) {
            Any_CheckState_OwnedFull<E>(pack);
            
            REQUIRE(pack.template As<E>() == *element);
            REQUIRE(*pack.template As<E*>() == *element);
            REQUIRE(pack.GetUses() == 1);
         }
         else if constexpr (CT::Same<E, T>) {
            Any_Helper_TestSame(pack, *element);
            
            REQUIRE(pack.GetUses() == element.GetUses());
            REQUIRE(pack.GetUses() == 2);
            REQUIRE(pack.IsStatic() == element.IsStatic());
            REQUIRE(pack.GetAllocation() == element.GetAllocation());
         }

         if constexpr (not CT::Typed<T>) {
            REQUIRE_THROWS(pack.template As<float>() == 0.0f);
            REQUIRE_THROWS(pack.template As<float*>() == nullptr);
         }

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("operator = (single value copy)") (timer meter) {
               some<T> storage(meter.runs(), element);
               meter.measure([&](int i) {
                  return storage[i] = value;
               });
            };

            BENCHMARK_ADVANCED("std::vector::operator = (single value copy)") (timer meter) {
               some<StdT> storage(meter.runs(), element);
               meter.measure([&](int i) {
                  return storage[i] = {value};
               });
            };

            BENCHMARK_ADVANCED("std::any::operator = (single value copy)") (timer meter) {
               some<std::any> storage(meter.runs(), element);
               meter.measure([&](int i) {
                  return storage[i] = value;
               });
            };
         #endif
      }
      
      WHEN("Assigned compatible value by move") {
         auto movable = *element;
         pack = ::std::move(movable);

         if constexpr (CT::Container<E>)
            Any_CheckState_Default<TypeOf<E>>(movable);

         if constexpr (CT::Flat<E>) {
            Any_CheckState_OwnedFull<E>(pack);
            
            REQUIRE(pack.template As<E>() == *element);
            REQUIRE(*pack.template As<E*>() == *element);
            REQUIRE(pack.GetUses() == 1);
         }
         else if constexpr (CT::Same<E, T>) {
            Any_Helper_TestSame(pack, *element);
            
            REQUIRE(pack.GetUses() == 2);
            REQUIRE(pack.IsStatic() == element.IsStatic());
            REQUIRE(pack.GetAllocation() == element.GetAllocation());
         }

         if constexpr (not CT::Typed<T>) {
            REQUIRE_THROWS(pack.template As<float>() == 0.0f);
            REQUIRE_THROWS(pack.template As<float*>() == nullptr);
         }

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("operator = (single value move)") (timer meter) {
               some<T> storage(meter.runs(), element);
               meter.measure([&](int i) {
                  return storage[i] = ::std::move(value);
               });
            };

            BENCHMARK_ADVANCED("std::vector::operator = (single value move)") (timer meter) {
               some<StdT> storage(meter.runs(), element);
               meter.measure([&](int i) {
                  return storage[i] = {::std::move(value)};
               });
            };

            BENCHMARK_ADVANCED("std::any::operator = (single value move)") (timer meter) {
               some<std::any> storage(meter.runs(), element);
               meter.measure([&](int i) {
                  return storage[i] = ::std::move(value);
               });
            };
         #endif
      }

      WHEN("Assigned compatible disowned value") {
         pack = Disown(*element);

         if constexpr (CT::Flat<E>) {
            Any_CheckState_OwnedFull<E>(pack);
            
            REQUIRE(pack.template As<E>() == *element);
            REQUIRE(*pack.template As<E*>() == *element);
            REQUIRE(pack.GetUses() == 1);
         }
         else if constexpr (CT::Same<E, T>) {
            Any_Helper_TestSame(pack, *element);
            
            REQUIRE(pack.GetUses() == 0);
            REQUIRE(pack.IsStatic());
            REQUIRE_FALSE(pack.GetAllocation());
         }

         if constexpr (not CT::Typed<T>) {
            REQUIRE_THROWS(pack.template As<float>() == 0.0f);
            REQUIRE_THROWS(pack.template As<float*>() == nullptr);
         }

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("operator = (single disowned value)") (timer meter) {
               some<T> storage(meter.runs(), element);
               meter.measure([&](int i) {
                  return storage[i] = Disown(value);
               });
            };

            BENCHMARK_ADVANCED("std::vector::operator = (single value copy)") (timer meter) {
               some<StdT> storage(meter.runs(), element);
               meter.measure([&](int i) {
                  return storage[i] = {value};
               });
            };

            BENCHMARK_ADVANCED("std::any::operator = (single value copy)") (timer meter) {
               some<std::any> storage(meter.runs(), element);
               meter.measure([&](int i) {
                  return storage[i] = value;
               });
            };
         #endif
      }
      
      WHEN("Assigned compatible abandoned value") {
         auto movable = *element;
         pack = Abandon(movable);

         if constexpr (CT::Container<E>)
            Any_CheckState_Abandoned<TypeOf<E>>(movable);

         if constexpr (CT::Flat<E>) {
            Any_CheckState_OwnedFull<E>(pack);
            
            REQUIRE(pack.template As<E>() == *element);
            REQUIRE(*pack.template As<E*>() == *element);
            REQUIRE(pack.GetUses() == 1);
         }
         else if constexpr (CT::Same<E, T>) {
            Any_Helper_TestSame(pack, *element);
            
            REQUIRE(pack.GetUses() == 2);
            REQUIRE(pack.IsStatic() == element.IsStatic());
            REQUIRE(pack.GetAllocation() == element.GetAllocation());
         }

         if constexpr (not CT::Typed<T>) {
            REQUIRE_THROWS(pack.template As<float>() == 0.0f);
            REQUIRE_THROWS(pack.template As<float*>() == nullptr);
         }

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("operator = (single abandoned value)") (timer meter) {
               some<T> storage(meter.runs(), element);
               meter.measure([&](int i) {
                  return storage[i] = Abandon(value);
               });
            };

            BENCHMARK_ADVANCED("std::vector::operator = (single value move)") (timer meter) {
               some<StdT> storage(meter.runs(), element);
               meter.measure([&](int i) {
                  return storage[i] = {::std::move(value)};
               });
            };

            BENCHMARK_ADVANCED("std::any::operator = (single value move)") (timer meter) {
               some<std::any> storage(meter.runs(), element);
               meter.measure([&](int i) {
                  return storage[i] = ::std::move(value);
               });
            };
         #endif
      }

      WHEN("Assigned compatible empty self") {
         pack = T {};

         Any_CheckState_Default<E>(pack);

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("operator = (self)") (timer meter) {
               some<T> storage(meter.runs(), element);
               meter.measure([&](int i) {
                  return storage[i] = storage[i];
               });
            };

            BENCHMARK_ADVANCED("std::vector::operator = (self)") (timer meter) {
               some<StdT> storage(meter.runs(), element);
               meter.measure([&](int i) {
                  return storage[i] = storage[i];
               });
            };

            BENCHMARK_ADVANCED("std::any::operator = (self)") (timer meter) {
               some<std::any> storage(meter.runs(), element);
               meter.measure([&](int i) {
                  return storage[i] = storage[i];
               });
            };
         #endif
      }

      WHEN("Assigned compatible full self") {
         pack = *element;
         auto packbackup = pack;
         LglsDisableWarningPush
         LglsDisableWarning_SelfAssign
         // ReSharper disable once CppIdenticalOperandsInBinaryExpression
         pack = pack;
         LglsDisableWarningPop
         
         Any_Helper_TestSame(pack, packbackup);

         REQUIRE(pack.IsTypeConstrained() == CT::Typed<T>);
         REQUIRE(pack.GetUses() == (CT::Deep<E> and CT::Same<T, E> ? 3 : 2));
         REQUIRE(pack.IsDeep() == (CT::Deep<Decay<E>> and not CT::Same<T, E>));
         REQUIRE(pack.IsAllocated());

      #ifdef LANGULUS_STD_BENCHMARK
         BENCHMARK_ADVANCED("operator = (self)") (timer meter) {
            some<T> storage(meter.runs(), element);
            meter.measure([&](int i) {
               return storage[i] = storage[i];
            });
         };

         BENCHMARK_ADVANCED("std::vector::operator = (self)") (timer meter) {
            some<StdT> storage(meter.runs(), element);
            meter.measure([&](int i) {
               return storage[i] = storage[i];
            });
         };

         BENCHMARK_ADVANCED("std::any::operator = (self)") (timer meter) {
            some<std::any> storage(meter.runs(), element);
            meter.measure([&](int i) {
               return storage[i] = storage[i];
            });
         };
      #endif
      }
   }

   GIVEN("Container constructed by value move") {
      const ScopedElement<E> element {555};
      E movable = *element;
      T pack {::std::move(movable)};

      if constexpr (CT::Container<E>)
         Any_CheckState_Default<TypeOf<E>>(movable);

      if constexpr (CT::Flat<E>) {
         Any_CheckState_OwnedFull<E>(pack);
         
         REQUIRE(pack.template As<E>() == *element);
         REQUIRE(*pack.template As<E*>() == *element);
         REQUIRE(pack.GetUses() == 1);
      }
      else if constexpr (CT::Same<E, T>) {
         Any_Helper_TestSame(pack, *element);
         
         REQUIRE(pack.GetUses() == 2);
         REQUIRE(pack.IsStatic() == element.IsStatic());
         REQUIRE(pack.GetAllocation() == element.GetAllocation());
      }
      else {
         REQUIRE(pack.template As<E>().GetRaw() == element.GetRaw());
         if constexpr (CT::Typed<T>)
            REQUIRE(pack.template IsExact<TypeOf<T>>());
         REQUIRE(pack.template As<E>() == *element);
         REQUIRE(*pack.template As<E*>() == *element);
         REQUIRE_FALSE(pack.template As<E>().IsStatic());
         REQUIRE_FALSE(pack.template As<E>().IsConstant());
         REQUIRE(pack.template As<E>().GetAllocation());
         REQUIRE(pack.template As<E>().GetUses() == 2);
         REQUIRE(pack.template As<E>() == *element);
         //REQUIRE(pack != element);
         REQUIRE(pack == *element);
         REQUIRE(pack.GetUses() == 1);
         REQUIRE(pack.IsDeep());
         REQUIRE_FALSE(pack.IsStatic());
         REQUIRE_FALSE(pack.IsConstant());
         REQUIRE(pack.GetAllocation());
      }

      if constexpr (not CT::Typed<T>) {
         REQUIRE_THROWS(pack.template As<float>() == 0.0f);
         REQUIRE_THROWS(pack.template As<float*>() == nullptr);
      }

      #ifdef LANGULUS_STD_BENCHMARK
         BENCHMARK_ADVANCED("construction (single value move)") (timer meter) {
            some<uninitialized<T>> storage(meter.runs());
            meter.measure([&](int i) {
               return storage[i].construct(::std::move(value));
            });
         };

         BENCHMARK_ADVANCED("std::vector::construction (single value move)") (timer meter) {
            some<uninitialized<StdT>> storage(meter.runs());
            meter.measure([&](int i) {
               return storage[i].construct(1, ::std::move(value));
            });
         };

         BENCHMARK_ADVANCED("std::any::construction (single value move)") (timer meter) {
            some<uninitialized<std::any>> storage(meter.runs());
            meter.measure([&](int i) {
               return storage[i].construct(::std::move(value));
            });
         };
      #endif
   }

   GIVEN("Container constructed by disowned value") {
      const ScopedElement<E> element {555};
      T pack {Disown(*element)};

      if constexpr (CT::Flat<E>) {
         Any_CheckState_OwnedFull<E>(pack);
         
         REQUIRE(pack.template As<E>() == *element);
         REQUIRE(*pack.template As<E*>() == *element);
         REQUIRE(pack.GetUses() == 1);
      }
      else if constexpr (CT::Same<E, T>) {
         Any_Helper_TestSame(pack, *element);
         
         REQUIRE(pack.GetUses() == 0);
         REQUIRE(pack.IsStatic());
         REQUIRE_FALSE(pack.GetAllocation());
      }
      else {
         REQUIRE(pack.template As<E>().GetRaw() == element.GetRaw());
         if constexpr (CT::Typed<T>)
            REQUIRE(pack.template IsExact<TypeOf<T>>());
         REQUIRE(pack.template As<E>() == *element);
         REQUIRE(*pack.template As<E*>() == *element);
         REQUIRE(pack.template As<E>().IsStatic());
         REQUIRE_FALSE(pack.template As<E>().IsConstant());
         REQUIRE_FALSE(pack.template As<E>().GetAllocation());
         REQUIRE(pack.template As<E>().GetUses() == 0);
         REQUIRE(pack.template As<E>() == *element);
         REQUIRE(pack == *element);
         //REQUIRE(pack != element);
         REQUIRE(pack.GetUses() == 1);
         REQUIRE_FALSE(pack.IsStatic());
         REQUIRE_FALSE(pack.IsConstant());
         REQUIRE(pack.GetAllocation());
         REQUIRE(pack.IsDeep());
      }

      if constexpr (not CT::Typed<T>) {
         REQUIRE_THROWS(pack.template As<float>() == 0.0f);
         REQUIRE_THROWS(pack.template As<float*>() == nullptr);
      }

   #ifdef LANGULUS_STD_BENCHMARK
      BENCHMARK_ADVANCED("construction (single disowned value)") (timer meter) {
         some<uninitialized<T>> storage(meter.runs());
         meter.measure([&](int i) {
            return storage[i].construct(Disowned(value));
         });
      };

      BENCHMARK_ADVANCED("std::vector::construction (single value copy)") (timer meter) {
         some<uninitialized<StdT>> storage(meter.runs());
         meter.measure([&](int i) {
            return storage[i].construct(1, value);
         });
      };

      BENCHMARK_ADVANCED("std::any::construction (single value copy)") (timer meter) {
         some<uninitialized<std::any>> storage(meter.runs());
         meter.measure([&](int i) {
            return storage[i].construct(value);
         });
      };
   #endif
   }
    
   GIVEN("Container constructed by abandoned value") {
      const ScopedElement<E> element {555};
      E movable = *element;
      T pack {Abandon(movable)};

      if constexpr (CT::Container<E>)
         Any_CheckState_Abandoned<TypeOf<E>>(movable);

      if constexpr (CT::Flat<E>) {
         Any_CheckState_OwnedFull<E>(pack);
         
         REQUIRE(pack.template As<E>() == *element);
         REQUIRE(*pack.template As<E*>() == *element);
         REQUIRE(pack.GetUses() == 1);
      }
      else if constexpr (CT::Same<E, T>) {
         Any_Helper_TestSame(pack, *element);

         REQUIRE(pack.GetUses() == 2);
         REQUIRE(pack.IsStatic() == element.IsStatic());
         REQUIRE(pack.GetAllocation() == element.GetAllocation());
      }
      else {
         REQUIRE(pack.template As<E>().GetRaw() == element.GetRaw());
         if constexpr (CT::Typed<T>)
            REQUIRE(pack.template IsExact<TypeOf<T>>());
         REQUIRE(pack.template As<E>() == *element);
         REQUIRE(*pack.template As<E*>() == *element);
         REQUIRE_FALSE(pack.template As<E>().IsStatic());
         REQUIRE_FALSE(pack.template As<E>().IsConstant());
         REQUIRE(pack.template As<E>().GetAllocation());
         REQUIRE(pack.template As<E>().GetUses() == 2);
         REQUIRE(pack.template As<E>() == *element);
         //REQUIRE(pack != element);
         REQUIRE(pack == *element);
         REQUIRE(pack.GetUses() == 1);
         REQUIRE(pack.IsDeep());
         REQUIRE_FALSE(pack.IsStatic());
         REQUIRE_FALSE(pack.IsConstant());
         REQUIRE(pack.GetAllocation());
      }

      if constexpr (not CT::Typed<T>) {
         REQUIRE_THROWS(pack.template As<float>() == 0.0f);
         REQUIRE_THROWS(pack.template As<float*>() == nullptr);
      }

   #ifdef LANGULUS_STD_BENCHMARK
      BENCHMARK_ADVANCED("construction (single abandoned value)") (timer meter) {
         some<uninitialized<T>> storage(meter.runs());
         meter.measure([&](int i) {
            return storage[i].construct(Abandon(value));
         });
      };

      BENCHMARK_ADVANCED("std::vector::construction (single value move)") (timer meter) {
         some<uninitialized<StdT>> storage(meter.runs());
         meter.measure([&](int i) {
            return storage[i].construct(1, ::std::move(value));
         });
      };

      BENCHMARK_ADVANCED("std::any::construction (single value move)") (timer meter) {
         some<uninitialized<std::any>> storage(meter.runs());
         meter.measure([&](int i) {
            return storage[i].construct(::std::move(value));
         });
      };
   #endif
   }

   GIVEN("Full container") {
      const ScopedElement<E> element {555};
      T pack {*element};
      const auto memory = pack.GetRaw();
      
      Any_CheckState_OwnedFull<E>(pack);
      
      REQUIRE(pack.GetCount() == 1);
      REQUIRE(pack.GetReserved() == 1);
      REQUIRE(pack.GetRaw());
      REQUIRE(pack == *element);
      
      WHEN("Emplace (overwrite existing)") {
         ScopedElement<E> i666 {666};
         const auto i666backup = *i666;
         if constexpr (CT::Typed<T>) {
            decltype(auto) instance = pack.Emplace(::std::move(*i666));

            Any_CheckState_OwnedFull<E>(pack);
            
            REQUIRE(pack.GetCount() == 1);
            REQUIRE(pack.GetReserved() >= 1);
            REQUIRE(*pack == i666backup);
            REQUIRE(&*pack == &*instance);

            #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("Anyness::TMany::Emplace(single move at the back)") (timer meter) {
               some<T> storage(meter.runs());
               for (auto&& o : storage)
                  o << darray1[0] << darray1[1] << darray1[2] << darray1[3] << darray1[4];

               meter.measure([&](int i) {
                  return storage[i].Emplace(IndexBack, ::std::move(i666d));
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
            /*const Many descriptor {::std::move(*i666)};
            REQUIRE_THROWS(pack.Emplace(Describe(descriptor)));
            
            Any_CheckState_Default<E>(pack);*/ //TODO
         }
      }
      
      WHEN("Cleared") {
         pack.Clear();

         Any_CheckState_OwnedEmpty<E>(pack);
      }

      WHEN("Reset") {
         pack.Reset();

         Any_CheckState_Default<E>(pack);
      }

      if constexpr (LANGULUS_FEATURE(MANAGED_MEMORY) and not CT::Container<E>) {
         // Works only if E doesn't move entries around
         WHEN("Reset, and then immediately allocated again") {
            pack.Reset();
            pack = *element;

            REQUIRE(pack.GetRaw() == memory);
         }
      }

      WHEN("Shallow-copied") {
         auto copy = pack;

         REQUIRE(copy.GetRaw() == pack.GetRaw());
         REQUIRE(copy.GetCount() == 1);
         REQUIRE(copy.GetCount() == pack.GetCount());
         REQUIRE(copy.GetReserved() == 1);
         REQUIRE(copy.GetReserved() == pack.GetReserved());
         REQUIRE(copy.GetState() == pack.GetState());
         REQUIRE(copy.GetType() == pack.GetType());
         REQUIRE(copy.GetUses() == 2);
         REQUIRE(pack.GetUses() == 2);
      }

      WHEN("Cloned") {
         T clone = Clone(pack);

         REQUIRE(clone.GetRaw() != pack.GetRaw());
         REQUIRE(clone.GetCount() == 1);
         REQUIRE(clone.GetCount() == pack.GetCount());
         REQUIRE(clone.GetReserved() == 1);
         REQUIRE(clone.GetReserved() >= clone.GetCount());
         REQUIRE(clone.GetState() == pack.GetState());
         REQUIRE(clone.GetType() == pack.GetType());
         REQUIRE(clone.GetUses() == 1);
         REQUIRE(pack.GetUses() == 1);
         REQUIRE(pack == *element);
         REQUIRE(clone == *element);
      }

      WHEN("Moved") {
         T movable = pack;
         const T moved = ::std::move(movable);

         Any_CheckState_Default<E>(movable);

         REQUIRE(pack.GetRaw() == moved.GetRaw());
         REQUIRE(pack.GetCount() == 1);
         REQUIRE(pack.GetCount() == moved.GetCount());
         REQUIRE(pack.GetReserved() == 1);
         REQUIRE(pack.GetReserved() == moved.GetReserved());
         REQUIRE(pack.GetState() == moved.GetState());
         REQUIRE(pack.GetType() == moved.GetType());
         REQUIRE(moved.GetUses() == 2);
         REQUIRE(pack.GetUses() == 2);
      }

      WHEN("Compared") {
         ScopedElement<E> e1 {1};
         ScopedElement<E> e2 {2};
         T another_pack1 {*e1};
         T another_pack2 {*e2};
         T defaulted_pack;
         T same_pack {pack};

         REQUIRE(pack != another_pack1);
         REQUIRE(pack != another_pack2);
         REQUIRE(pack != defaulted_pack);
         REQUIRE(pack == same_pack);
      }

      WHEN("Contains when full") {
         ScopedElement<E> e1 {1};
         
         REQUIRE      (pack.Contains(*element));
         REQUIRE_FALSE(pack.Contains(*e1));
      }
   }

   GIVEN("Two full containers") {
      const ScopedElement<E> e1 {555};
      const ScopedElement<E> e2 {666};
      T pack1 {*e1};
      T pack2 {*e2};
      const T memory1 = pack1;
      const T memory2 = pack2;

      WHEN("Copy-assign pack1 to pack2") {
         pack2 = Copy(pack1);
         
         Any_CheckState_OwnedFull<E>(pack1);
         Any_CheckState_OwnedFull<E>(pack2);

         REQUIRE(pack1.GetUses() == 2);
         REQUIRE(pack2.GetUses() == 1);
         REQUIRE(memory2.GetUses() == 1);
         REQUIRE(pack1 == pack2);
         REQUIRE(pack2 == memory1);
         REQUIRE(pack2 != memory2);
         REQUIRE(pack2 == *e1);
      }
      
      WHEN("Refer-assign pack1 in pack2") {
         pack2 = pack1;

         Any_CheckState_OwnedFull<E>(pack1);
         Any_CheckState_OwnedFull<E>(pack2);
         
         REQUIRE(pack1.GetUses() == 3);
         REQUIRE(pack2.GetUses() == 3);
         REQUIRE(memory2.GetUses() == 1);
         REQUIRE(pack1 == pack2);
         REQUIRE(pack2 == memory1);
         REQUIRE(pack2 != memory2);
         REQUIRE(pack2 == *e1);
      }

      WHEN("Move-assign pack1 in pack2") {
         auto movable = pack1;
         pack2 = ::std::move(movable);

         Any_CheckState_Default<E>(movable);
         Any_CheckState_OwnedFull<E>(pack1);
         Any_CheckState_OwnedFull<E>(pack2);
         
         REQUIRE(pack1.GetUses() == 3);
         REQUIRE(pack2.GetUses() == 3);
         REQUIRE(memory2.GetUses() == 1);
         REQUIRE(pack1 == pack2);
         REQUIRE(movable != pack1);
         REQUIRE(movable == T {});
      }

      WHEN("Disown-assign pack1 in pack2") {
         pack2 = Disown(pack1);
         
         Any_CheckState_OwnedFull<E>(pack1);
         Any_CheckState_DisownedFull<E>(pack2);

         REQUIRE(pack1.GetUses() == 2);
         REQUIRE(pack2.GetUses() == 0);
         REQUIRE(memory2.GetUses() == 1);
         REQUIRE(pack1 == pack2);
         REQUIRE(pack2 == memory1);
         REQUIRE(pack2 != memory2);
         REQUIRE(pack2.GetAllocation() == nullptr);
         REQUIRE(pack2 == *e1);
      }

      WHEN("Abandon-assign pack1 in pack2") {
         auto movable = pack1;
         pack2 = Abandon(movable);

         Any_CheckState_Abandoned<E>(movable);
         Any_CheckState_OwnedFull<E>(pack1);
         Any_CheckState_OwnedFull<E>(pack2);

         REQUIRE(pack1.GetUses() == 3);
         REQUIRE(pack2.GetUses() == 3);
         REQUIRE(memory2.GetUses() == 1);
         REQUIRE(pack1 == pack2);
      }

      WHEN("Copy-assign pack1 in pack2, then reset pack1") {
         pack2 = Copy(pack1);
         pack1.Reset();
         
         Any_CheckState_Default<E>(pack1);
         Any_CheckState_OwnedFull<E>(pack2);

         REQUIRE(pack2.GetUses() == 1);
         REQUIRE(pack2 == memory1);
      }
      
      WHEN("Refer-assign pack1 in pack2, then reset pack1") {
         pack2 = pack1;
         pack1.Reset();
         
         Any_CheckState_Default<E>(pack1);
         Any_CheckState_OwnedFull<E>(pack2);

         REQUIRE(pack2.GetUses() == 2);
         REQUIRE(pack2 == memory1);
      }

      WHEN("Clone-assign pack1 in pack2") {
         pack2 = Clone(pack1);

         REQUIRE(pack1.GetUses() == 2);
         REQUIRE(pack2.GetUses() == 1);
         REQUIRE(pack1 == pack2);
         REQUIRE(pack2 == memory1);
         REQUIRE(pack2 != memory2);
      }

      WHEN("Clone-assign pack1 in pack2, then reset pack1") {
         pack2 = Clone(pack1);
         const T memory3 = pack2;
         pack1.Reset();

         REQUIRE_FALSE(pack1.GetAllocation());
         REQUIRE(pack2.GetUses() == 2);
         REQUIRE(memory3.GetUses() == 2);
      }
   }

   REQUIRE(memoryState.Assert());
   REQUIRE_FALSE(Allocator::CollectGarbage());
}
