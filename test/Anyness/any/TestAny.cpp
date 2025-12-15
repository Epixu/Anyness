///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#include "TestAnyCommon.hpp"
#include "../../TestTypes/ReferencedType.hpp"
#include <any>

#if LANGULUS_FEATURE(MANAGED_MEMORY)
   #include "../../TestTypes/PackedPointers.hpp"
#endif


TEMPLATE_TEST_CASE("Test Any/TAny", "[any]"
   // Elements are not allocated by the memory manager                  
   , (Types<Any, Text, ScopedElement<Text>>)
   , (Types<Any, int, ScopedElement<int>>)
   , (Types<Any, Any, ScopedElement<Any>>)
   , (Types<Any, RT, ScopedElement<RT>>)
   , (Types<Any, char, ScopedElement<char>>)

   , (Types<Any, Text*, ScopedElement<Text*>>)
   , (Types<Any, int*, ScopedElement<int*>>)
   , (Types<Any, Any*, ScopedElement<Any*>>)
   , (Types<Any, RT*, ScopedElement<RT*>>)
   , (Types<Any, char*, ScopedElement<char*>>)

   , (Types<Any, Text**, ScopedElement<Text**>>)
   , (Types<Any, int**, ScopedElement<int**>>)
   , (Types<Any, Any**, ScopedElement<Any**>>)
   , (Types<Any, char**, ScopedElement<char**>>)

   , (Types<TAny<Text>, Text, ScopedElement<Text>>)
   , (Types<TAny<int>, int, ScopedElement<int>>)
   , (Types<TAny<Any>, Any, ScopedElement<Any>>)
   , (Types<TAny<char>, char, ScopedElement<char>>)

   , (Types<TAny<Text*>, Text*, ScopedElement<Text*>>)
   , (Types<TAny<int*>, int*, ScopedElement<int*>>)
   , (Types<TAny<Any*>, Any*, ScopedElement<Any*>>)
   , (Types<TAny<char*>, char*, ScopedElement<char*>>)

   , (Types<TAny<Text**>, Text**, ScopedElement<Text**>>)
   , (Types<TAny<int**>, int**, ScopedElement<int**>>)
   , (Types<TAny<Any**>, Any**, ScopedElement<Any**>>)
   , (Types<TAny<char**>, char**, ScopedElement<char**>>)

   #if LANGULUS_FEATURE(MANAGED_MEMORY)
   // Elements are allocated by the memory manager                      
   , (Types<Any, Text, ScopedElement<Text, true>>)
   , (Types<Any, int, ScopedElement<int, true>>)
   , (Types<Any, Any, ScopedElement<Any, true>>)
   , (Types<Any, RT, ScopedElement<RT, true>>)

   , (Types<Any, Text*, ScopedElement<Text*, true>>)
   , (Types<Any, int*, ScopedElement<int*, true>>)
   , (Types<Any, Any*, ScopedElement<Any*, true>>)
   , (Types<Any, RT*, ScopedElement<RT*, true>>)

   , (Types<Any, Text**, ScopedElement<Text**, true>>)
   , (Types<Any, int**, ScopedElement<int**, true>>)
   , (Types<Any, Any**, ScopedElement<Any**, true>>)
   , (Types<Any, RT**, ScopedElement<RT**, true>>)

   , (Types<TAny<Text>, Text, ScopedElement<Text, true>>)
   , (Types<TAny<int>, int, ScopedElement<int, true>>)
   , (Types<TAny<Any>, Any, ScopedElement<Any, true>>)
   , (Types<TAny<RT>, RT, ScopedElement<RT, true>>)

   , (Types<TAny<Text*>, Text*, ScopedElement<Text*, true>>)
   , (Types<TAny<int*>, int*, ScopedElement<int*, true>>)
   , (Types<TAny<Any*>, Any*, ScopedElement<Any*, true>>)
   , (Types<TAny<RT*>, RT*, ScopedElement<RT*, true>>)

   , (Types<TAny<Text**>, Text**, ScopedElement<Text**, true>>)
   , (Types<TAny<int**>, int**, ScopedElement<int**, true>>)
   , (Types<TAny<Any**>, Any**, ScopedElement<Any**, true>>)
   , (Types<TAny<RT**>, RT**, ScopedElement<RT**, true>>)

   // Packed pointers                                                   
   , (Types<Any, pptr8,  ScopedElementPacked<pptr8>>)
   , (Types<Any, pptr16, ScopedElementPacked<pptr16>>)
   , (Types<Any, pptr32, ScopedElementPacked<pptr32>>)
   
   , (Types<TAny<pptr8>,  pptr8,  ScopedElementPacked<pptr8>>)
   , (Types<TAny<pptr16>, pptr16, ScopedElementPacked<pptr16>>)
   , (Types<TAny<pptr32>, pptr32, ScopedElementPacked<pptr32>>)
   #endif
) {
   static MemoryState memoryState;
   using T = typename TestType::First;
   using E = typename TestType::Second;
   using ScopedE = typename TestType::template At<2>;

   if constexpr (CT::Untyped<T>) {
      // All type-erased containers should have all intent              
      // constructors and assigners available, and errors will instead  
      // be thrown as exceptions at runtime                             
      static_assert(Exact<TypeOf<T>, void>);
      static_assert(CT::TypeErased<T>);

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
      static_assert(    Exact<TypeOf<T>, E>);
      static_assert(not CT::TypeErased<T>);
      static_assert(    CT::Comparable<TypeOf<T>, E>);

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
   
   static_assert(    CT::Deep<T>);
   static_assert(    CT::ContainsOne<T>);
   static_assert(not CT::ContainsMany<T>);
   static_assert(    CT::HasVariableCount<T>);
   static_assert(    CT::HeapAllocated<T>);
   static_assert(    CT::DeeplyOwned<T>);
   static_assert(    CT::Owned<T>);
   static_assert(    CT::AutoOwned<T>);
   static_assert(    CT::Comparable<T, T>);
   static_assert(    CT::Comparable<T, E>);
   static_assert(not ::std::ranges::range<T>);

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

   constexpr bool Ambiguous = not Same<T, E> and CT::Deep<E> and CT::Dense<E> and LANGULUS(SAFE);
   
   GIVEN("Gap test") {
      alignas(T) char unininitialized[sizeof(T)];
      memset(unininitialized, 254, sizeof(unininitialized));
      new (unininitialized) T {};
      for (auto b : unininitialized) {
         REQUIRE(b != 254);
      }
      Logger::Info("Size of ", NameOf<::std::any>(), " container is: ", sizeof(::std::any), " bytes");
      auto s = Logger::Section("Size of ", NameOf<T>(), " container is: ", sizeof(T), " bytes");
      size_t accumulated_size = 0;
      size_t accumulated_stack_size = 0;
      T::ComponentList::ForEach([&]<class C> {
         if constexpr (requires { typename C::StackRequest; }) {
            Logger::Info(NameOf<C>(), " component is: ", sizeof(C), " bytes (reserves ", sizeof(typename C::StackRequest), " bytes on the stack)");
            accumulated_stack_size += sizeof(typename C::StackRequest);
         }
         else Logger::Info(NameOf<C>(), " component is: ", sizeof(C), " bytes");
         accumulated_size += sizeof(C);
      });
      Logger::Info("-----------------------------------------");
      Logger::Info("For a total of ", accumulated_size, " bytes in components (should be optimized-out as empty bases)");
      Logger::Info("For a total of ", accumulated_stack_size, " bytes on the stack");
      //STATIC_REQUIRE(sizeof(T) <= sizeof(::std::any)); // G++ implements std::any entirely on the heap, and I refuse to do it like this
   }

   GIVEN("Default-constructed container") {
      const ScopedE element {555};
      T pack;

      Any_CheckState_Default<E>(pack);
      
      #ifdef LANGULUS_STD_BENCHMARK
         BENCHMARK_ADVANCED("default construction") (timer meter) {
            some<uninitialized<T>> storage(meter.runs());
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

      if constexpr (Ambiguous) {
         WHEN("Ambiguous assign value by referral") {
            REQUIRE_THROWS(pack = *element);
            REQUIRE_THROWS(pack = Refer(*element));
         }
      }

      WHEN("Assigned value by referral") {
         pack.Assign(*element);

         Any_CheckState_OwnedFull<E>(pack);
         Any_CheckState_ContainsOne(pack, element);
         
         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("operator = (single value copy)") (timer meter) {
               some<T> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = value;
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

      if constexpr (Same<T, E>) {
         WHEN("Assigned and absorbed by referral") {
            pack.AssignFrom(*element);

            Any_Helper_TestSame(pack, *element);
         
            REQUIRE(pack.GetUses() == element->GetUses());
            REQUIRE(pack.GetUses() == 2);
            REQUIRE(pack.GetAllocation() == element->GetAllocation());

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

               BENCHMARK_ADVANCED("std::any::operator = (single value copy)") (timer meter) {
                  some<std::any> storage(meter.runs());
                  meter.measure([&](int i) {
                     return storage[i] = value;
                  });
               };
            #endif
         }
      }

      if constexpr (Ambiguous) {
         WHEN("Ambiguous assign value by move") {
            auto movable = *element;
            REQUIRE_THROWS(pack = ::std::move(movable));
            REQUIRE_THROWS(pack = Move(movable));
         }
      }
      
      WHEN("Assigned value by move") {
         auto movable = *element;
         pack.Assign(::std::move(movable));
         
         if constexpr (CT::Container<E>)
            Any_CheckState_Default<TypeOf<E>>(movable);

         Any_CheckState_OwnedFull<E>(pack);
         Any_CheckState_ContainsOne(pack, element);

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("operator = (single value move)") (timer meter) {
               some<T> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = ::std::move(value);
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

      if constexpr (Same<T, E>) {
         WHEN("Assigned and absorbed by move") {
            auto movable = *element;
            pack.AssignFrom(::std::move(movable));
         
            if constexpr (CT::Container<E>)
               Any_CheckState_Default<TypeOf<E>>(movable);

            Any_Helper_TestSame(pack, *element);

            REQUIRE(pack.GetUses() == element->GetUses());
            REQUIRE(pack.GetUses() == 2);
            REQUIRE(pack.GetAllocation() == element->GetAllocation());

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

               BENCHMARK_ADVANCED("std::any::operator = (single value move)") (timer meter) {
                  some<std::any> storage(meter.runs());
                  meter.measure([&](int i) {
                     return storage[i] = ::std::move(value);
                  });
               };
            #endif
         }
      }

      if constexpr (Ambiguous) {
         WHEN("Ambiguous assign disowned value") {
            REQUIRE_THROWS(pack = Disown(*element));
         }
      }
      
      WHEN("Assigned disowned value") {
         pack.Assign(Disown(*element));

         Any_CheckState_OwnedFull<E>(pack);
         Any_CheckState_ContainsOne(pack, element, true);
         
         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("operator = (single disowned value)") (timer meter) {
               some<T> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = Disown(value);
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

      if constexpr (Same<T, E>) {
         WHEN("Assigned and absorbed disowned value") {
            pack.AssignFrom(Disown(*element));

            REQUIRE(pack.GetRaw() == element->GetRaw());
            REQUIRE(pack.IsExact(element->GetType()));
            REQUIRE(pack == *element);
            REQUIRE(pack.IsDeep() == element->IsDeep());
            REQUIRE(pack.IsConstant() != element->IsConstant());
            REQUIRE(pack.GetUnconstrainedState() == element->GetUnconstrainedState());

            REQUIRE(pack.GetUses() == 0);
            REQUIRE_FALSE(pack.GetAllocation());

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

               BENCHMARK_ADVANCED("std::any::operator = (single value copy)") (timer meter) {
                  some<std::any> storage(meter.runs());
                  meter.measure([&](int i) {
                     return storage[i] = value;
                  });
               };
            #endif
         }
      }

      if constexpr (Ambiguous) {
         WHEN("Ambiguous assigned abandoned value") {
            auto movable = *element;
            REQUIRE_THROWS(pack = Abandon(movable));
         }
      }
      
      WHEN("Assigned abandoned value") {
         auto movable = *element;
         pack.Assign(Abandon(movable));

         if constexpr (CT::Container<E>)
            Any_CheckState_Abandoned<E>(movable);

         Any_CheckState_OwnedFull<E>(pack);
         Any_CheckState_ContainsOne(pack, element);

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("operator = (single abandoned value)") (timer meter) {
               some<T> storage(meter.runs());
               meter.measure([&](int i) {
                  return storage[i] = Abandon(value);
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

      if constexpr (Same<T, E>) {
         WHEN("Assigned and absorbed abandoned value") {
            auto movable = *element;
            pack.AssignFrom(Abandon(movable));

            if constexpr (CT::Container<E>)
               Any_CheckState_Abandoned<E>(movable);

            Any_Helper_TestSame(pack, *element);

            REQUIRE(pack.GetUses() == 2);
            REQUIRE(pack.GetAllocation() == element->GetAllocation());

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

               BENCHMARK_ADVANCED("std::any::operator = (single value move)") (timer meter) {
                  some<std::any> storage(meter.runs());
                  meter.measure([&](int i) {
                     return storage[i] = ::std::move(value);
                  });
               };
            #endif
         }
      }

      WHEN("Ambigous assigned empty self") {
         LglsDisableWarningPush
         LglsDisableWarning_SelfAssign
         // ReSharper disable once CppIdenticalOperandsInBinaryExpression
         REQUIRE_NOTHROW(pack = pack);
         LglsDisableWarningPop
      }
      
      WHEN("Assigned empty self") {
         pack.AssignFrom(pack);

         Any_CheckState_Default<E>(pack);

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("operator = (self)") (timer meter) {
               some<T> storage(meter.runs());
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
         ScopedE i666 {666};
         const auto i666backup = *i666;
         if constexpr (CT::Typed<T>) {
            decltype(auto) instance = pack.Emplace(::std::move(*i666));

            Any_CheckState_OwnedFull<E>(pack);
            
            REQUIRE(pack.GetCount() == 1);
            REQUIRE(pack.GetReserved() >= 1);
            REQUIRE(*pack == i666backup);
            REQUIRE(&*pack == &*instance);
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
         ScopedE e1 {1};
         ScopedE e2 {2};
         T another_pack1 {Piecewise, *e1};
         T another_pack2 {Piecewise, *e2};
         T defaulted_pack1;

         REQUIRE(pack != another_pack1);
         REQUIRE(pack != another_pack2);
         REQUIRE(pack == defaulted_pack1);

         STATIC_REQUIRE(T{} == T{});

         if constexpr (CT::Deep<E> and CT::Dense<E>) {
            STATIC_REQUIRE(T{} == E{});
            STATIC_REQUIRE(E{} == T{});
         }
         else {
            STATIC_REQUIRE(T{} != E{});
            STATIC_REQUIRE(E{} != T{});
         }
      }

      WHEN("Contains when empty") {
         REQUIRE_FALSE(pack.Contains(*element));
      }

      if constexpr (Exact<E, Text>) {
         WHEN("Given text that will be destroyed before the pack") {
            Text owned_text = "666";
            pack = Text(owned_text.operator Token());
         }
      }
   }

   if constexpr (Ambiguous) {
      GIVEN("Container ambiguously constructed by value referral") {
         const ScopedE element {555};
         REQUIRE_THROWS(T {*element});      
         REQUIRE_THROWS(T {Refer(*element)});
      }
   }

   GIVEN("Container constructed by value referral") {
      const ScopedE originalElement {556};
      const ScopedE element {555};
      T pack {Piecewise, *originalElement};

      Any_CheckState_OwnedFull<E>(pack);
      Any_CheckState_ContainsOne(pack, originalElement);

      #ifdef LANGULUS_STD_BENCHMARK
         BENCHMARK_ADVANCED("construction (single value copy)") (timer meter) {
            some<uninitialized<T>> storage(meter.runs());
            meter.measure([&](int i) {
               return storage[i].construct(value);
            });
         };

         BENCHMARK_ADVANCED("std::any::construction (single value copy)") (timer meter) {
            some<uninitialized<std::any>> storage(meter.runs());
            meter.measure([&](int i) {
               return storage[i].construct(value);
            });
         };
      #endif

      WHEN("Assigned compatible value by referral") {
         pack.Assign(*element);

         Any_CheckState_OwnedFull<E>(pack);
         Any_CheckState_ContainsOne(pack, element);

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("operator = (single value copy)") (timer meter) {
               some<T> storage(meter.runs(), element);
                  
               meter.measure([&](int i) {
                  return storage[i] = value;
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

      if constexpr (Same<T, E>) {
         WHEN("Assigned and absorbed by referral") {
            pack.AssignFrom(*element);

            Any_Helper_TestSame(pack, *element);
         
            REQUIRE(pack.GetUses() == element->GetUses());
            REQUIRE(pack.GetUses() == 2);
            REQUIRE(pack.GetAllocation() == element->GetAllocation());

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

               BENCHMARK_ADVANCED("std::any::operator = (single value copy)") (timer meter) {
                  some<std::any> storage(meter.runs(), element);
                  meter.measure([&](int i) {
                     return storage[i] = value;
                  });
               };
            #endif
         }
      }
      
      WHEN("Assigned compatible value by move") {
         auto movable = *element;
         pack.Assign(::std::move(movable));

         if constexpr (CT::Container<E>)
            Any_CheckState_Default<TypeOf<E>>(movable);

         Any_CheckState_OwnedFull<E>(pack);
         Any_CheckState_ContainsOne(pack, element);

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("operator = (single value move)") (timer meter) {
               some<T> storage(meter.runs(), element);
               meter.measure([&](int i) {
                  return storage[i] = ::std::move(value);
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

      if constexpr (Same<T, E>) {
         WHEN("Assigned and absorbed by move") {
            auto movable = *element;
            pack.AssignFrom(::std::move(movable));

            if constexpr (CT::Container<E>)
               Any_CheckState_Default<TypeOf<E>>(movable);

            Any_Helper_TestSame(pack, *element);
         
            REQUIRE(pack.GetUses() == 2);
            REQUIRE(pack.GetAllocation() == element->GetAllocation());

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

               BENCHMARK_ADVANCED("std::any::operator = (single value move)") (timer meter) {
                  some<std::any> storage(meter.runs(), element);
                  meter.measure([&](int i) {
                     return storage[i] = ::std::move(value);
                  });
               };
            #endif
         }
      }

      WHEN("Assigned compatible disowned value") {
         pack.Assign(Disown(*element));

         Any_CheckState_OwnedFull<E>(pack);
         Any_CheckState_ContainsOne(pack, element, true);

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("operator = (single disowned value)") (timer meter) {
               some<T> storage(meter.runs(), element);
               meter.measure([&](int i) {
                  return storage[i] = Disown(value);
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

      if constexpr (Same<T, E>) {
         WHEN("Assigned and absorbed disowned value") {
            pack.AssignFrom(Disown(*element));

            REQUIRE(pack.GetRaw() == element->GetRaw());
            REQUIRE(pack.IsExact(element->GetType()));
            REQUIRE(pack == *element);
            REQUIRE(pack.IsDeep() == element->IsDeep());
            REQUIRE(pack.IsConstant() != element->IsConstant());
            REQUIRE(pack.GetUnconstrainedState() == element->GetUnconstrainedState());
         
            REQUIRE(pack.GetUses() == 0);
            REQUIRE_FALSE(pack.GetAllocation());

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

               BENCHMARK_ADVANCED("std::any::operator = (single value copy)") (timer meter) {
                  some<std::any> storage(meter.runs(), element);
                  meter.measure([&](int i) {
                     return storage[i] = value;
                  });
               };
            #endif
         }
      }
      
      WHEN("Assigned compatible abandoned value") {
         auto movable = *element;
         pack.Assign(Abandon(movable));

         if constexpr (CT::Container<E>)
            Any_CheckState_Abandoned<TypeOf<E>>(movable);

         Any_CheckState_OwnedFull<E>(pack);
         Any_CheckState_ContainsOne(pack, element);

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("operator = (single abandoned value)") (timer meter) {
               some<T> storage(meter.runs(), element);
               meter.measure([&](int i) {
                  return storage[i] = Abandon(value);
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

      if constexpr (Same<T, E>) {
         WHEN("Assigned and absorbed abandoned value") {
            auto movable = *element;
            pack.AssignFrom(Abandon(movable));

            if constexpr (CT::Container<E>)
               Any_CheckState_Abandoned<TypeOf<E>>(movable);

            Any_Helper_TestSame(pack, *element);
         
            REQUIRE(pack.GetUses() == 2);
            REQUIRE(pack.GetAllocation() == element->GetAllocation());

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

               BENCHMARK_ADVANCED("std::any::operator = (single value move)") (timer meter) {
                  some<std::any> storage(meter.runs(), element);
                  meter.measure([&](int i) {
                     return storage[i] = ::std::move(value);
                  });
               };
            #endif
         }
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

            BENCHMARK_ADVANCED("std::any::operator = (self)") (timer meter) {
               some<std::any> storage(meter.runs(), element);
               meter.measure([&](int i) {
                  return storage[i] = storage[i];
               });
            };
         #endif
      }

      WHEN("Assigned compatible full self") {
         auto packbackup = pack;
         const auto uses_before = pack.GetUses();
         LglsDisableWarningPush
         LglsDisableWarning_SelfAssign
         // ReSharper disable once CppIdenticalOperandsInBinaryExpression
         pack = pack;
         LglsDisableWarningPop
         
         Any_Helper_TestSame(pack, packbackup);

         REQUIRE(pack.GetUses() == uses_before);

         #ifdef LANGULUS_STD_BENCHMARK
            BENCHMARK_ADVANCED("operator = (self)") (timer meter) {
               some<T> storage(meter.runs(), element);
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

      WHEN("Absorbed by referral") {
         T absorbed1 {pack};
         T absorbed2 {Refer {pack}};

         Any_Helper_TestSame(absorbed1, pack);
         Any_Helper_TestSame(absorbed2, pack);
         
         REQUIRE(absorbed1.GetUses() == 3);
         REQUIRE(absorbed2.GetUses() == 3);
      }
      
      WHEN("Absorbed by move") {
         T backup = pack;
         T absorbed {::std::move(pack)};

         Any_CheckState_Default<E>(pack);
         Any_CheckState_OwnedFull<E>(absorbed);
         Any_Helper_TestSame(absorbed, backup);
         
         REQUIRE(absorbed.GetUses() == 2);
      }
      
      WHEN("Absorbed by move (alt)") {
         T backup = pack;
         T absorbed {Move {pack}};

         Any_CheckState_Default<E>(pack);
         Any_CheckState_OwnedFull<E>(absorbed);
         Any_Helper_TestSame(absorbed, backup);
         
         REQUIRE(absorbed.GetUses() == 2);
      }
      
      WHEN("Absorbed by abandon") {
         T backup = pack;
         T absorbed {Abandon {pack}};

         Any_CheckState_Abandoned<E>(pack);
         Any_CheckState_OwnedFull<E>(absorbed);
         Any_Helper_TestSame(absorbed, backup);
         
         REQUIRE(absorbed.GetUses() == 2);
      }
      
      WHEN("Absorbed by disown") {
         T absorbed {Disown {pack}};

         Any_CheckState_OwnedFull<E>(pack);
         Any_CheckState_DisownedFull<E>(absorbed);

         REQUIRE(absorbed.GetRaw() == pack.GetRaw());
         REQUIRE(absorbed.IsExact(pack.GetType()));
         REQUIRE(absorbed == pack);
         REQUIRE(absorbed.IsDeep() == pack.IsDeep());
         REQUIRE(absorbed.IsConstant() != pack.IsConstant());
         REQUIRE(absorbed.GetUnconstrainedState() == pack.GetUnconstrainedState());
         REQUIRE(absorbed.GetUses() == 0);
      }
      
      WHEN("Absorbed by copy") {
         T absorbed {Copy {pack}};

         Any_CheckState_OwnedFull<E>(pack);
         Any_CheckState_OwnedFull<E>(absorbed);
         
         REQUIRE(absorbed == pack);
         REQUIRE(absorbed.GetRaw() != pack.GetRaw());
         REQUIRE(absorbed.template As<E>() == pack.template As<E>());
         if constexpr (CT::Sparse<E>) {
            auto entry = *absorbed.GetEntries();
            if (entry) {
               REQUIRE(entry->GetUses() == 3);

               if constexpr (CT::Referenced<Decay<E>>) {
                  auto e = absorbed.template As<E>();
                  REQUIRE(DenseCast(e).GetReferences() == 3);
               }
            }
            else {
               if constexpr (CT::Referenced<Decay<E>>) {
                  auto e = absorbed.template As<E>();
                  REQUIRE(DenseCast(e).GetReferences() == 1);
               }
            }
         }
         REQUIRE(absorbed.GetUses() == 1);
         REQUIRE(pack.GetUses() == 1);
      }
      
      WHEN("Absorbed by clone") {
         T absorbed {Clone {pack}};

         Any_CheckState_OwnedFull<E>(pack);
         Any_CheckState_OwnedFull<E>(absorbed);

         REQUIRE((absorbed == pack) == CT::Dense<E>);        
         REQUIRE(absorbed.GetUses() == 1);
      }
      
      WHEN("Emplace (overwrite existing)") {
         ScopedE i666 {666};
         const auto i666backup = *i666;
         if constexpr (CT::Typed<T>) {
            decltype(auto) instance = pack.Emplace(::std::move(*i666));

            Any_CheckState_OwnedFull<E>(pack);
            
            REQUIRE(pack.GetCount() == 1);
            REQUIRE(pack.GetReserved() >= 1);
            REQUIRE(*pack == i666backup);
            REQUIRE(&*pack == &*instance);
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
            const auto memory = pack.GetRaw();
            pack.Reset();
            pack = *element;

            REQUIRE(pack.GetRaw() == memory);
         }
      }

      WHEN("Compared") {
         ScopedE e1 {1};
         ScopedE e2 {2};
         T another_pack1 {Piecewise, *e1};
         T another_pack2 {Piecewise, *e2};
         T defaulted_pack;
         T same_pack {pack};

         REQUIRE(pack != another_pack1);
         REQUIRE(pack != another_pack2);
         REQUIRE(pack != defaulted_pack);
         REQUIRE(pack == same_pack);
      }

      WHEN("Contains when full") {
         ScopedE e1 {1};
         
         REQUIRE      (pack.Contains(*originalElement));
         REQUIRE_FALSE(pack.Contains(*e1));
      }
   }

   if constexpr (Ambiguous) {
      GIVEN("Container ambiguously constructed by moved value") {
         const ScopedE element {555};
         E movable = *element;     
      
         REQUIRE_THROWS(T {::std::move(movable)});
         REQUIRE_THROWS(T {Move(movable)});
      }
   }

   GIVEN("Container constructed by moved value") {
      const ScopedE element {555};
      E movable = *element;
      T pack {Piecewise, ::std::move(movable)};

      if constexpr (CT::Container<E>)
         Any_CheckState_Default<TypeOf<E>>(movable);

      Any_CheckState_OwnedFull<E>(pack);
      Any_CheckState_ContainsOne(pack, element);

      #ifdef LANGULUS_STD_BENCHMARK
         BENCHMARK_ADVANCED("construction (single value move)") (timer meter) {
            some<uninitialized<T>> storage(meter.runs());
            meter.measure([&](int i) {
               return storage[i].construct(::std::move(value));
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

   if constexpr (Ambiguous) {
      GIVEN("Container ambiguously constructed by disowned value") {
         const ScopedE element {555};
         REQUIRE_THROWS(T {Disown(*element)});
      }
   }

   GIVEN("Container constructed by disowned value") {
      const ScopedE element {555};
      T pack {Piecewise, Disown(*element)};
      
      if constexpr (CT::Container<E>)
         Any_CheckState_OwnedFull<TypeOf<E>>(*element);

      Any_CheckState_OwnedFull<E>(pack);
      Any_CheckState_ContainsOne(pack, element, true);
      
   #ifdef LANGULUS_STD_BENCHMARK
      BENCHMARK_ADVANCED("construction (single disowned value)") (timer meter) {
         some<uninitialized<T>> storage(meter.runs());
         meter.measure([&](int i) {
            return storage[i].construct(Disowned(value));
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

   if constexpr (Ambiguous) {
      GIVEN("Container ambiguously constructed by abandoned value") {
         const ScopedE element {555};
         E movable = *element;
         
         REQUIRE_THROWS(T {Abandon(movable)});
      }
   }
    
   GIVEN("Container constructed by abandoned value") {
      const ScopedE element {555};
      E movable = *element;
      T pack {Piecewise, Abandon(movable)};

      if constexpr (CT::Container<E>)
         Any_CheckState_Abandoned<TypeOf<E>>(movable);

      Any_CheckState_OwnedFull<E>(pack);
      Any_CheckState_ContainsOne(pack, element);

   #ifdef LANGULUS_STD_BENCHMARK
      BENCHMARK_ADVANCED("construction (single abandoned value)") (timer meter) {
         some<uninitialized<T>> storage(meter.runs());
         meter.measure([&](int i) {
            return storage[i].construct(Abandon(value));
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

   GIVEN("Two full containers") {
      const ScopedE e1 {555};
      const ScopedE e2 {666};
      T pack1 {Piecewise, *e1};
      T pack2 {Piecewise, *e2};
      const T memory1 = pack1;
      const T memory2 = pack2;

      WHEN("Copy-assign pack1 to pack2") {
         pack2 = Copy(pack1);
         
         Any_CheckState_OwnedFull<E>(pack1);
         Any_CheckState_OwnedFull<E>(pack2);
         Any_CheckState_ContainsOne(pack2, e1);

         REQUIRE(pack1.GetUses() == 2);
         REQUIRE(pack2.GetUses() == 1);
         REQUIRE(memory2.GetUses() == 1);
         
         REQUIRE(    pack1.CompareEqual(pack1));
         REQUIRE(    pack1.CompareEqual(pack2));
         REQUIRE(    pack2.CompareEqual(memory1));
         REQUIRE(not pack2.CompareEqual(memory2));
         REQUIRE(    pack2.CompareOneEqual(*e1));
         REQUIRE(not pack2.CompareOneEqual(*e2));
      }
      
      WHEN("Refer-assign pack1 in pack2") {
         pack2 = pack1;

         Any_CheckState_OwnedFull<E>(pack1);
         Any_CheckState_OwnedFull<E>(pack2);
         
         REQUIRE(pack1.GetUses() == 3);
         REQUIRE(pack2.GetUses() == 3);
         REQUIRE(memory2.GetUses() == 1);
         REQUIRE(pack1.CompareEqual(pack2));
         REQUIRE(pack2.CompareEqual(memory1));
         REQUIRE(not pack2.CompareEqual(memory2));
         REQUIRE(pack2.CompareOneEqual(*e1));
      }

      WHEN("Refer-assign pack1 in pack2 (alt)") {
         pack2 = Refer {pack1};

         Any_CheckState_OwnedFull<E>(pack1);
         Any_CheckState_OwnedFull<E>(pack2);
         
         REQUIRE(pack1.GetUses() == 3);
         REQUIRE(pack2.GetUses() == 3);
         REQUIRE(memory2.GetUses() == 1);
         REQUIRE(pack1.CompareEqual(pack2));
         REQUIRE(pack2.CompareEqual(memory1));
         REQUIRE(not pack2.CompareEqual(memory2));
         REQUIRE(pack2.CompareOneEqual(*e1));
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

      WHEN("Move-assign pack1 in pack2 (alt)") {
         auto movable = pack1;
         pack2 = Move {movable};

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
         REQUIRE(pack2.CompareOneEqual(*e1));
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

      WHEN("Clone-assign pack1 in pack2") {
         pack2 = Clone(pack1);

         REQUIRE(pack1.GetUses() == 2);
         REQUIRE(pack2.GetUses() == 1);
         REQUIRE((pack1 == pack2) == CT::Dense<E>);
         REQUIRE((pack2 == memory1) == CT::Dense<E>);
         REQUIRE(pack2 != memory2);
      }

      WHEN("Copy-assign pack1 in pack2, then reset pack1") {
         pack2 = Copy(pack1);
         pack1.Reset();

         Any_CheckState_Default<E>(pack1);
         Any_CheckState_OwnedFull<E>(pack2);

         REQUIRE(pack2.GetUses() == 1);
         REQUIRE(pack2 == memory1);
      }

      WHEN("Clone-assign pack1 in pack2, then reset pack1") {
         pack2 = Clone(pack1);
         REQUIRE(pack1.GetUses() == 2);
         REQUIRE(pack2.GetUses() == 1);
         if constexpr (CT::Sparse<E>)
            REQUIRE((*pack2.GetEntries())->GetUses() == 1);
         if constexpr (CT::Sparse<Deptr<E>>)
            REQUIRE((*(pack2.GetEntries()+1))->GetUses() == 1);

         const T memory3 = pack2;
         REQUIRE(pack2.GetUses() == 2);
         if constexpr (CT::Sparse<E>)
            REQUIRE((*pack2.GetEntries())->GetUses() == 2);
         if constexpr (CT::Sparse<Deptr<E>>)
            REQUIRE((*(pack2.GetEntries() + 1))->GetUses() == 2);

         pack1.Reset();

         REQUIRE_FALSE(pack1.GetAllocation());
         REQUIRE(pack2.GetUses() == 2);
         REQUIRE(memory3.GetUses() == 2);
      }

      WHEN("Refer-assign pack1 in pack2, then reset pack1") {
         pack2 = pack1;
         pack1.Reset();
         
         Any_CheckState_Default<E>(pack1);
         Any_CheckState_OwnedFull<E>(pack2);

         REQUIRE(pack2.GetUses() == 2);
         REQUIRE(pack2 == memory1);
      }
   }

   REQUIRE(memoryState.Assert());
   REQUIRE_FALSE(Allocator::CollectGarbage());
}
