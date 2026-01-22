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
#include <Langulus/Anyness/Many.hpp>

#if LANGULUS_FEATURE(MANAGED_MEMORY)
   #include "../../TestTypes/PackedPointers.hpp"
#endif

#if LANGULUS(BENCHMARK)
   #include <Langulus/Profiler.hpp>
   constexpr int BenchmarkWarmupCycles  =  100;
   constexpr int BenchmarkMeasureCycles = 1000;

   #define Benchmark(func, tolerance, my_init, my) { \
      const auto token = ::std::string("Test/") + static_cast<::std::string>(func) + " |" + static_cast<::std::string>(NameOf<T>()) + "|"; \
      volatile int i = 0; \
      for (; i < BenchmarkWarmupCycles; i += 1) { \
         my_init; \
         my; \
      } \
      for (; i < BenchmarkWarmupCycles + BenchmarkMeasureCycles; i += 1) { \
         my_init; \
         { \
            CTRACK_NAME_PERSIST(token.c_str()); \
            my; \
         } \
      } \
      auto results = ctrack::result_get_detail_table(); \
      results.check_highscore(tolerance); \
   }

   #define BenchmarkStd(func, tolerance_highscore, tolerance, my_init, my, theirs_init, theirs) { \
      const auto token = ::std::string("Test/") + static_cast<::std::string>(func) + " |" + static_cast<::std::string>(NameOf<T>()) + "|"; \
      volatile int i = 0; \
      for (; i < BenchmarkWarmupCycles; i += 1) { \
         my_init; \
         my; \
      } \
      for (; i < BenchmarkWarmupCycles + BenchmarkMeasureCycles; i += 1) { \
         my_init; \
         { \
            CTRACK_NAME_PERSIST(token.c_str()); \
            my; \
         } \
      } \
      i = 0; \
      const auto token_std = ::std::string("Test/") + static_cast<::std::string>(func) + " |std::any|"; \
      for (; i < BenchmarkWarmupCycles; i += 1) { \
         theirs_init; \
         theirs; \
      } \
      for (; i < BenchmarkWarmupCycles + BenchmarkMeasureCycles; i += 1) { \
         theirs_init; \
         { \
            CTRACK_NAME(token_std.c_str()); \
            theirs; \
         } \
      } \
      auto results = ctrack::result_get_detail_table(); \
      results.check_highscore(tolerance_highscore); \
      REQUIRE(results.check_same(token.c_str(), token_std.c_str(), tolerance)); \
   }
#else
   #define Benchmark(func, tolerance, my_init, my)
   #define BenchmarkStd(func, tolerance_highscore, tolerance, my_init, my, theirs_init, theirs)
#endif


TEST_CASE_TEMPLATE("Test Any/TAny", TestType
   // Elements are not allocated by the memory manager                  
   , Types<Any, Text,   ScopedElement<Text>>
   , Types<Any, int,    ScopedElement<int>>
   , Types<Any, Any,    ScopedElement<Any>>
   , Types<Any, RT,     ScopedElement<RT>>
   , Types<Any, char,   ScopedElement<char>>
                        
   , Types<Any, Text*,  ScopedElement<Text*>>
   , Types<Any, int*,   ScopedElement<int*>>
   , Types<Any, Any*,   ScopedElement<Any*>>
   , Types<Any, RT*,    ScopedElement<RT*>>
   , Types<Any, char*,  ScopedElement<char*>>

   , Types<Any, Text**, ScopedElement<Text**>>
   , Types<Any, int**,  ScopedElement<int**>>
   , Types<Any, Any**,  ScopedElement<Any**>>
   , Types<Any, char**, ScopedElement<char**>>

   , Types<TAny<Text>,   Text,   ScopedElement<Text>>
   , Types<TAny<int>,    int,    ScopedElement<int>>
   , Types<TAny<Any>,    Any,    ScopedElement<Any>>
   , Types<TAny<char>,   char,   ScopedElement<char>>
                                 
   , Types<TAny<Text*>,  Text*,  ScopedElement<Text*>>
   , Types<TAny<int*>,   int*,   ScopedElement<int*>>
   , Types<TAny<Any*>,   Any*,   ScopedElement<Any*>>
   , Types<TAny<char*>,  char*,  ScopedElement<char*>>

   , Types<TAny<Text**>, Text**, ScopedElement<Text**>>
   , Types<TAny<int**>,  int**,  ScopedElement<int**>>
   , Types<TAny<Any**>,  Any**,  ScopedElement<Any**>>
   , Types<TAny<char**>, char**, ScopedElement<char**>>

   #if LANGULUS_FEATURE(MANAGED_MEMORY)
   // Elements are allocated by the memory manager                      
   , Types<Any, Text,   ScopedElement<Text, true>>
   , Types<Any, int,    ScopedElement<int, true>>
   , Types<Any, Any,    ScopedElement<Any, true>>
   , Types<Any, RT,     ScopedElement<RT, true>>
                        
   , Types<Any, Text*,  ScopedElement<Text*, true>>
   , Types<Any, int*,   ScopedElement<int*, true>>
   , Types<Any, Any*,   ScopedElement<Any*, true>>
   , Types<Any, RT*,    ScopedElement<RT*, true>>

   , Types<Any, Text**, ScopedElement<Text**, true>>
   , Types<Any, int**,  ScopedElement<int**, true>>
   , Types<Any, Any**,  ScopedElement<Any**, true>>
   , Types<Any, RT**,   ScopedElement<RT**, true>>

   , Types<TAny<Text>,   Text,   ScopedElement<Text, true>>
   , Types<TAny<int>,    int,    ScopedElement<int, true>>
   , Types<TAny<Any>,    Any,    ScopedElement<Any, true>>
   , Types<TAny<RT>,     RT,     ScopedElement<RT, true>>
                                 
   , Types<TAny<Text*>,  Text*,  ScopedElement<Text*, true>>
   , Types<TAny<int*>,   int*,   ScopedElement<int*, true>>
   , Types<TAny<Any*>,   Any*,   ScopedElement<Any*, true>>
   , Types<TAny<RT*>,    RT*,    ScopedElement<RT*, true>>

   , Types<TAny<Text**>, Text**, ScopedElement<Text**, true>>
   , Types<TAny<int**>,  int**,  ScopedElement<int**, true>>
   , Types<TAny<Any**>,  Any**,  ScopedElement<Any**, true>>
   , Types<TAny<RT**>,   RT**,   ScopedElement<RT**, true>>

   // Packed pointers                                                   
   , Types<Any, pptr8,  ScopedElementPacked<pptr8>>
   , Types<Any, pptr16, ScopedElementPacked<pptr16>>
   , Types<Any, pptr32, ScopedElementPacked<pptr32>>

   , Types<TAny<pptr8>,  pptr8,  ScopedElementPacked<pptr8>>
   , Types<TAny<pptr16>, pptr16, ScopedElementPacked<pptr16>>
   , Types<TAny<pptr32>, pptr32, ScopedElementPacked<pptr32>>
   #endif
) {
   static MemoryState memoryState;
   using T = typename TestType::First;
   using E = typename TestType::Second;
   using ScopedE = typename TestType::template At<2>;
   constexpr bool Managed = ScopedE::Managed;

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
   
   {
      static_assert(CT::Deep<T>);
      static_assert(CT::ContainsOne<T>);
      static_assert(not CT::ContainsMany<T>);
      static_assert(CT::HasVariableCount<T>);
      static_assert(CT::HeapAllocated<T>);
      static_assert(CT::DeeplyOwned<T>);
      static_assert(CT::Owned<T>);
      static_assert(CT::AutoOwned<T>);
      static_assert(CT::Comparable<T, T>);
      static_assert(CT::Comparable<T, E>);
      static_assert(not ::std::ranges::range<T>);

      static_assert(not requires (T pack, E item) { pack.operator +   (item); });
      static_assert(not requires (T pack, E item) { pack.operator +=  (item); });
      static_assert(not requires (T pack, E item) { pack.operator <<  (item); });
      static_assert(not requires (T pack, E item) { pack.operator >>  (item); });
      static_assert(not requires (T pack, E item) { pack.operator <<= (item); });
      static_assert(not requires (T pack, E item) { pack.operator >>= (item); });
      static_assert(not requires (T pack, E item) { pack.InsertAt(Index::Back, item); });
      static_assert(not requires (T pack, E item) { pack.EmplaceAt(Index::Back, item); });
      static_assert(not requires (T pack, E item) { pack.Remove(item); });
      static_assert(not requires (T pack, E item) { pack.RemoveAt(Index::Front); });
      static_assert(not requires (T pack, E item) { pack.Reserve(20); });
      static_assert(not requires (T pack, E item) { pack.EnableOr(); });
      static_assert(not requires (T pack, E item) { pack.IsOr(); });
      static_assert(not requires (T pack, E item) { pack.Find(item); });
      static_assert(not requires (T pack, E item) { pack.ForEach([](const int&) {}); });
      static_assert(not requires (T pack, E item) { pack.ForEachRev([](const int&) {}); });
   }

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
      //static_assert(sizeof(T) <= sizeof(::std::any)); // G++ implements std::any entirely on the heap, and I refuse to do it like this
   }

   GIVEN("Default-constructed container") {
      const ScopedE element {555};
      T pack;

      WHEN("Default-constructed") {
         Any_CheckState_Default<E>(pack);
      
         if constexpr (Ambiguous) {
            WHEN("Ambiguous assign value by referral") {
               REQUIRE_THROWS(pack = *element);
               REQUIRE_THROWS(pack = Refer(*element));
            }
         }

         BenchmarkStd("Empty/DefaultConstructor", 30, 40,
            T temp,                 new (&temp)     T{},
            ::std::any temp_std,    new (&temp_std) ::std::any{}
         );
      }

      WHEN("Assigned value by referral") {
         pack.Assign(*element);

         Any_CheckState_OwnedFull<E>(pack);
         Any_CheckState_ContainsOne(pack, Refer(element));

         BenchmarkStd("Empty/Assign(Refer(" + NameOf<E>() + "))", 30, 100,
            T temp,                 temp.Assign(*element),
            ::std::any temp_std,    temp_std = *element
         );
      }

      if constexpr (CT::ContainsOne<E>) {
         WHEN("Assigned and absorbed by referral") {
            if (CT::Typed<T> and not pack.IsSame(element->GetType())) {
               const auto element_backup = *element;
               REQUIRE_THROWS(pack.AssignAbsorb(*element));
               Any_CheckState_Default<E>(pack);
               Any_Helper_TestSame(element_backup, *element);
               return;
            }

            pack.AssignAbsorb(*element);

            Any_Helper_TestSame(pack, *element);         
            REQUIRE(pack.GetUses() == element->GetUses());
            REQUIRE(pack.GetUses() == 2);
            REQUIRE(pack.GetAllocation() == element->GetAllocation());

            [[maybe_unused]] ::std::any src_std{*element};
            BenchmarkStd("Empty/AssignAbsorb(Refer(" + NameOf<E>() + "))", 30, 100,
               T temp,              temp.AssignAbsorb(*element),
               ::std::any temp_std, temp_std = src_std;
            );
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
         Any_CheckState_ContainsOne(pack, Refer(element));

         BenchmarkStd("Empty/Assign(Move(" + NameOf<E>() + "))", 30, 100,
            auto movable = *element; T temp,                temp.Assign(::std::move(movable)),
            auto movable = *element; ::std::any temp_std,   temp_std = ::std::move(movable)
         );
      }

      if constexpr (CT::ContainsOne<E>) {
         WHEN("Assigned and absorbed by move") {
            auto movable = *element;

            if (CT::Typed<T> and not pack.IsSame(element->GetType())) {
               REQUIRE_THROWS(pack.AssignAbsorb(::std::move(movable)));
               Any_CheckState_Default<E>(pack);
               Any_Helper_TestSame(movable, *element);
               return;
            }

            pack.AssignAbsorb(::std::move(movable));

            if constexpr (CT::Container<E>)
               Any_CheckState_Default<TypeOf<E>>(movable);
            Any_Helper_TestSame(pack, *element);
            REQUIRE(pack.GetUses() == element->GetUses());
            REQUIRE(pack.GetUses() == 2);
            REQUIRE(pack.GetAllocation() == element->GetAllocation());

            BenchmarkStd("Empty/AssignAbsorb(Move(" + NameOf<E>() + "))", 30, 100,
               auto movable = *element;  T temp,                temp.AssignAbsorb(::std::move(movable)),
               ::std::any movable = 555; ::std::any temp_std,   temp_std = ::std::move(movable)
            );
         }
      }

      if constexpr (Ambiguous) {
         WHEN("Ambiguous assign copied value") {
            REQUIRE_THROWS(pack = Copy(*element));
         }
      }
      
      WHEN("Assigned copied value") {
         pack.Assign(Copy(*element));

         Any_CheckState_OwnedFull<E>(pack);
         Any_CheckState_ContainsOne(pack, Copy(element));

         BenchmarkStd("Empty/Assign(Copy(" + NameOf<E>() + "))", 30, 100,
            T temp,                 temp.Assign(Copy(*element)),
            ::std::any temp_std,    temp_std = *element
         );
      }

      if constexpr (CT::ContainsOne<E>) {
         WHEN("Assigned and absorbed copied value") {
            if (CT::Typed<T> and not pack.IsSame(element->GetType())) {
               const auto element_backup = *element;
               REQUIRE_THROWS(pack.AssignAbsorb(Copy(*element)));
               Any_CheckState_Default<E>(pack);
               Any_Helper_TestSame(element_backup, *element);
               return;
            }

            pack.AssignAbsorb(Copy(*element));

            REQUIRE(pack.GetRaw() != element->GetRaw());
            REQUIRE(pack.IsExact(element->GetType()));
            REQUIRE(pack == *element);
            REQUIRE(pack.IsDeep() == element->IsDeep());
            REQUIRE_FALSE(pack.IsConstant());
            REQUIRE(pack.GetUnconstrainedState() == element->GetUnconstrainedState());

            REQUIRE(pack.GetUses() == 1);
            REQUIRE(pack.GetAllocation());

            [[maybe_unused]] ::std::any src_std = *element;
            BenchmarkStd("Empty/AssignAbsorb(Copy(" + NameOf<E>() + "))", 30, 100,
               T temp,                 temp.AssignAbsorb(Copy(*element)),
               ::std::any temp_std,    temp_std = src_std
            );
         }
      }

      if constexpr (Ambiguous) {
         WHEN("Ambiguous assign cloned value") {
            REQUIRE_THROWS(pack = Clone(*element));
         }
      }
      
      WHEN("Assigned cloned value") {
         pack.Assign(Clone(*element));

         Any_CheckState_OwnedFull<E>(pack);
         Any_CheckState_ContainsOne(pack, Clone(element));

         BenchmarkStd("Empty/Assign(Clone(" + NameOf<E>() + "))", 30, 100,
            T temp,                 temp.Assign(Clone(*element)),
            ::std::any temp_std,    temp_std = *element
         );
      }

      if constexpr (CT::ContainsOne<E>) {
         WHEN("Assigned and absorbed cloned value") {
            if (CT::Typed<T> and not pack.IsSame(element->GetType())) {
               const auto element_backup = *element;
               REQUIRE_THROWS(pack.AssignAbsorb(Clone(*element)));
               Any_CheckState_Default<E>(pack);
               Any_Helper_TestSame(element_backup, *element);
               return;
            }

            pack.AssignAbsorb(Clone(*element));

            REQUIRE(pack.GetRaw() != element->GetRaw());
            REQUIRE(pack.IsExact(element->GetType()));
            REQUIRE(pack == *element);
            REQUIRE(pack.IsDeep() == element->IsDeep());
            REQUIRE_FALSE(pack.IsConstant());
            REQUIRE(pack.GetUnconstrainedState() == element->GetUnconstrainedState());
            REQUIRE(pack.GetUses() == 1);
            REQUIRE(pack.GetAllocation());

            [[maybe_unused]] ::std::any src_std = *element;
            BenchmarkStd("Empty/AssignAbsorb(Clone(" + NameOf<E>() + "))", 30, 100,
               T temp,                 temp.AssignAbsorb(Clone(*element)),
               ::std::any temp_std,    temp_std = src_std
            );
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
         Any_CheckState_ContainsOne(pack, Disown(element));

         BenchmarkStd("Empty/Assign(Disown(" + NameOf<E>() + "))", 30, 100,
            T temp,                 temp.Assign(Disown(*element)),
            ::std::any temp_std,    temp_std = *element
         );
      }

      if constexpr (CT::ContainsOne<E>) {
         WHEN("Assigned and absorbed disowned value") {
            if (CT::Typed<T> and not pack.IsSame(element->GetType())) {
               const auto element_backup = *element;
               REQUIRE_THROWS(pack.AssignAbsorb(Disown(*element)));
               Any_CheckState_Default<E>(pack);
               Any_Helper_TestSame(element_backup, *element);
               return;
            }

            pack.AssignAbsorb(Disown(*element));

            REQUIRE(pack.GetRaw() == element->GetRaw());
            REQUIRE(pack.IsExact(element->GetType()));
            REQUIRE(pack == *element);
            REQUIRE(pack.IsDeep() == element->IsDeep());
            REQUIRE(pack.IsConstant() != element->IsConstant());
            REQUIRE(pack.GetUnconstrainedState() == element->GetUnconstrainedState());
            REQUIRE(pack.GetUses() == 0);
            REQUIRE_FALSE(pack.GetAllocation());

            [[maybe_unused]] ::std::any src_std = *element;
            BenchmarkStd("Empty/AssignAbsorb(Disown(" + NameOf<E>() + "))", 30, 100,
               T temp,                 temp.AssignAbsorb(Disown(*element)),
               ::std::any temp_std,    temp_std = src_std
            );
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
         Any_CheckState_ContainsOne(pack, Refer(element));

         BenchmarkStd("Empty/Assign(Abandon(" + NameOf<E>() + "))", 30, 100,
            auto movable = *element; T temp,                temp.Assign(Abandon(movable)),
            auto movable = *element; ::std::any temp_std,   temp_std = ::std::move(movable)
         );
      }

      if constexpr (CT::ContainsOne<E>) {
         WHEN("Assigned and absorbed abandoned value") {
            auto movable = *element;

            if (CT::Typed<T> and not pack.IsSame(element->GetType())) {
               REQUIRE_THROWS(pack.AssignAbsorb(Abandon(movable)));
               Any_CheckState_Default<E>(pack);
               Any_Helper_TestSame(movable, *element);
               return;
            }

            pack.AssignAbsorb(Abandon(movable));

            if constexpr (CT::Container<E>)
               Any_CheckState_Abandoned<E>(movable);
            Any_Helper_TestSame(pack, *element);
            REQUIRE(pack.GetUses() == 2);
            REQUIRE(pack.GetAllocation() == element->GetAllocation());

            BenchmarkStd("Empty/AssignAbsorb(Abandon(" + NameOf<E>() + "))", 30, 100,
               auto movable = *element;  T temp,                temp.AssignAbsorb(Abandon(movable)),
               ::std::any movable = 555; ::std::any temp_std,   temp_std = ::std::move(movable)
            );
         }
      }

      WHEN("Ambigous assigned empty self") {
         LglsDisableWarningPush
         LglsDisableWarning_SelfAssign
         REQUIRE_NOTHROW(pack = pack);
         LglsDisableWarningPop
      }
      
      WHEN("Assigned empty self") {
         pack.AssignAbsorb(pack);

         Any_CheckState_Default<E>(pack);
      }

      WHEN("Emplace (insert)") {
         ScopedE i666 {666};
         const auto i666backup = *i666;
         decltype(auto) instance = pack.Emplace(::std::move(*i666));
         Any_CheckState_OwnedFull<E>(pack);
         REQUIRE(instance.CompareOneEqual(i666backup));
         REQUIRE(pack.GetCount() == 1);
         REQUIRE(pack.GetReserved() >= 1);

         if constexpr (CT::Typed<T>) {
            REQUIRE(*pack == i666backup);
            REQUIRE(&*pack == &*instance);
         }

         Benchmark("Empty/Emplace(" + NameOf<E>() + ")", 30,
            auto movable = *element; T temp,
            temp.Emplace(::std::move(movable))
         );
      }

      WHEN("Emplace (insert, describe)") {
         ScopedE i666{666};
         const auto i666backup = *i666;
         Many descriptor {Piecewise, ::std::move(*i666)};
         if constexpr (CT::DescribeConstructible<E> and not CT::Container<T>) {
            decltype(auto) instance = pack.template Emplace<E>(Describe{descriptor});
            Any_CheckState_OwnedFull<E>(pack);
            REQUIRE(instance.CompareOneEqual(i666backup));
            REQUIRE(pack.GetCount() == 1);
            REQUIRE(pack.GetReserved() >= 1);

            Benchmark("Empty/Emplace(Describe(" + NameOf<E>() + "))", 30,
               T temp,
               temp.Emplace(Describe{descriptor})
            );
         }
         else if constexpr (CT::TypeErased<T>) {
            pack.template SetType<E>();
            REQUIRE_THROWS(pack.Emplace(Describe{descriptor}));
            Any_CheckState_Default<E>(pack, true);
         }
      }

      WHEN("Cleared") {
         pack.Clear();

         Any_CheckState_Default<E>(pack);

         BenchmarkStd("Empty/Clear(" + NameOf<E>() + ")", 30, 100,
            T temp,                 temp.Clear(),
            ::std::any temp_std,    temp_std.reset()
         );
      }

      WHEN("Reset") {
         pack.Reset();

         Any_CheckState_Default<E>(pack);

         BenchmarkStd("Empty/Reset(" + NameOf<E>() + ")", 30, 100,
            T temp,                 temp.Reset(),
            ::std::any temp_std,    temp_std.reset()
         );
      }

      WHEN("Referred empty") {
         T refer1 = pack;

         Any_Helper_TestSame(refer1, pack);
         Any_CheckState_Default<E>(refer1);
         Any_CheckState_Default<E>(pack);

         T refer2 = Refer(pack);

         Any_Helper_TestSame(refer2, pack);
         Any_CheckState_Default<E>(refer2);
         Any_CheckState_Default<E>(pack);
      }

      WHEN("Cloned empty") {
         T clone = Clone(pack);

         Any_Helper_TestSame(clone, pack);
         Any_CheckState_Default<E>(clone);
         Any_CheckState_Default<E>(pack);
      }

      WHEN("Disowned empty") {
         T disowned = Disown(pack);

         Any_Helper_TestSame(disowned, pack);
         Any_CheckState_Default<E>(disowned);
         Any_CheckState_Default<E>(pack);
      }

      WHEN("Copied empty") {
         T copy = Copy(pack);

         Any_Helper_TestSame(copy, pack);
         Any_CheckState_Default<E>(copy);
         Any_CheckState_Default<E>(pack);
      }

      WHEN("Moved empty") {
         T movable1 = pack;
         const T moved1 = ::std::move(movable1);

         Any_CheckState_Default<E>(movable1);
         Any_Helper_TestSame(moved1, pack);
         Any_CheckState_Default<E>(moved1);
         Any_CheckState_Default<E>(pack);

         T movable2 = pack;
         const T moved2 = Move(movable2);

         Any_CheckState_Default<E>(movable2);
         Any_Helper_TestSame(moved2, pack);
         Any_CheckState_Default<E>(moved2);
         Any_CheckState_Default<E>(pack);
      }

      WHEN("Abandoned empty") {
         T movable = pack;
         const T moved = Abandon(movable);

         Any_CheckState_Default<E>(movable);
         Any_Helper_TestSame(moved, pack);
         Any_CheckState_Default<E>(moved);
         Any_CheckState_Default<E>(pack);
      }

      WHEN("Compared empty") {
         T another_pack1;
         T another_pack2;

         REQUIRE      (another_pack1 == another_pack2);
         REQUIRE_FALSE(another_pack1 != another_pack2);
         static_assert(     T{} == T{} );
         static_assert(not (T{} != T{}));

         // Unfortunately, ::std::any aren't comparable when empty      
         [[maybe_unused]] volatile bool dont_optimize = false;
         Benchmark("Empty/operator==(" + NameOf<E>() + ")", 30,
            (void) 0, dont_optimize |= (another_pack1 == another_pack2)
         );
         Benchmark("Empty/operator!=(" + NameOf<E>() + ")", 30,
            (void) 0, dont_optimize |= (another_pack1 != another_pack2)
         );
      }

      WHEN("Contains when empty") {
         REQUIRE_FALSE(pack.Contains(*element));

         [[maybe_unused]] volatile bool dont_optimize = false;
         Benchmark("Empty/Contains(" + NameOf<E>() + ")", 30,
            (void) 0, dont_optimize |= pack.Contains(*element)
         );
      }

      if constexpr (Exact<E, Text>) {
         WHEN("Given text that will be destroyed before the pack") {
            Text owned_text = "666";
            pack = Text(owned_text.operator Token());
         }
      }
   }

   if constexpr (Ambiguous) {
      GIVEN("Piecewise-constructed container (ambiguously)") {
         ScopedE element {555};
         REQUIRE_THROWS(T {*element});      
         REQUIRE_THROWS(T {Refer(*element)});
         REQUIRE_THROWS(T {Copy(*element)});
         REQUIRE_THROWS(T {Clone(*element)});
         REQUIRE_THROWS(T {::std::move(*element)});
         REQUIRE_THROWS(T {Move(*element)});
         REQUIRE_THROWS(T {Abandon(*element)});
         REQUIRE_THROWS(T {Disown(*element)});
      }
   }

   GIVEN("Piecewise-constructed container") {
      const ScopedE originalElement {556};
      const ScopedE element {555};
      auto originalElement_movable1 = *originalElement;
      auto originalElement_movable2 = *originalElement;
      auto originalElement_movable3 = *originalElement;

      T pack_referred1{Piecewise,             *originalElement };
      T pack_referred2{Piecewise,       Refer(*originalElement)};
      T pack_copied   {Piecewise,        Copy(*originalElement)};
      T pack_cloned   {Piecewise,       Clone(*originalElement)};
      T pack_moved1   {Piecewise, ::std::move( originalElement_movable1)};
      T pack_moved2   {Piecewise,        Move( originalElement_movable2)};
      T pack_abandoned{Piecewise,     Abandon( originalElement_movable3)};
      T pack_disowned {Piecewise,      Disown(*originalElement)};

      WHEN("Value-constructed") {
         Any_CheckState_OwnedFull<E>(pack_referred1);
         Any_CheckState_OwnedFull<E>(pack_referred2);
         Any_CheckState_OwnedFull<E>(pack_copied);
         Any_CheckState_OwnedFull<E>(pack_cloned);
         Any_CheckState_OwnedFull<E>(pack_moved1);
         Any_CheckState_OwnedFull<E>(pack_moved2);
         Any_CheckState_OwnedFull<E>(pack_abandoned);
         Any_CheckState_OwnedFull<E>(pack_disowned);

         Any_CheckState_ContainsOne(pack_referred1, Refer(originalElement));
         Any_CheckState_ContainsOne(pack_referred2, Refer(originalElement));
         Any_CheckState_ContainsOne(pack_copied,    Refer(originalElement));
         Any_CheckState_ContainsOne(pack_cloned,    Clone(originalElement));
         Any_CheckState_ContainsOne(pack_moved1,    Refer(originalElement));
         Any_CheckState_ContainsOne(pack_abandoned, Refer(originalElement));
         Any_CheckState_ContainsOne(pack_disowned,  Disown(originalElement));

         BenchmarkStd("Empty/PiecewiseConstructor(" + NameOf<E>() + ")", 30, 400,
            T temp,              (new (&temp)     T{Piecewise, *originalElement}),
            ::std::any temp_std, new (&temp_std) ::std::any{*originalElement}
         );
      }

      WHEN("Assigned compatible referred value") {
         auto assign_refer = [&](auto& a, const char* intent) {
            a.Assign(*element);

            if constexpr (CT::Container<E>)
               Any_CheckState_OwnedFull<TypeOf<E>>(*element);
            Any_CheckState_OwnedFull<E>(a);
            Any_CheckState_ContainsOne(a, Refer(element));

            BenchmarkStd(
               std::string("Piecewise/") + intent + "/Assign(Refer(" + static_cast<std::string>(NameOf<E>()) + "))", 30, 100,
               a.Assign(*element),              a.Assign(*originalElement),
               ::std::any temp_std = *element,  temp_std = *originalElement
            );
         };

         assign_refer(pack_referred1, "Refer");
         assign_refer(pack_copied,    "Copy");
         assign_refer(pack_cloned,    "Clone");
         assign_refer(pack_moved1,    "Move");
         assign_refer(pack_abandoned, "Abandon");
         assign_refer(pack_disowned,  "Disown");
      }

      if constexpr (CT::ContainsOne<E>) {
         WHEN("Assigned and absorbed referred container") {
            if (not pack_referred1.IsSame(element->GetType())) {
               auto misabsorb_refer = [&](auto& a) {
                  REQUIRE_THROWS(a.AssignAbsorb(*element));
                  Any_CheckState_OwnedFull<E>(a);
                  Any_CheckState_ContainsOne(a, Refer(originalElement));
               };

               misabsorb_refer(pack_referred1);
               misabsorb_refer(pack_referred2);
               misabsorb_refer(pack_copied);
               misabsorb_refer(pack_cloned);
               misabsorb_refer(pack_moved1);
               misabsorb_refer(pack_moved2);
               misabsorb_refer(pack_abandoned);
               misabsorb_refer(pack_disowned);
               return;
            }

            auto absorb_refer = [&](auto& a, const char* intent) {
               a.AssignAbsorb(*element);

               Any_Helper_TestSame(a, *element);
               REQUIRE(a.GetUses() == element->GetUses());
               REQUIRE(a.GetUses() == 2);
               REQUIRE(a.GetAllocation() == element->GetAllocation());

               BenchmarkStd(
                  std::string("Piecewise/") + intent + "/AssignAbsorb(Refer(" + static_cast<std::string>(NameOf<E>()) + "))", 30, 100,
                  a.AssignAbsorb(*element),                 a.AssignAbsorb(*originalElement),
                  ::std::any temp_std1 = *element;
                  ::std::any temp_std2 = *originalElement,  temp_std1 = temp_std2
               );
            };

            absorb_refer(pack_referred1, "Refer");
            absorb_refer(pack_copied,    "Copy");
            absorb_refer(pack_cloned,    "Clone");
            absorb_refer(pack_moved1,    "Move");
            absorb_refer(pack_abandoned, "Abandon");
            absorb_refer(pack_disowned,  "Disown");
         }
      }
      
      WHEN("Assigned compatible cloned value") {
         auto assign_clone = [&](auto& a, const char* intent) {
            a.Assign(Clone(*element));

            if constexpr (CT::Container<E>)
               Any_CheckState_OwnedFull<TypeOf<E>>(*element);
            Any_CheckState_OwnedFull<E>(a);
            Any_CheckState_ContainsOne(a, Clone(element));

            BenchmarkStd(
               std::string("Piecewise/") + intent + "/Assign(Clone(" + static_cast<std::string>(NameOf<E>()) + "))", 30, 100,
               a.Assign(Clone(*element)),          a.Assign(Clone(*originalElement)),
               ::std::any temp_std = *element,     temp_std = *originalElement
            );
         };

         assign_clone(pack_referred1, "Refer");
         assign_clone(pack_copied,    "Copy");
         assign_clone(pack_cloned,    "Clone");
         assign_clone(pack_moved1,    "Move");
         assign_clone(pack_abandoned, "Abandon");
         assign_clone(pack_disowned,  "Disown");
      }

      if constexpr (CT::ContainsOne<E>) {
         WHEN("Assigned and absorbed cloned container") {
            if (not pack_referred1.IsSame(element->GetType())) {
               auto misabsorb_clone = [&](auto& a) {
                  REQUIRE_THROWS(a.AssignAbsorb(Clone(*element)));
                  Any_CheckState_OwnedFull<E>(a);
                  Any_CheckState_ContainsOne(a, Clone(originalElement));
               };

               misabsorb_clone(pack_referred1);
               misabsorb_clone(pack_referred2);
               misabsorb_clone(pack_copied);
               misabsorb_clone(pack_cloned);
               misabsorb_clone(pack_moved1);
               misabsorb_clone(pack_moved2);
               misabsorb_clone(pack_abandoned);
               misabsorb_clone(pack_disowned);
               return;
            }

            auto absorb_clone = [&](auto& a, const char* intent) {
               a.AssignAbsorb(Clone(*element));

               if constexpr (CT::Container<E>)
                  Any_CheckState_OwnedFull<TypeOf<E>>(*element);
               Any_Helper_TestSame(a, *element);
               REQUIRE(a.GetUses() == 2);
               REQUIRE(a.GetAllocation() == element->GetAllocation());

               BenchmarkStd(
                  std::string("Piecewise/") + intent + "/AssignAbsorb(Clone(" + static_cast<std::string>(NameOf<E>()) + "))", 30, 100,
                  a.AssignAbsorb(Clone(*element)),          a.AssignAbsorb(Clone(*originalElement)),
                  ::std::any temp_std1 = *element;
                  ::std::any temp_std2 = *originalElement,  temp_std1 = temp_std2
               );
            };

            absorb_clone(pack_referred1, "Refer");
            absorb_clone(pack_copied,    "Copy");
            absorb_clone(pack_cloned,    "Clone");
            absorb_clone(pack_moved1,    "Move");
            absorb_clone(pack_abandoned, "Abandon");
            absorb_clone(pack_disowned,  "Disown");
         }
      }

      WHEN("Assigned compatible copied value") {
         auto assign_copy = [&](auto& a, const char* intent) {
            a.Assign(Copy(*element));

            if constexpr (CT::Container<E>)
               Any_CheckState_OwnedFull<TypeOf<E>>(*element);
            Any_CheckState_OwnedFull<E>(a);
            Any_CheckState_ContainsOne(a, Refer(element));

            BenchmarkStd(
               std::string("Piecewise/") + intent + "/Assign(Copy(" + static_cast<std::string>(NameOf<E>()) + "))", 30, 100,
               a.Assign(Copy(*element)),        a.Assign(Copy(*originalElement)),
               ::std::any temp_std = *element,  temp_std = *originalElement
            );
         };

         assign_copy(pack_referred1, "Refer");
         assign_copy(pack_copied,    "Copy");
         assign_copy(pack_cloned,    "Clone");
         assign_copy(pack_moved1,    "Move");
         assign_copy(pack_abandoned, "Abandon");
         assign_copy(pack_disowned,  "Disown");
      }

      if constexpr (CT::ContainsOne<E>) {
         WHEN("Assigned and absorbed copied container") {
            if (not pack_referred1.IsSame(element->GetType())) {
               auto misabsorb_copy = [&](auto& a) {
                  REQUIRE_THROWS(a.AssignAbsorb(Copy(*element)));
                  Any_CheckState_OwnedFull<E>(a);
                  Any_CheckState_ContainsOne(a, Refer(originalElement));
               };

               misabsorb_copy(pack_referred1);
               misabsorb_copy(pack_referred2);
               misabsorb_copy(pack_copied);
               misabsorb_copy(pack_cloned);
               misabsorb_copy(pack_moved1);
               misabsorb_copy(pack_moved2);
               misabsorb_copy(pack_abandoned);
               misabsorb_copy(pack_disowned);
               return;
            }

            auto absorb_copy = [&](auto& a, const char* intent) {
               a.AssignAbsorb(Copy(*element));

               if constexpr (CT::Container<E>)
                  Any_CheckState_OwnedFull<TypeOf<E>>(*element);
               Any_Helper_TestSame(a, *element);
               REQUIRE(a.GetUses() == 2);
               REQUIRE(a.GetAllocation() == element->GetAllocation());

               BenchmarkStd(
                  std::string("Piecewise/") + intent + "/AssignAbsorb(Copy(" + static_cast<std::string>(NameOf<E>()) + "))", 30, 100,
                  a.AssignAbsorb(Copy(*element)),           a.AssignAbsorb(Copy(*originalElement)),
                  ::std::any temp_std1 = *element;
                  ::std::any temp_std2 = *originalElement,  temp_std1 = temp_std2
               );
            };

            absorb_copy(pack_referred1, "Refer");
            absorb_copy(pack_copied,    "Copy");
            absorb_copy(pack_cloned,    "Clone");
            absorb_copy(pack_moved1,    "Move");
            absorb_copy(pack_abandoned, "Abandon");
            absorb_copy(pack_disowned,  "Disown");
         }
      }

      WHEN("Assigned compatible moved value") {
         auto assign_move = [&](auto& a, const char* intent) {
            auto movable = *element;
            a.Assign(::std::move(movable));

            if constexpr (CT::Container<E>)
               Any_CheckState_Default<TypeOf<E>>(movable);
            Any_CheckState_OwnedFull<E>(a);
            Any_CheckState_ContainsOne(a, Refer(element));

            BenchmarkStd(
               std::string("Piecewise/") + intent + "/Assign(Move(" + static_cast<std::string>(NameOf<E>()) + "))", 30, 100,
               auto movable1 = *element;
               auto movable2 = *originalElement;
               a.Assign(Move(movable1)),                       a.Assign(Move(movable2)),
               auto movable1 = *element;
               auto movable2 = *originalElement;
               ::std::any temp_std = ::std::move(movable1),    temp_std = ::std::move(movable2)
            );
         };

         assign_move(pack_referred1, "Refer");
         assign_move(pack_copied,    "Copy");
         assign_move(pack_cloned,    "Clone");
         assign_move(pack_moved1,    "Move");
         assign_move(pack_abandoned, "Abandon");
         assign_move(pack_disowned,  "Disown");
      }

      if constexpr (CT::ContainsOne<E>) {
         WHEN("Assigned and absorbed moved container") {
            if (not pack_referred1.IsSame(element->GetType())) {
               auto misabsorb_move = [&](auto& a) {
                  auto movable = *element;
                  REQUIRE_THROWS(a.AssignAbsorb(::std::move(movable)));

                  Any_CheckState_OwnedFull<E>(a);
                  Any_CheckState_ContainsOne(a, Refer(originalElement));
                  Any_CheckState_OwnedFull<int>(movable);
                  REQUIRE(movable.GetUses() == 2);
                  REQUIRE(movable.template As<int>() == 555);
               };

               misabsorb_move(pack_referred1);
               misabsorb_move(pack_referred2);
               misabsorb_move(pack_copied);
               misabsorb_move(pack_cloned);
               misabsorb_move(pack_moved1);
               misabsorb_move(pack_moved2);
               misabsorb_move(pack_abandoned);
               misabsorb_move(pack_disowned);
               return;
            };

            auto absorb_move = [&](auto& a, const char* intent) {
               auto movable = *element;
               a.AssignAbsorb(::std::move(movable));

               if constexpr (CT::Container<E>)
                  Any_CheckState_Default<TypeOf<E>>(movable);
               Any_Helper_TestSame(a, *element);
               REQUIRE(a.GetUses() == 2);
               REQUIRE(a.GetAllocation() == element->GetAllocation());

               BenchmarkStd(
                  std::string("Piecewise/") + intent + "/AssignAbsorb(Move(" + static_cast<std::string>(NameOf<E>()) + "))", 30, 100,
                  T movable1 = *element;
                  T movable2 = *originalElement;
                  a.AssignAbsorb(Move(movable1)),           a.AssignAbsorb(Move(movable2)),
                  ::std::any movable1 = *element;
                  ::std::any movable2 = *originalElement,   movable1 = ::std::move(movable2)
               );
            };

            absorb_move(pack_referred1, "Refer");
            absorb_move(pack_copied,    "Copy");
            absorb_move(pack_cloned,    "Clone");
            absorb_move(pack_moved1,    "Move");
            absorb_move(pack_abandoned, "Abandon");
            absorb_move(pack_disowned,  "Disown");
         }
      }

      WHEN("Assigned compatible disowned value") {
         auto assign_disown = [&](auto& a, const char* intent) {
            a.Assign(Disown(*element));

            Any_CheckState_OwnedFull<E>(a);
            Any_CheckState_ContainsOne(a, Disown(element));

            BenchmarkStd(
               std::string("Piecewise/") + intent + "/Assign(Disown(" + static_cast<std::string>(NameOf<E>()) + "))", 30, 100,
               a.Assign(Disown(*element)),         a.Assign(Disown(*originalElement)),
               ::std::any temp_std = *element,     temp_std = *originalElement
            );
         };

         assign_disown(pack_referred1, "Refer");
         assign_disown(pack_copied,    "Copy");
         assign_disown(pack_cloned,    "Clone");
         assign_disown(pack_moved1,    "Move");
         assign_disown(pack_abandoned, "Abandon");
         assign_disown(pack_disowned,  "Disown");
      }

      if constexpr (CT::ContainsOne<E>) {
         WHEN("Assigned and absorbed disowned container") {
            if (not pack_referred1.IsSame(element->GetType())) {
               auto misabsorb_disown = [&](auto& a) {
                  REQUIRE_THROWS(a.AssignAbsorb(Disown(*element)));
                  Any_CheckState_OwnedFull<E>(a);
                  Any_CheckState_ContainsOne(a, Disown(originalElement));
               };

               misabsorb_disown(pack_referred1);
               misabsorb_disown(pack_referred2);
               misabsorb_disown(pack_copied);
               misabsorb_disown(pack_cloned);
               misabsorb_disown(pack_moved1);
               misabsorb_disown(pack_moved2);
               misabsorb_disown(pack_abandoned);
               misabsorb_disown(pack_disowned);
               return;
            }

            auto absorb_disown = [&](auto& a, const char* intent) {
               a.AssignAbsorb(Disown(*element));

               REQUIRE(a.GetRaw() == element->GetRaw());
               REQUIRE(a.IsExact(element->GetType()));
               REQUIRE(a == *element);
               REQUIRE(a.IsDeep() == element->IsDeep());
               REQUIRE(a.IsConstant() != element->IsConstant());
               REQUIRE(a.GetUnconstrainedState() == element->GetUnconstrainedState());
               REQUIRE(a.GetUses() == 0);
               REQUIRE_FALSE(a.GetAllocation());

               BenchmarkStd(
                  std::string("Piecewise/") + intent + "/AssignAbsorb(Disown(" + static_cast<std::string>(NameOf<E>()) + "))", 30, 100,
                  a.AssignAbsorb(Disown(*element)),         a.AssignAbsorb(Disown(*originalElement)),
                  ::std::any temp_std1 = *element;
                  ::std::any temp_std2 = *originalElement,  temp_std1 = temp_std2
               );
            };

            absorb_disown(pack_referred1, "Refer");
            absorb_disown(pack_copied,    "Copy");
            absorb_disown(pack_cloned,    "Clone");
            absorb_disown(pack_moved1,    "Move");
            absorb_disown(pack_abandoned, "Abandon");
            absorb_disown(pack_disowned,  "Disown");
         }
      }
      
      WHEN("Assigned compatible abandoned value") {
         auto assign_abandon = [&](auto& a, const char* intent) {
            auto movable = *element;
            a.Assign(Abandon(movable));

            if constexpr (CT::Container<E>)
               Any_CheckState_Abandoned<TypeOf<E>>(movable);
            Any_CheckState_OwnedFull<E>(a);
            Any_CheckState_ContainsOne(a, Refer(element));

            BenchmarkStd(
               std::string("Piecewise/") + intent + "/Assign(Abandon(" + static_cast<std::string>(NameOf<E>()) + "))", 30, 100,
               auto movable1 = *element;
               auto movable2 = *originalElement;
               a.Assign(Abandon(movable1)),                    a.Assign(Abandon(movable2)),
               auto movable1 = *element;
               auto movable2 = *originalElement;
               ::std::any temp_std = ::std::move(movable1),    temp_std = ::std::move(movable2)
            );
         };

         assign_abandon(pack_referred1, "Refer");
         assign_abandon(pack_copied,    "Copy");
         assign_abandon(pack_cloned,    "Clone");
         assign_abandon(pack_moved1,    "Move");
         assign_abandon(pack_abandoned, "Abandon");
         assign_abandon(pack_disowned,  "Disown");
      }

      if constexpr (CT::ContainsOne<E>) {
         WHEN("Assigned and absorbed abandoned container") {
            if (not pack_referred1.IsSame(element->GetType())) {
               auto misabsorb_abandon = [&](auto& a) {
                  auto movable = *element;
                  REQUIRE_THROWS(a.AssignAbsorb(Abandon(movable)));

                  Any_CheckState_OwnedFull<E>(a);
                  Any_CheckState_ContainsOne(a, Refer(originalElement));
                  Any_CheckState_OwnedFull<int>(movable);
                  REQUIRE(movable.GetUses() == 2);
                  REQUIRE(movable.template As<int>() == 555);
               };

               misabsorb_abandon(pack_referred1);
               misabsorb_abandon(pack_referred2);
               misabsorb_abandon(pack_copied);
               misabsorb_abandon(pack_cloned);
               misabsorb_abandon(pack_moved1);
               misabsorb_abandon(pack_moved2);
               misabsorb_abandon(pack_abandoned);
               misabsorb_abandon(pack_disowned);
               return;
            }

            auto absorb_abandon = [&](auto& a, const char* intent) {
               auto movable = *element;
               a.AssignAbsorb(Abandon(movable));

               if constexpr (CT::Container<E>)
                  Any_CheckState_Abandoned<TypeOf<E>>(movable);
               Any_Helper_TestSame(a, *element);
               REQUIRE(a.GetUses() == 2);
               REQUIRE(a.GetAllocation() == element->GetAllocation());

               BenchmarkStd(
                  std::string("Piecewise/") + intent + "/AssignAbsorb(Abandon(" + static_cast<std::string>(NameOf<E>()) + "))", 30, 100,
                  T movable1 = *element;
                  T movable2 = *originalElement;
                  a.AssignAbsorb(Abandon(movable1)),              a.AssignAbsorb(Abandon(movable2)),
                  ::std::any movable1 = *element;
                  ::std::any movable2 = *originalElement;
                  ::std::any temp_std = ::std::move(movable1),    temp_std = ::std::move(movable2)
               );
            };

            absorb_abandon(pack_referred1, "Refer");
            absorb_abandon(pack_copied,    "Copy");
            absorb_abandon(pack_cloned,    "Clone");
            absorb_abandon(pack_moved1,    "Move");
            absorb_abandon(pack_abandoned, "Abandon");
            absorb_abandon(pack_disowned,  "Disown");
         }
      }

      WHEN("Assigned compatible empty self") {
         #define assign_empty_self(a) \
            a = T{}; \
            Any_CheckState_Default<E>(a);

         assign_empty_self(pack_referred1);
         assign_empty_self(pack_referred2);
         assign_empty_self(pack_copied);
         assign_empty_self(pack_cloned);
         assign_empty_self(pack_moved1);
         assign_empty_self(pack_moved2);
         assign_empty_self(pack_abandoned);
         assign_empty_self(pack_disowned);
      }

      WHEN("Assigned compatible full self") {
         #define assign_full_self(a) { \
            auto backup = a; \
            const auto uses_before = a.GetUses(); \
            LglsDisableWarningPush \
               LglsDisableWarning_SelfAssign \
               a = a; \
            LglsDisableWarningPop \
            Any_Helper_TestSame(a, backup); \
            REQUIRE(a.GetUses() == uses_before); \
         }

         assign_full_self(pack_referred1);
         assign_full_self(pack_referred2);
         assign_full_self(pack_copied);
         assign_full_self(pack_cloned);
         assign_full_self(pack_moved1);
         assign_full_self(pack_moved2);
         assign_full_self(pack_abandoned);
         assign_full_self(pack_disowned);
      }

      WHEN("Absorbed by referral") {
         #define absorb_construct_refer(a) { \
            T absorbed1 {a}; \
            T absorbed2{Refer {a}}; \
            Any_Helper_TestSame(absorbed1, a); \
            Any_Helper_TestSame(absorbed2, a); \
            REQUIRE(absorbed1.GetUses() == 3); \
            REQUIRE(absorbed2.GetUses() == 3); \
         }

         absorb_construct_refer(pack_referred1);
         absorb_construct_refer(pack_referred2);
         absorb_construct_refer(pack_copied);
         absorb_construct_refer(pack_cloned);
         absorb_construct_refer(pack_moved1);
         absorb_construct_refer(pack_moved2);
         absorb_construct_refer(pack_abandoned);
         absorb_construct_refer(pack_disowned);
      }
      
      WHEN("Absorbed by move") {
         #define absorb_construct_move1(a) { \
            T backup = a; \
            T absorbed {::std::move(a)}; \
            Any_CheckState_Default<E>(a); \
            Any_CheckState_OwnedFull<E>(absorbed); \
            Any_Helper_TestSame(absorbed, backup); \
            REQUIRE(absorbed.GetUses() == 2); \
         }

         absorb_construct_move1(pack_referred1);
         absorb_construct_move1(pack_referred2);
         absorb_construct_move1(pack_copied);
         absorb_construct_move1(pack_cloned);
         absorb_construct_move1(pack_moved1);
         absorb_construct_move1(pack_moved2);
         absorb_construct_move1(pack_abandoned);
         absorb_construct_move1(pack_disowned);
      }
      
      WHEN("Absorbed by move (alt)") {
         #define absorb_construct_move2(a) { \
            T backup = a; \
            T absorbed {Move(a)}; \
            Any_CheckState_Default<E>(a); \
            Any_CheckState_OwnedFull<E>(absorbed); \
            Any_Helper_TestSame(absorbed, backup); \
            REQUIRE(absorbed.GetUses() == 2); \
         }

         absorb_construct_move2(pack_referred1);
         absorb_construct_move2(pack_referred2);
         absorb_construct_move2(pack_copied);
         absorb_construct_move2(pack_cloned);
         absorb_construct_move2(pack_moved1);
         absorb_construct_move2(pack_moved2);
         absorb_construct_move2(pack_abandoned);
         absorb_construct_move2(pack_disowned);
      }
      
      WHEN("Absorbed by abandon") {
         #define absorb_construct_abandon(a) { \
            T backup = a; \
            T absorbed {Abandon {a}}; \
            Any_CheckState_Abandoned<E>(a); \
            Any_CheckState_OwnedFull<E>(absorbed); \
            Any_Helper_TestSame(absorbed, backup); \
            REQUIRE(absorbed.GetUses() == 2); \
         }

         absorb_construct_abandon(pack_referred1);
         absorb_construct_abandon(pack_referred2);
         absorb_construct_abandon(pack_copied);
         absorb_construct_abandon(pack_cloned);
         absorb_construct_abandon(pack_moved1);
         absorb_construct_abandon(pack_moved2);
         absorb_construct_abandon(pack_abandoned);
         absorb_construct_abandon(pack_disowned);
      }
      
      WHEN("Absorbed by disown") {
         #define absorb_construct_disown(a) { \
            T absorbed {Disown {a}}; \
            Any_CheckState_OwnedFull<E>(a); \
            Any_CheckState_DisownedFull<E>(absorbed); \
            REQUIRE(absorbed.GetRaw() == a.GetRaw()); \
            REQUIRE(absorbed.IsExact(a.GetType())); \
            REQUIRE(absorbed == a); \
            REQUIRE(absorbed.IsDeep() == a.IsDeep()); \
            REQUIRE(absorbed.IsConstant() != a.IsConstant()); \
            REQUIRE(absorbed.GetUnconstrainedState() == a.GetUnconstrainedState()); \
            REQUIRE(absorbed.GetUses() == 0); \
         }

         absorb_construct_disown(pack_referred1);
         absorb_construct_disown(pack_referred2);
         absorb_construct_disown(pack_copied);
         absorb_construct_disown(pack_cloned);
         absorb_construct_disown(pack_moved1);
         absorb_construct_disown(pack_moved2);
         absorb_construct_disown(pack_abandoned);
         absorb_construct_disown(pack_disowned);
      }
      
      WHEN("Absorbed by copy") {
         #define absorb_construct_copy(a, entry_refs) { \
            T absorbed {Copy {a}}; \
            Any_CheckState_OwnedFull<E>(a); \
            Any_CheckState_OwnedFull<E>(absorbed); \
            REQUIRE(absorbed == a); \
            REQUIRE(absorbed.GetRaw() != a.GetRaw()); \
            REQUIRE(absorbed.template As<E>() == a.template As<E>()); \
            if constexpr (CT::Sparse<E>) { \
               auto entry = *absorbed.GetEntries(); \
               if ((entry_refs) == 0) \
                  REQUIRE(entry == nullptr); \
               if (entry) { \
                  REQUIRE(entry->GetUses() == (entry_refs)); \
                  if constexpr (CT::Referenced<Decay<E>>) { \
                     auto e = absorbed.template As<E>(); \
                     REQUIRE(DenseCast(e).GetReferences() == (entry_refs)); \
                  } \
               } \
               else { \
                  if constexpr (CT::Referenced<Decay<E>>) { \
                     auto e = absorbed.template As<E>(); \
                     REQUIRE(DenseCast(e).GetReferences() == (managed_sparse ? 7 : 1)); \
                  } \
               } \
            } \
            REQUIRE(absorbed.GetUses() == 1); \
            REQUIRE(a.GetUses() == 1); \
         }

         const bool managed_sparse = CT::Sparse<E> and Managed;
         absorb_construct_copy(pack_referred1, managed_sparse ? 8 : 3);
         absorb_construct_copy(pack_referred2, managed_sparse ? 8 : 3);
         absorb_construct_copy(pack_copied,    managed_sparse ? 8 : 3);
         absorb_construct_copy(pack_cloned,    2);
         absorb_construct_copy(pack_moved1,    managed_sparse ? 8 : 1);
         absorb_construct_copy(pack_moved2,    managed_sparse ? 8 : 1);
         absorb_construct_copy(pack_abandoned, managed_sparse ? 8 : 1);
         absorb_construct_copy(pack_disowned,  0);
      }
      
      WHEN("Absorbed by clone") {
         #define absorb_construct_clone(a) { \
            T absorbed {Clone {a}}; \
            Any_CheckState_OwnedFull<E>(a); \
            Any_CheckState_OwnedFull<E>(absorbed); \
            REQUIRE((absorbed == a) == CT::Dense<E>); \
            REQUIRE(absorbed.GetUses() == 1); \
         }

         absorb_construct_clone(pack_referred1);
         absorb_construct_clone(pack_referred2);
         absorb_construct_clone(pack_copied);
         absorb_construct_clone(pack_cloned);
         absorb_construct_clone(pack_moved1);
         absorb_construct_clone(pack_moved2);
         absorb_construct_clone(pack_abandoned);
         absorb_construct_clone(pack_disowned);
      }
      
      WHEN("Emplace (overwrite)") {
         auto emplace_overwrite = [&](auto& a, const char* intent) {
            ScopedE i666{666};
            const auto i666backup = *i666;
            decltype(auto) instance = a.Emplace(::std::move(*i666));

            Any_CheckState_OwnedFull<E>(a);
            REQUIRE(instance.CompareOneEqual(i666backup));
            REQUIRE(a.GetCount() == 1);
            REQUIRE(a.GetReserved() >= 1);
            if constexpr (CT::Typed<T>) {
               REQUIRE(*a == i666backup);
               REQUIRE(&*a == &*instance);
            }

            Benchmark(
               std::string("Piecewise/") + intent + "/Emplace(" + static_cast<std::string>(NameOf<E>()) + ")", 30,
               auto movable1 = *element;
               auto movable2 = *originalElement;
               a.Emplace(::std::move(movable1)),      a.Emplace(::std::move(movable2))
            );
         };

         emplace_overwrite(pack_referred1, "Refer");
         emplace_overwrite(pack_copied,    "Copy");
         emplace_overwrite(pack_cloned,    "Clone");
         emplace_overwrite(pack_moved1,    "Move");
         emplace_overwrite(pack_abandoned, "Abandon");
         emplace_overwrite(pack_disowned,  "Disown");
      }

      WHEN("Emplace (overwrite, describe)") {
         auto emplace_overwrite_describe = [&](auto& a, const char* intent) {
            ScopedE i666{666};
            const auto i666backup = *i666;
            Many descriptor {Piecewise, ::std::move(*i666)};

            if constexpr (CT::DescribeConstructible<E> and not CT::Container<T>) {
               decltype(auto) instance = a.Emplace(Describe{descriptor});

               Any_CheckState_OwnedFull<E>(a);
               REQUIRE(instance.CompareOneEqual(i666backup));
               REQUIRE(a.GetCount() == 1);
               REQUIRE(a.GetReserved() >= 1);

               Benchmark(
                  std::string("Piecewise/") + intent + "/Emplace(Describe(" + static_cast<std::string>(NameOf<E>()) + "))", 30,
                  auto movable1 = *element;
                  a.Emplace(::std::move(movable1)),      a.Emplace(Describe{descriptor})
               );
            }
            else if constexpr (CT::TypeErased<T>) {
               REQUIRE_THROWS(a.Emplace(Describe{descriptor}));

               Any_CheckState_Default<E>(a, true);
            }
         };

         emplace_overwrite_describe(pack_referred1, "Refer");
         emplace_overwrite_describe(pack_copied,    "Copy");
         emplace_overwrite_describe(pack_cloned,    "Clone");
         emplace_overwrite_describe(pack_moved1,    "Move");
         emplace_overwrite_describe(pack_abandoned, "Abandon");
         emplace_overwrite_describe(pack_disowned,  "Disown");
      }
      
      WHEN("Cleared") {
         auto clear_full = [&](auto& a, const char* intent) {
            CAPTURE(intent);
            BenchmarkStd(
               std::string("Piecewise/") + intent + "/Clear(" + static_cast<std::string>(NameOf<E>()) + ")", 30, 100,
               T temp = a,                      temp.Clear(),
               ::std::any temp_std = *element,  temp_std.reset()
            );

            a.Clear();

            Any_CheckState_OwnedEmpty<E>(a);
         };

         clear_full(pack_referred1, "Refer");
         clear_full(pack_copied,    "Copy");
         clear_full(pack_cloned,    "Clone");
         clear_full(pack_moved1,    "Move");
         clear_full(pack_abandoned, "Abandon");
         clear_full(pack_disowned,  "Disown");
      }

      WHEN("Reset") {
         auto reset_full = [&](auto& a, const char* intent) {
            BenchmarkStd(
               std::string("Piecewise/") + intent + "/Reset(" + static_cast<std::string>(NameOf<E>()) + ")", 30, 100,
               T temp = a,                      temp.Reset(),
               ::std::any temp_std = *element,  temp_std.reset()
            );

            a.Reset();

            Any_CheckState_Default<E>(a);
         };

         reset_full(pack_referred1, "Refer");
         reset_full(pack_copied,    "Copy");
         reset_full(pack_cloned,    "Clone");
         reset_full(pack_moved1,    "Move");
         reset_full(pack_abandoned, "Abandon");
         reset_full(pack_disowned,  "Disown");
      }

      if constexpr (LANGULUS_FEATURE(MANAGED_MEMORY) and not CT::Container<E>) {
         // Works only if E doesn't move entries around                 
         WHEN("Reset, and then immediately allocated again") {
            #define reset_and_reallocate(a) { \
               const auto memory = a.GetRaw(); \
               a.Reset(); \
               a = *element; \
               REQUIRE(a.GetRaw() == memory); \
            }

            reset_and_reallocate(pack_referred1);
            reset_and_reallocate(pack_referred2);
            reset_and_reallocate(pack_copied);
            reset_and_reallocate(pack_cloned);
            reset_and_reallocate(pack_moved1);
            reset_and_reallocate(pack_moved2);
            reset_and_reallocate(pack_abandoned);
            //reset_and_reallocate(pack_disowned); // likely to be reallocated in a new place due to lack of authority on the original memory
         }
      }

      WHEN("Compared") {
         ScopedE e1 {1};
         T another_pack1 {Piecewise, *e1};
         T defaulted_pack;

         auto compared_full = [&](auto& a, const char* intent) {
            T same_pack {a};

            REQUIRE      (a != another_pack1);
            REQUIRE_FALSE(a == another_pack1);
            REQUIRE      (a != defaulted_pack);
            REQUIRE_FALSE(a == defaulted_pack);
            REQUIRE      (a == same_pack);
            REQUIRE_FALSE(a != same_pack);

            [[maybe_unused]] volatile bool dont_optimize = false;
            BenchmarkStd(
               std::string("Piecewise/") + intent + "/operator==(" + static_cast<std::string>(NameOf<E>()) + ")", 30, 100,
               (void) 0,                                    dont_optimize |= (a == same_pack),
               const ::std::any a_std = *element;
               const ::std::any another_pack1_std = *e1,    dont_optimize |= (std::any_cast<E const&>(a_std) == std::any_cast<E const&>(another_pack1_std))
            );
            BenchmarkStd(
               std::string("Piecewise/") + intent + "/operator!=(" + static_cast<std::string>(NameOf<E>()) + ")", 30, 100,
               (void) 0,                                    dont_optimize |= (a != same_pack),
               const ::std::any a_std = *element;
               const ::std::any another_pack1_std = *e1,    dont_optimize |= (std::any_cast<E const&>(a_std) != std::any_cast<E const&>(another_pack1_std))
            );
         };

         compared_full(pack_referred1, "Refer");
         compared_full(pack_copied,    "Copy");
         compared_full(pack_cloned,    "Clone");
         compared_full(pack_moved1,    "Move");
         compared_full(pack_abandoned, "Abandon");
         compared_full(pack_disowned,  "Disown");
      }

      WHEN("Contains when full") {
         ScopedE e1 {1};
         
         #define contains_full(a) { \
            REQUIRE      (a.Contains(*originalElement)); \
            REQUIRE_FALSE(a.Contains(*e1)); \
         }

         contains_full(pack_referred1);
         contains_full(pack_referred2);
         contains_full(pack_copied);

         if constexpr (CT::Sparse<E>) {
            REQUIRE      (pack_cloned.GetDense().Contains(DenseCast(*originalElement)));
            REQUIRE_FALSE(pack_cloned.Contains(*originalElement));
            REQUIRE_FALSE(pack_cloned.Contains(*e1));
         }
         else contains_full(pack_cloned);

         contains_full(pack_moved1);
         contains_full(pack_moved2);
         contains_full(pack_abandoned);
         contains_full(pack_disowned);

         [[maybe_unused]] volatile bool dont_optimize = false;
         Benchmark("Piecewise/Contains(" + NameOf<E>() + ")", 30,
            (void) 0, dont_optimize |= pack_referred1.Contains(*element)
         );
      }
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
         Any_CheckState_ContainsOne(pack2, Refer(e1));

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
         T movable = pack1;
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
         T movable = pack1;
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
         T movable = pack1;
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

      WHEN("Compared") {
         T defaulted_pack;

         REQUIRE      (pack1 != pack2);
         REQUIRE_FALSE(pack1 == pack2);
         REQUIRE      (pack1 != defaulted_pack);
         REQUIRE_FALSE(pack1 == defaulted_pack);
         REQUIRE      (pack2 != defaulted_pack);
         REQUIRE_FALSE(pack2 == defaulted_pack);

         if constexpr (CT::Deep<E> and CT::Dense<E>) {
            static_assert(     T{} == E{} );
            static_assert(not (T{} != E{}));
            static_assert(     E{} == T{} );
            static_assert(not (E{} != T{}));
         }
         else {
            static_assert(     T{} != E{} );
            static_assert(not (T{} == E{}));
            static_assert(     E{} != T{} );
            static_assert(not (E{} == T{}));
         }
      }
   }

   REQUIRE(memoryState.Assert());
   REQUIRE_FALSE(Allocator::CollectGarbage());
}
