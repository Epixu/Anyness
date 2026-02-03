///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#include "TestManyCommon.hpp"
#include <Langulus/Anyness/Many.hpp>


TEST_CASE_TEMPLATE("Test empty Many/TMany", TestType
   , Types<Many, Text**, ScopedElement<Text**>>

   // Elements are not allocated by the memory manager                  
   , Types<Many, Text,   ScopedElement<Text>>
   , Types<Many, int,    ScopedElement<int>>
   , Types<Many, Many, ScopedElement<Many>>
   , Types<Many, RT,     ScopedElement<RT>>
   , Types<Many, char,   ScopedElement<char>>

   , Types<Many, Text*,  ScopedElement<Text*>>
   , Types<Many, int*,   ScopedElement<int*>>
   , Types<Many, Many*,  ScopedElement<Many*>>
   , Types<Many, RT*,    ScopedElement<RT*>>
   , Types<Many, char*,  ScopedElement<char*>>

   , Types<Many, int**,  ScopedElement<int**>>
   , Types<Many, Many**, ScopedElement<Many**>>
   , Types<Many, RT**,   ScopedElement<RT**>>
   , Types<Many, char**, ScopedElement<char**>>

   , Types<TMany<Text>,   Text,   ScopedElement<Text>>
   , Types<TMany<int>,    int,    ScopedElement<int>>
   , Types<TMany<Many>,   Many,   ScopedElement<Many>>
   , Types<TMany<RT>,     RT,     ScopedElement<RT>>
   , Types<TMany<char>,   char,   ScopedElement<char>>

   , Types<TMany<Text*>,  Text*,  ScopedElement<Text*>>
   , Types<TMany<int*>,   int*,   ScopedElement<int*>>
   , Types<TMany<Many*>,  Many*,  ScopedElement<Many*>>
   , Types<TMany<RT*>,    RT*,    ScopedElement<RT*>>
   , Types<TMany<char*>,  char*,  ScopedElement<char*>>

   , Types<TMany<Text**>, Text**, ScopedElement<Text**>>
   , Types<TMany<int**>,  int**,  ScopedElement<int**>>
   , Types<TMany<Many**>, Many**, ScopedElement<Many**>>
   , Types<TMany<RT**>,   RT**,   ScopedElement<RT**>>
   , Types<TMany<char**>, char**, ScopedElement<char**>>

   #if LANGULUS_FEATURE(MANAGED_MEMORY)
   // Elements are allocated by the memory manager                      
   , Types<Many, Text,   ScopedElement<Text, true>>
   , Types<Many, int,    ScopedElement<int, true>>
   , Types<Many, Many,   ScopedElement<Many, true>>
   , Types<Many, RT,     ScopedElement<RT, true>>
   , Types<Many, char,   ScopedElement<char, true>>

   , Types<Many, Text*,  ScopedElement<Text*, true>>
   , Types<Many, int*,   ScopedElement<int*, true>>
   , Types<Many, Many*,  ScopedElement<Many*, true>>
   , Types<Many, RT*,    ScopedElement<RT*, true>>
   , Types<Many, char*,  ScopedElement<char*, true>>

   , Types<Many, Text**, ScopedElement<Text**, true>>
   , Types<Many, int**,  ScopedElement<int**, true>>
   , Types<Many, Many**, ScopedElement<Many**, true>>
   , Types<Many, RT**,   ScopedElement<RT**, true>>
   , Types<Many, char**, ScopedElement<char**, true>>

   , Types<TMany<Text>,   Text,   ScopedElement<Text, true>>
   , Types<TMany<int>,    int,    ScopedElement<int, true>>
   , Types<TMany<Many>,   Many,   ScopedElement<Many, true>>
   , Types<TMany<RT>,     RT,     ScopedElement<RT, true>>
   , Types<TMany<char>,   char,   ScopedElement<char, true>>

   , Types<TMany<Text*>,  Text*,  ScopedElement<Text*, true>>
   , Types<TMany<int*>,   int*,   ScopedElement<int*, true>>
   , Types<TMany<Many*>,  Many*,  ScopedElement<Many*, true>>
   , Types<TMany<RT*>,    RT*,    ScopedElement<RT*, true>>
   , Types<TMany<char*>,  char*,  ScopedElement<char*, true>>

   , Types<TMany<Text**>, Text**, ScopedElement<Text**, true>>
   , Types<TMany<int**>,  int**,  ScopedElement<int**, true>>
   , Types<TMany<Many**>, Many**, ScopedElement<Many**, true>>
   , Types<TMany<RT**>,   RT**,   ScopedElement<RT**, true>>
   , Types<TMany<char**>, char**, ScopedElement<char**, true>>

   // Packed pointers                                                   
   , Types<Many, pptr8,  ScopedElementPacked<pptr8>>
   , Types<Many, pptr16, ScopedElementPacked<pptr16>>
   , Types<Many, pptr32, ScopedElementPacked<pptr32>>

   , Types<TMany<pptr8>,  pptr8,  ScopedElementPacked<pptr8>>
   , Types<TMany<pptr16>, pptr16, ScopedElementPacked<pptr16>>
   , Types<TMany<pptr32>, pptr32, ScopedElementPacked<pptr32>>
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
   
   {
      static_assert(CT::Deep<T>);
      static_assert(not CT::ContainsOne<T>);
      static_assert(CT::ContainsMany<T>);
      static_assert(CT::HasVariableCount<T>);
      static_assert(CT::HeapAllocated<T>);
      static_assert(CT::DeeplyOwned<T> == (CT::TypeErased<T> or CT::Sparse<TypeOf<T>>));
      static_assert(CT::Owned<T>);
      static_assert(CT::AutoOwned<T>);
      static_assert(CT::Comparable<T, T>);
      static_assert(CT::Comparable<T, E>);

      static_assert(::std::input_or_output_iterator<decltype(Fake<T>().begin())>);
      static_assert(::std::input_or_output_iterator<decltype(Fake<T>().end())>);

      static_assert(::std::ranges::range<T>);

      T test;
      for (auto& it : IterateInReverse(test)) {
         (void) it;
      }
      for (auto& it : test) {
         (void) it;
      }
      for (auto& it : IterateDefault(test)) {
         (void) it;
      }
      for (auto& it : IterateNoDeref(test)) {
         (void) it;
      }

      static_assert(requires (T pack)         { pack.operator +   (pack); });
      static_assert(requires (T pack, E item) { pack.operator +   (item); });
      static_assert(requires (T pack)         { pack.operator +=  (pack); });
      static_assert(requires (T pack, E item) { pack.operator +=  (item); });
      static_assert(requires (T pack, E item) { pack.operator <<  (item); });
      static_assert(requires (T pack, E item) { pack.operator >>  (item); });
      static_assert(requires (T pack, E item) { pack.operator <<= (item); });
      static_assert(requires (T pack, E item) { pack.operator >>= (item); });
      static_assert(requires (T pack, E item) { pack.InsertAt(Index::Back, item); });
      static_assert(requires (T pack, E item) { pack.EmplaceAt(Index::Back, item); });
      static_assert(requires (T pack, E item) { pack.Remove(item); });
      static_assert(requires (T pack, E item) { pack.RemoveAt(Index::Front); });
      static_assert(requires (T pack, E item) { pack.Reserve(20); });
      static_assert(requires (T pack, E item) { pack.EnableOr(); });
      static_assert(requires (T pack, E item) { pack.IsOr(); });
      static_assert(requires (T pack, E item) { pack.Find(item); });
      static_assert(requires (T pack, E item) { pack.ForEach([](const int&) {}); });
      static_assert(requires (T pack, E item) { pack.ForEachRev([](const int&) {}); });
   }

   constexpr bool Ambiguous = not Same<T, E> and CT::Deep<E> and CT::Dense<E> and LANGULUS(SAFE);
   
   GIVEN("Gap test") {
      alignas(T) char unininitialized[sizeof(T)];
      memset(unininitialized, 254, sizeof(unininitialized));
      new (unininitialized) T {};
      for (auto b : unininitialized) {
         REQUIRE(b != 254);
      }
      Logger::Info("Size of ", NameOf<::std::vector<E>>(), " container is: ", sizeof(::std::vector<E>), " bytes");
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
      //static_assert(sizeof(T) <= sizeof(::std::vector<E>)); // bigger, because it precomputes and stores a hash on the stack
   }

   GIVEN("Default-constructed container") {
      const ScopedE element {555};
      T pack;

      WHEN("Default-constructed") {
         Many_CheckState_Default<E>(pack);
      
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

         Many_CheckState_OwnedFull<E>(pack);
         Many_CheckState_ContainsOne(pack, Refer(element));

         BenchmarkStd("Empty/Assign(Refer(" + NameOf<E>() + "))", 30, 100,
            T temp,                 temp.Assign(*element),
            ::std::any temp_std,    temp_std = *element
         );
      }

      if constexpr (CT::Deep<E> and CT::Dense<E>) {
         WHEN("Assigned and absorbed by referral") {
            if (CT::Typed<T> and not pack.IsSame(element->GetType())) {
               const auto element_backup = *element;
               REQUIRE_THROWS(pack.AssignAbsorb(*element));
               Many_CheckState_Default<E>(pack);
               Many_Helper_TestSame(element_backup, *element);
               return;
            }

            pack.AssignAbsorb(*element);

            Many_Helper_TestSame(pack, *element);
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

         Many_CheckState_OwnedFull<E>(pack);
         Many_CheckState_ContainsOne(pack, Refer(element));

         BenchmarkStd("Empty/Assign(Move(" + NameOf<E>() + "))", 30, 100,
            auto movable = *element; T temp,                temp.Assign(::std::move(movable)),
            auto movable = *element; ::std::any temp_std,   temp_std = ::std::move(movable)
         );
      }

      if constexpr (CT::Deep<E> and CT::Dense<E>) {
         WHEN("Assigned and absorbed by move") {
            auto movable = *element;

            if (CT::Typed<T> and not pack.IsSame(element->GetType())) {
               REQUIRE_THROWS(pack.AssignAbsorb(::std::move(movable)));
               Many_CheckState_Default<E>(pack);
               Many_Helper_TestSame(movable, *element);
               return;
            }

            pack.AssignAbsorb(::std::move(movable));

            if constexpr (CT::Container<E>)
               Any_CheckState_Default<TypeOf<E>>(movable);
            Many_Helper_TestSame(pack, *element);
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

         Many_CheckState_OwnedFull<E>(pack);
         Many_CheckState_ContainsOne(pack, Copy(element));

         BenchmarkStd("Empty/Assign(Copy(" + NameOf<E>() + "))", 30, 100,
            T temp,                 temp.Assign(Copy(*element)),
            ::std::any temp_std,    temp_std = *element
         );
      }

      if constexpr (CT::Deep<E> and CT::Dense<E>) {
         WHEN("Assigned and absorbed copied value") {
            if (CT::Typed<T> and not pack.IsSame(element->GetType())) {
               const auto element_backup = *element;
               REQUIRE_THROWS(pack.AssignAbsorb(Copy(*element)));
               Many_CheckState_Default<E>(pack);
               Many_Helper_TestSame(element_backup, *element);
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

         Many_CheckState_OwnedFull<E>(pack);
         Many_CheckState_ContainsOne(pack, Clone(element));

         BenchmarkStd("Empty/Assign(Clone(" + NameOf<E>() + "))", 30, 100,
            T temp,                 temp.Assign(Clone(*element)),
            ::std::any temp_std,    temp_std = *element
         );
      }

      if constexpr (CT::Deep<E> and CT::Dense<E>) {
         WHEN("Assigned and absorbed cloned value") {
            if (CT::Typed<T> and not pack.IsSame(element->GetType())) {
               const auto element_backup = *element;
               REQUIRE_THROWS(pack.AssignAbsorb(Clone(*element)));
               Many_CheckState_Default<E>(pack);
               Many_Helper_TestSame(element_backup, *element);
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

         Many_CheckState_OwnedFull<E>(pack);
         Many_CheckState_ContainsOne(pack, Disown(element));

         BenchmarkStd("Empty/Assign(Disown(" + NameOf<E>() + "))", 30, 100,
            T temp,                 temp.Assign(Disown(*element)),
            ::std::any temp_std,    temp_std = *element
         );
      }

      if constexpr (CT::Deep<E> and CT::Dense<E>) {
         WHEN("Assigned and absorbed disowned value") {
            if (CT::Typed<T> and not pack.IsSame(element->GetType())) {
               const auto element_backup = *element;
               REQUIRE_THROWS(pack.AssignAbsorb(Disown(*element)));
               Many_CheckState_Default<E>(pack);
               Many_Helper_TestSame(element_backup, *element);
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
            Many_CheckState_Abandoned<E>(movable);
         Many_CheckState_OwnedFull<E>(pack);
         Many_CheckState_ContainsOne(pack, Refer(element));

         BenchmarkStd("Empty/Assign(Abandon(" + NameOf<E>() + "))", 30, 100,
            auto movable = *element; T temp,                temp.Assign(Abandon(movable)),
            auto movable = *element; ::std::any temp_std,   temp_std = ::std::move(movable)
         );
      }

      if constexpr (CT::Deep<E> and CT::Dense<E>) {
         WHEN("Assigned and absorbed abandoned value") {
            auto movable = *element;

            if (CT::Typed<T> and not pack.IsSame(element->GetType())) {
               REQUIRE_THROWS(pack.AssignAbsorb(Abandon(movable)));
               Many_CheckState_Default<E>(pack);
               Many_Helper_TestSame(movable, *element);
               return;
            }

            pack.AssignAbsorb(Abandon(movable));

            if constexpr (CT::Container<E>)
               Many_CheckState_Abandoned<E>(movable);
            Many_Helper_TestSame(pack, *element);
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

         Many_CheckState_Default<E>(pack);
      }

      WHEN("Emplace (insert)") {
         ScopedE i666 {666};
         const auto i666backup = *i666;
         decltype(auto) instance = pack.Emplace(::std::move(*i666));
         Many_CheckState_OwnedFull<E>(pack);
         if constexpr (CT::Handle<decltype(instance)>)
            REQUIRE(instance.CompareOneEqual(i666backup));
         else
            REQUIRE(instance == i666backup);
         REQUIRE(pack.GetCount() == 1);
         REQUIRE(pack.GetReserved() >= 1);

         if constexpr (CT::Typed<T>) {
            REQUIRE(*pack == i666backup);
            if constexpr (CT::Handle<decltype(instance)>)
               REQUIRE(&*pack == &*instance);
            else
               REQUIRE(&*pack == &instance);
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
            Many_CheckState_OwnedFull<E>(pack);
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
            Many_CheckState_Default<E>(pack, true);
         }
      }

      WHEN("Cleared") {
         pack.Clear();

         Many_CheckState_Default<E>(pack);

         BenchmarkStd("Empty/Clear(" + NameOf<E>() + ")", 30, 100,
            T temp,                 temp.Clear(),
            ::std::any temp_std,    temp_std.reset()
         );
      }

      WHEN("Reset") {
         pack.Reset();

         Many_CheckState_Default<E>(pack);

         BenchmarkStd("Empty/Reset(" + NameOf<E>() + ")", 30, 100,
            T temp,                 temp.Reset(),
            ::std::any temp_std,    temp_std.reset()
         );
      }

      WHEN("Referred empty") {
         T refer1 = pack;

         Many_Helper_TestSame(refer1, pack);
         Many_CheckState_Default<E>(refer1);
         Many_CheckState_Default<E>(pack);

         T refer2 = Refer(pack);

         Many_Helper_TestSame(refer2, pack);
         Many_CheckState_Default<E>(refer2);
         Many_CheckState_Default<E>(pack);
      }

      WHEN("Cloned empty") {
         T clone = Clone(pack);

         Many_Helper_TestSame(clone, pack);
         Many_CheckState_Default<E>(clone);
         Many_CheckState_Default<E>(pack);
      }

      WHEN("Disowned empty") {
         T disowned = Disown(pack);

         Many_Helper_TestSame(disowned, pack);
         Many_CheckState_Default<E>(disowned);
         Many_CheckState_Default<E>(pack);
      }

      WHEN("Copied empty") {
         T copy = Copy(pack);

         Many_Helper_TestSame(copy, pack);
         Many_CheckState_Default<E>(copy);
         Many_CheckState_Default<E>(pack);
      }

      WHEN("Moved empty") {
         T movable1 = pack;
         const T moved1 = ::std::move(movable1);

         Many_CheckState_Default<E>(movable1);
         Many_Helper_TestSame(moved1, pack);
         Many_CheckState_Default<E>(moved1);
         Many_CheckState_Default<E>(pack);

         T movable2 = pack;
         const T moved2 = Move(movable2);

         Many_CheckState_Default<E>(movable2);
         Many_Helper_TestSame(moved2, pack);
         Many_CheckState_Default<E>(moved2);
         Many_CheckState_Default<E>(pack);
      }

      WHEN("Abandoned empty") {
         T movable = pack;
         const T moved = Abandon(movable);

         Many_CheckState_Default<E>(movable);
         Many_Helper_TestSame(moved, pack);
         Many_CheckState_Default<E>(moved);
         Many_CheckState_Default<E>(pack);
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

   REQUIRE(memoryState.Assert());
   REQUIRE_FALSE(Allocator::CollectGarbage());
}
