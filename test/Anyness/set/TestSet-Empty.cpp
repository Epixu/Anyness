///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#include "TestSetCommon.hpp"
#include <Langulus/Anyness/Many.hpp>


TEST_CASE_TEMPLATE("Test empty Set/TSet", TestType
   // Elements are not allocated by the memory manager                  
   , Types<Set, Text,   ScopedElement<Text>>
   , Types<Set, int,    ScopedElement<int>>
   , Types<Set, Any,    ScopedElement<Any>>
   , Types<Set, RT,     ScopedElement<RT>>
   , Types<Set, char,   ScopedElement<char>>

   , Types<Set, Text*,  ScopedElement<Text*>>
   , Types<Set, int*,   ScopedElement<int*>>
   , Types<Set, Any*,   ScopedElement<Any*>>
   , Types<Set, RT*,    ScopedElement<RT*>>
   , Types<Set, char*,  ScopedElement<char*>>

   , Types<Set, Text**, ScopedElement<Text**>>
   , Types<Set, int**,  ScopedElement<int**>>
   , Types<Set, Any**,  ScopedElement<Any**>>
   , Types<Set, RT**,   ScopedElement<RT**>>
   , Types<Set, char**, ScopedElement<char**>>

   , Types<TSet<Text>,   Text,   ScopedElement<Text>>
   , Types<TSet<int>,    int,    ScopedElement<int>>
   , Types<TSet<Any>,    Any,    ScopedElement<Any>>
   , Types<TSet<RT>,     RT,     ScopedElement<RT>>
   , Types<TSet<char>,   char,   ScopedElement<char>>

   , Types<TSet<Text*>,  Text*,  ScopedElement<Text*>>
   , Types<TSet<int*>,   int*,   ScopedElement<int*>>
   , Types<TSet<Any*>,   Any*,   ScopedElement<Any*>>
   , Types<TSet<RT*>,    RT*,    ScopedElement<RT*>>
   , Types<TSet<char*>,  char*,  ScopedElement<char*>>

   , Types<TSet<Text**>, Text**, ScopedElement<Text**>>
   , Types<TSet<int**>,  int**,  ScopedElement<int**>>
   , Types<TSet<Any**>,  Any**,  ScopedElement<Any**>>
   , Types<TSet<RT**>,   RT**,   ScopedElement<RT**>>
   , Types<TSet<char**>, char**, ScopedElement<char**>>

   #if LANGULUS_FEATURE(MANAGED_MEMORY)
   // Elements are allocated by the memory manager                      
   , Types<Set, Text,   ScopedElement<Text, true>>
   , Types<Set, int,    ScopedElement<int, true>>
   , Types<Set, Any,    ScopedElement<Any, true>>
   , Types<Set, RT,     ScopedElement<RT, true>>
   , Types<Set, char,   ScopedElement<char, true>>

   , Types<Set, Text*,  ScopedElement<Text*, true>>
   , Types<Set, int*,   ScopedElement<int*, true>>
   , Types<Set, Any*,   ScopedElement<Any*, true>>
   , Types<Set, RT*,    ScopedElement<RT*, true>>
   , Types<Set, char*,  ScopedElement<char*, true>>

   , Types<Set, Text**, ScopedElement<Text**, true>>
   , Types<Set, int**,  ScopedElement<int**, true>>
   , Types<Set, Any**,  ScopedElement<Any**, true>>
   , Types<Set, RT**,   ScopedElement<RT**, true>>
   , Types<Set, char**, ScopedElement<char**, true>>

   , Types<TSet<Text>,   Text,   ScopedElement<Text, true>>
   , Types<TSet<int>,    int,    ScopedElement<int, true>>
   , Types<TSet<Any>,    Any,    ScopedElement<Any, true>>
   , Types<TSet<RT>,     RT,     ScopedElement<RT, true>>
   , Types<TSet<char>,   char,   ScopedElement<char, true>>

   , Types<TSet<Text*>,  Text*,  ScopedElement<Text*, true>>
   , Types<TSet<int*>,   int*,   ScopedElement<int*, true>>
   , Types<TSet<Any*>,   Any*,   ScopedElement<Any*, true>>
   , Types<TSet<RT*>,    RT*,    ScopedElement<RT*, true>>
   , Types<TSet<char*>,  char*,  ScopedElement<char*, true>>

   , Types<TSet<Text**>, Text**, ScopedElement<Text**, true>>
   , Types<TSet<int**>,  int**,  ScopedElement<int**, true>>
   , Types<TSet<Any**>,  Any**,  ScopedElement<Any**, true>>
   , Types<TSet<RT**>,   RT**,   ScopedElement<RT**, true>>
   , Types<TSet<char**>, char**, ScopedElement<char**, true>>

   // Packed pointers                                                   
   , Types<Set, pptr8,  ScopedElementPacked<pptr8>>
   , Types<Set, pptr16, ScopedElementPacked<pptr16>>
   , Types<Set, pptr32, ScopedElementPacked<pptr32>>

   , Types<TSet<pptr8>,  pptr8,  ScopedElementPacked<pptr8>>
   , Types<TSet<pptr16>, pptr16, ScopedElementPacked<pptr16>>
   , Types<TSet<pptr32>, pptr32, ScopedElementPacked<pptr32>>
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

      static_assert(    requires (T pack)         { pack.operator +   (pack); });
      static_assert(    requires (T pack, E item) { pack.operator +   (item); });
      static_assert(    requires (T pack)         { pack.operator +=  (pack); });
      static_assert(    requires (T pack, E item) { pack.operator +=  (item); });
      static_assert(    requires (T pack, E item) { pack.operator <<  (item); });
      static_assert(    requires (T pack, E item) { pack.operator >>  (item); });
      static_assert(    requires (T pack, E item) { pack.operator <<= (item); });
      static_assert(    requires (T pack, E item) { pack.operator >>= (item); });
      static_assert(not requires (T pack, E item) { pack.InsertAt(Index::Back, item); });
      static_assert(not requires (T pack, E item) { pack.EmplaceAt(Index::Back, item); });
      static_assert(    requires (T pack, E item) { pack.Remove(item); });
      static_assert(not requires (T pack, E item) { pack.RemoveAt(Index::Front); });
      static_assert(    requires (T pack, E item) { pack.Reserve(20); });
      static_assert(not requires (T pack, E item) { pack.EnableOr(); });
      static_assert(not requires (T pack, E item) { pack.IsOr(); });
      static_assert(    requires (T pack, E item) { pack.Find(item); });
      static_assert(    requires (T pack, E item) { pack.ForEach([](const int&) {}); });
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
      Logger::Info("Size of ", NameOf<::std::unordered_set<E>>(), " container is: ", sizeof(::std::unordered_set<E>), " bytes");
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
      static_assert(sizeof(T) <= sizeof(::std::unordered_set<E>));
   }

   GIVEN("Default-constructed container") {
      const ScopedE element {555};
      T pack;

      WHEN("Default-constructed") {
         Set_CheckState_Default<E>(pack);
      
         if constexpr (Ambiguous) {
            WHEN("Ambiguous assign value by referral") {
               REQUIRE_THROWS(pack = *element);
               REQUIRE_THROWS(pack = Refer(*element));
            }
         }

         BenchmarkSetStd("Empty/DefaultConstructor", 30, 40,
            T temp,                           new (&temp)     T{},
            ::std::unordered_set<E> temp_std, new (&temp_std) ::std::unordered_set<E>{}
         );
      }

      WHEN("Assigned value by referral") {
         pack.Assign(*element);

         Set_CheckState_OwnedFull<E>(pack);
         Set_CheckState_ContainsOne(pack, Refer(element));

         BenchmarkSetStd("Empty/Assign(Refer(" + NameOf<E>() + "))", 30, 100,
            T temp,                           temp.Assign(*element),
            ::std::unordered_set<E> temp_std, temp_std.emplace_back(*element)
         );
      }

      if constexpr (CT::Deep<E> and CT::Dense<E>) {
         WHEN("Assigned and absorbed by referral") {
            if (CT::Typed<T> and not pack.IsSame(element->GetType())) {
               const auto element_backup = *element;
               REQUIRE_THROWS(pack.AssignAbsorb(*element));
               Set_CheckState_Default<E>(pack);
               Set_Helper_TestSame(element_backup, *element);
               return;
            }

            pack.AssignAbsorb(*element);

            Set_Helper_TestSame(pack, *element);
            REQUIRE(pack.GetUses() == element->GetUses());
            REQUIRE(pack.GetUses() == 2);
            REQUIRE(pack.GetAllocation() == element->GetAllocation());

            [[maybe_unused]] ::std::unordered_set<E> src_std {1, *element};
            BenchmarkSetStd("Empty/AssignAbsorb(Refer(" + NameOf<E>() + "))", 30, 100,
               T temp,                           temp.AssignAbsorb(*element),
               ::std::unordered_set<E> temp_std, temp_std = src_std;
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
         
         if constexpr (CT::Deep<E> and CT::Dense<E>)
            Set_CheckState_Default<TypeOf<E>>(movable);

         Set_CheckState_OwnedFull<E>(pack);
         Set_CheckState_ContainsOne(pack, Refer(element));

         BenchmarkSetStd("Empty/Assign(Move(" + NameOf<E>() + "))", 30, 100,
            auto movable = *element;
            T temp,                             temp.Assign(::std::move(movable)),
            auto movable = *element;
            ::std::unordered_set<E> temp_std,   temp_std.emplace_back(::std::move(movable))
         );
      }

      if constexpr (CT::Deep<E> and CT::Dense<E>) {
         WHEN("Assigned and absorbed by move") {
            auto movable = *element;

            if (CT::Typed<T> and not pack.IsSame(element->GetType())) {
               REQUIRE_THROWS(pack.AssignAbsorb(::std::move(movable)));
               Set_CheckState_Default<E>(pack);
               Set_Helper_TestSame(movable, *element);
               return;
            }

            pack.AssignAbsorb(::std::move(movable));

            Set_CheckState_Default<TypeOf<E>>(movable);
            Set_Helper_TestSame(pack, *element);
            REQUIRE(pack.GetUses() == element->GetUses());
            REQUIRE(pack.GetUses() == 2);
            REQUIRE(pack.GetAllocation() == element->GetAllocation());

            BenchmarkSetStd("Empty/AssignAbsorb(Move(" + NameOf<E>() + "))", 30, 100,
               auto movable = *element;
               T temp,                                   temp.AssignAbsorb(::std::move(movable)),
               ::std::unordered_set<E> movable (1, 555);
               ::std::unordered_set<E> temp_std,         temp_std.emplace_back(::std::move(movable))
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

         Set_CheckState_OwnedFull<E>(pack);
         Set_CheckState_ContainsOne(pack, Copy(element));

         BenchmarkSetStd("Empty/Assign(Copy(" + NameOf<E>() + "))", 30, 100,
            T temp,                            temp.Assign(Copy(*element)),
            ::std::unordered_set<E> temp_std,  temp_std.emplace_back(*element)
         );
      }

      if constexpr (CT::Deep<E> and CT::Dense<E>) {
         WHEN("Assigned and absorbed copied value") {
            if (CT::Typed<T> and not pack.IsSame(element->GetType())) {
               const auto element_backup = *element;
               REQUIRE_THROWS(pack.AssignAbsorb(Copy(*element)));
               Set_CheckState_Default<E>(pack);
               Set_Helper_TestSame(element_backup, *element);
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

            [[maybe_unused]] ::std::vector<E> src_std (1, *element);
            BenchmarkSetStd("Empty/AssignAbsorb(Copy(" + NameOf<E>() + "))", 30, 100,
               T temp,                             temp.AssignAbsorb(Copy(*element)),
               ::std::unordered_set<E> temp_std,   temp_std = src_std
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

         Set_CheckState_OwnedFull<E>(pack);
         Set_CheckState_ContainsOne(pack, Clone(element));

         BenchmarkSetStd("Empty/Assign(Clone(" + NameOf<E>() + "))", 30, 100,
            T temp,                             temp.Assign(Clone(*element)),
            ::std::unordered_set<E> temp_std,   temp_std.emplace_back(*element)
         );
      }

      if constexpr (CT::Deep<E> and CT::Dense<E>) {
         WHEN("Assigned and absorbed cloned value") {
            if (CT::Typed<T> and not pack.IsSame(element->GetType())) {
               const auto element_backup = *element;
               REQUIRE_THROWS(pack.AssignAbsorb(Clone(*element)));
               Set_CheckState_Default<E>(pack);
               Set_Helper_TestSame(element_backup, *element);
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

            [[maybe_unused]] ::std::unordered_set<E> src_std (1, *element);
            BenchmarkSetStd("Empty/AssignAbsorb(Clone(" + NameOf<E>() + "))", 30, 100,
               T temp,                             temp.AssignAbsorb(Clone(*element)),
               ::std::unordered_set<E> temp_std,   temp_std = src_std
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

         Set_CheckState_OwnedFull<E>(pack);
         Set_CheckState_ContainsOne(pack, Disown(element));

         BenchmarkSetStd("Empty/Assign(Disown(" + NameOf<E>() + "))", 30, 100,
            T temp,                             temp.Assign(Disown(*element)),
            ::std::unordered_set<E> temp_std,   temp_std.emplace_back(*element)
         );
      }

      if constexpr (CT::Deep<E> and CT::Dense<E>) {
         WHEN("Assigned and absorbed disowned value") {
            if (CT::Typed<T> and not pack.IsSame(element->GetType())) {
               const auto element_backup = *element;
               REQUIRE_THROWS(pack.AssignAbsorb(Disown(*element)));
               Set_CheckState_Default<E>(pack);
               Set_Helper_TestSame(element_backup, *element);
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

            [[maybe_unused]] ::std::unordered_set<E> src_std (1, *element);
            BenchmarkSetStd("Empty/AssignAbsorb(Disown(" + NameOf<E>() + "))", 30, 100,
               T temp,                             temp.AssignAbsorb(Disown(*element)),
               ::std::unordered_set<E> temp_std,   temp_std = src_std
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

         if constexpr (CT::Deep<E> and CT::Dense<E>)
            Set_CheckState_Abandoned<E>(movable);
         Set_CheckState_OwnedFull<E>(pack);
         Set_CheckState_ContainsOne(pack, Refer(element));

         BenchmarkSetStd("Empty/Assign(Abandon(" + NameOf<E>() + "))", 30, 100,
            auto movable = *element;
            T temp,                             temp.Assign(Abandon(movable)),
            auto movable = *element;
            ::std::unordered_set<E> temp_std,   temp_std.emplace_back(::std::move(movable))
         );
      }

      if constexpr (CT::Deep<E> and CT::Dense<E>) {
         WHEN("Assigned and absorbed abandoned value") {
            auto movable = *element;

            if (CT::Typed<T> and not pack.IsSame(element->GetType())) {
               REQUIRE_THROWS(pack.AssignAbsorb(Abandon(movable)));
               Set_CheckState_Default<E>(pack);
               Set_Helper_TestSame(movable, *element);
               return;
            }

            pack.AssignAbsorb(Abandon(movable));

            Set_CheckState_Abandoned<E>(movable);
            Set_Helper_TestSame(pack, *element);
            REQUIRE(pack.GetUses() == 2);
            REQUIRE(pack.GetAllocation() == element->GetAllocation());

            BenchmarkSetStd("Empty/AssignAbsorb(Abandon(" + NameOf<E>() + "))", 30, 100,
               auto movable = *element;
               T temp,                                      temp.AssignAbsorb(Abandon(movable)),
               ::std::unordered_set<E> movable (1, 555);
               ::std::unordered_set<E> temp_std,            temp_std = ::std::move(movable)
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

         Set_CheckState_Default<E>(pack);
      }

      WHEN("Emplace (insert)") {
         ScopedE i666 {666};
         const auto i666backup = *i666;
         decltype(auto) instance = pack.Emplace(::std::move(*i666));
         Set_CheckState_OwnedFull<E>(pack);
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

         BenchmarkSet("Empty/Emplace(" + NameOf<E>() + ")", 30,
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
            Set_CheckState_OwnedFull<E>(pack);
            REQUIRE(instance.CompareOneEqual(i666backup));
            REQUIRE(pack.GetCount() == 1);
            REQUIRE(pack.GetReserved() >= 1);

            BenchmarkSet("Empty/Emplace(Describe(" + NameOf<E>() + "))", 30,
               T temp,
               temp.Emplace(Describe{descriptor})
            );
         }
         else if constexpr (CT::TypeErased<T>) {
            pack.template SetType<E>();
            REQUIRE_THROWS(pack.Emplace(Describe{descriptor}));
            Set_CheckState_Default<E>(pack, true);
         }
      }

      WHEN("Cleared") {
         pack.Clear();

         Set_CheckState_Default<E>(pack);

         BenchmarkSetStd("Empty/Clear(" + NameOf<E>() + ")", 30, 100,
            T temp,                             temp.Clear(),
            ::std::unordered_set<E> temp_std,   temp_std.clear()
         );
      }

      WHEN("Reset") {
         pack.Reset();

         Set_CheckState_Default<E>(pack);

         BenchmarkSetStd("Empty/Reset(" + NameOf<E>() + ")", 30, 100,
            T temp,                             temp.Reset(),
            ::std::unordered_set<E> temp_std,   temp_std.clear()
         );
      }

      WHEN("Referred empty") {
         T refer1 = pack;

         Set_Helper_TestSame(refer1, pack);
         Set_CheckState_Default<E>(refer1);
         Set_CheckState_Default<E>(pack);

         T refer2 = Refer(pack);

         Set_Helper_TestSame(refer2, pack);
         Set_CheckState_Default<E>(refer2);
         Set_CheckState_Default<E>(pack);
      }

      WHEN("Cloned empty") {
         T clone = Clone(pack);

         Set_Helper_TestSame(clone, pack);
         Set_CheckState_Default<E>(clone);
         Set_CheckState_Default<E>(pack);
      }

      WHEN("Disowned empty") {
         T disowned = Disown(pack);

         Set_Helper_TestSame(disowned, pack);
         Set_CheckState_Default<E>(disowned);
         Set_CheckState_Default<E>(pack);
      }

      WHEN("Copied empty") {
         T copy = Copy(pack);

         Set_Helper_TestSame(copy, pack);
         Set_CheckState_Default<E>(copy);
         Set_CheckState_Default<E>(pack);
      }

      WHEN("Moved empty") {
         T movable1 = pack;
         const T moved1 = ::std::move(movable1);

         Set_CheckState_Default<E>(movable1);
         Set_Helper_TestSame(moved1, pack);
         Set_CheckState_Default<E>(moved1);
         Set_CheckState_Default<E>(pack);

         T movable2 = pack;
         const T moved2 = Move(movable2);

         Set_CheckState_Default<E>(movable2);
         Set_Helper_TestSame(moved2, pack);
         Set_CheckState_Default<E>(moved2);
         Set_CheckState_Default<E>(pack);
      }

      WHEN("Abandoned empty") {
         T movable = pack;
         const T moved = Abandon(movable);

         Set_CheckState_Default<E>(movable);
         Set_Helper_TestSame(moved, pack);
         Set_CheckState_Default<E>(moved);
         Set_CheckState_Default<E>(pack);
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
         BenchmarkSet("Empty/operator==(" + NameOf<E>() + ")", 30,
            (void) 0, dont_optimize |= (another_pack1 == another_pack2)
         );
         BenchmarkSet("Empty/operator!=(" + NameOf<E>() + ")", 30,
            (void) 0, dont_optimize |= (another_pack1 != another_pack2)
         );
      }

      WHEN("Contains when empty") {
         REQUIRE_FALSE(pack.Contains(*element));

         [[maybe_unused]] volatile bool dont_optimize = false;
         BenchmarkSet("Empty/Contains(" + NameOf<E>() + ")", 30,
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