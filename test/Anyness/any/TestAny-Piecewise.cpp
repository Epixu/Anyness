///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#include "TestAnyCommon.hpp"
#include <Langulus/Anyness/Many.hpp>


TEST_CASE_TEMPLATE("Test piecewise-constructed Any/TAny", TestType
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
   , Types<Any, RT**,   ScopedElement<RT**>>
   , Types<Any, char**, ScopedElement<char**>>

   , Types<TAny<Text>,   Text,   ScopedElement<Text>>
   , Types<TAny<int>,    int,    ScopedElement<int>>
   , Types<TAny<Any>,    Any,    ScopedElement<Any>>
   , Types<TAny<RT>,     RT,     ScopedElement<RT>>
   , Types<TAny<char>,   char,   ScopedElement<char>>
                                 
   , Types<TAny<Text*>,  Text*,  ScopedElement<Text*>>
   , Types<TAny<int*>,   int*,   ScopedElement<int*>>
   , Types<TAny<Any*>,   Any*,   ScopedElement<Any*>>
   , Types<TAny<RT*>,    RT*,    ScopedElement<RT*>>
   , Types<TAny<char*>,  char*,  ScopedElement<char*>>

   , Types<TAny<Text**>, Text**, ScopedElement<Text**>>
   , Types<TAny<int**>,  int**,  ScopedElement<int**>>
   , Types<TAny<Any**>,  Any**,  ScopedElement<Any**>>
   , Types<TAny<RT**>,   RT**,   ScopedElement<RT**>>
   , Types<TAny<char**>, char**, ScopedElement<char**>>

   #if LANGULUS_FEATURE(MANAGED_MEMORY)
   // Elements are allocated by the memory manager                      
   , Types<Any, Text,   ScopedElement<Text, true>>
   , Types<Any, int,    ScopedElement<int, true>>
   , Types<Any, Any,    ScopedElement<Any, true>>
   , Types<Any, RT,     ScopedElement<RT, true>>
   , Types<Any, char,   ScopedElement<char, true>>
                        
   , Types<Any, Text*,  ScopedElement<Text*, true>>
   , Types<Any, int*,   ScopedElement<int*, true>>
   , Types<Any, Any*,   ScopedElement<Any*, true>>
   , Types<Any, RT*,    ScopedElement<RT*, true>>
   , Types<Any, char*,  ScopedElement<char*, true>>

   , Types<Any, Text**, ScopedElement<Text**, true>>
   , Types<Any, int**,  ScopedElement<int**, true>>
   , Types<Any, Any**,  ScopedElement<Any**, true>>
   , Types<Any, RT**,   ScopedElement<RT**, true>>
   , Types<Any, char**, ScopedElement<char**, true>>

   , Types<TAny<Text>,   Text,   ScopedElement<Text, true>>
   , Types<TAny<int>,    int,    ScopedElement<int, true>>
   , Types<TAny<Any>,    Any,    ScopedElement<Any, true>>
   , Types<TAny<RT>,     RT,     ScopedElement<RT, true>>
   , Types<TAny<char>,   char,   ScopedElement<char, true>>
                                 
   , Types<TAny<Text*>,  Text*,  ScopedElement<Text*, true>>
   , Types<TAny<int*>,   int*,   ScopedElement<int*, true>>
   , Types<TAny<Any*>,   Any*,   ScopedElement<Any*, true>>
   , Types<TAny<RT*>,    RT*,    ScopedElement<RT*, true>>
   , Types<TAny<char*>,  char*,  ScopedElement<char*, true>>

   , Types<TAny<Text**>, Text**, ScopedElement<Text**, true>>
   , Types<TAny<int**>,  int**,  ScopedElement<int**, true>>
   , Types<TAny<Any**>,  Any**,  ScopedElement<Any**, true>>
   , Types<TAny<RT**>,   RT**,   ScopedElement<RT**, true>>
   , Types<TAny<char**>, char**, ScopedElement<char**, true>>

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
   constexpr bool Ambiguous = not Same<T, E> and CT::Deep<E> and CT::Dense<E> and LANGULUS(SAFE);

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

         BenchmarkAnyStd("Empty/PiecewiseConstructor(" + NameOf<E>() + ")", 30, 400,
            T temp,              (new (&temp)     T{Piecewise, *originalElement}),
            ::std::any temp_std, new (&temp_std) ::std::any{*originalElement}
         );
      }

      WHEN("Assigned compatible referred value") {
         auto assign_refer = [&](auto& a, [[maybe_unused]] const char* intent) {
            a.Assign(*element);

            if constexpr (CT::Container<E>)
               Any_CheckState_OwnedFull<TypeOf<E>>(*element);
            Any_CheckState_OwnedFull<E>(a);
            Any_CheckState_ContainsOne(a, Refer(element));

            BenchmarkAnyStd(
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

            auto absorb_refer = [&](auto& a, [[maybe_unused]] const char* intent) {
               a.AssignAbsorb(*element);

               Any_Helper_TestSame(a, *element);
               REQUIRE(a.GetUses() == element->GetUses());
               REQUIRE(a.GetUses() == 2);
               REQUIRE(a.GetAllocation() == element->GetAllocation());

               BenchmarkAnyStd(
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
         auto assign_clone = [&](auto& a, [[maybe_unused]] const char* intent) {
            a.Assign(Clone(*element));

            if constexpr (CT::Container<E>)
               Any_CheckState_OwnedFull<TypeOf<E>>(*element);
            Any_CheckState_OwnedFull<E>(a);
            Any_CheckState_ContainsOne(a, Clone(element));

            BenchmarkAnyStd(
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

            auto absorb_clone = [&](auto& a, [[maybe_unused]] const char* intent) {
               a.AssignAbsorb(Clone(*element));

               if constexpr (CT::Container<E>)
                  Any_CheckState_OwnedFull<TypeOf<E>>(*element);
               Any_Helper_TestSame(a, *element);
               REQUIRE(a.GetUses() == 2);
               REQUIRE(a.GetAllocation() == element->GetAllocation());

               BenchmarkAnyStd(
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
         auto assign_copy = [&](auto& a, [[maybe_unused]] const char* intent) {
            a.Assign(Copy(*element));

            if constexpr (CT::Container<E>)
               Any_CheckState_OwnedFull<TypeOf<E>>(*element);
            Any_CheckState_OwnedFull<E>(a);
            Any_CheckState_ContainsOne(a, Refer(element));

            BenchmarkAnyStd(
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

            auto absorb_copy = [&](auto& a, [[maybe_unused]] const char* intent) {
               a.AssignAbsorb(Copy(*element));

               if constexpr (CT::Container<E>)
                  Any_CheckState_OwnedFull<TypeOf<E>>(*element);
               Any_Helper_TestSame(a, *element);
               REQUIRE(a.GetUses() == 2);
               REQUIRE(a.GetAllocation() == element->GetAllocation());

               BenchmarkAnyStd(
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
         auto assign_move = [&](auto& a, [[maybe_unused]] const char* intent) {
            auto movable = *element;
            a.Assign(::std::move(movable));

            if constexpr (CT::Container<E>)
               Any_CheckState_Default<TypeOf<E>>(movable);
            Any_CheckState_OwnedFull<E>(a);
            Any_CheckState_ContainsOne(a, Refer(element));

            BenchmarkAnyStd(
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

            auto absorb_move = [&](auto& a, [[maybe_unused]] const char* intent) {
               auto movable = *element;
               a.AssignAbsorb(::std::move(movable));

               if constexpr (CT::Container<E>)
                  Any_CheckState_Default<TypeOf<E>>(movable);
               Any_Helper_TestSame(a, *element);
               REQUIRE(a.GetUses() == 2);
               REQUIRE(a.GetAllocation() == element->GetAllocation());

               BenchmarkAnyStd(
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
         auto assign_disown = [&](auto& a, [[maybe_unused]] const char* intent) {
            a.Assign(Disown(*element));

            Any_CheckState_OwnedFull<E>(a);
            Any_CheckState_ContainsOne(a, Disown(element));

            BenchmarkAnyStd(
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

            auto absorb_disown = [&](auto& a, [[maybe_unused]] const char* intent) {
               a.AssignAbsorb(Disown(*element));

               REQUIRE(a.GetRaw() == element->GetRaw());
               REQUIRE(a.IsExact(element->GetType()));
               REQUIRE(a == *element);
               REQUIRE(a.IsDeep() == element->IsDeep());
               REQUIRE(a.IsConstant() != element->IsConstant());
               REQUIRE(a.GetUnconstrainedState() == element->GetUnconstrainedState());
               REQUIRE(a.GetUses() == 0);
               REQUIRE_FALSE(a.GetAllocation());

               BenchmarkAnyStd(
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
         auto assign_abandon = [&](auto& a, [[maybe_unused]] const char* intent) {
            auto movable = *element;
            a.Assign(Abandon(movable));

            if constexpr (CT::Container<E>)
               Any_CheckState_Abandoned<TypeOf<E>>(movable);
            Any_CheckState_OwnedFull<E>(a);
            Any_CheckState_ContainsOne(a, Refer(element));

            BenchmarkAnyStd(
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

            auto absorb_abandon = [&](auto& a, [[maybe_unused]] const char* intent) {
               auto movable = *element;
               a.AssignAbsorb(Abandon(movable));

               if constexpr (CT::Container<E>)
                  Any_CheckState_Abandoned<TypeOf<E>>(movable);
               Any_Helper_TestSame(a, *element);
               REQUIRE(a.GetUses() == 2);
               REQUIRE(a.GetAllocation() == element->GetAllocation());

               BenchmarkAnyStd(
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
         auto assign_empty_self = [&](auto& a) {
            a = T{};
            Any_CheckState_Default<E>(a);
         };

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
         auto assign_full_self = [&](auto& a) {
            auto backup = a;
            const auto uses_before = a.GetUses();
            LglsDisableWarningPush
            LglsDisableWarning_SelfAssign
               a = a;
            LglsDisableWarningPop
            Any_Helper_TestSame(a, backup);
            REQUIRE(a.GetUses() == uses_before);
         };

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
         auto absorb_construct_refer = [&](auto& a) {
            T absorbed1 {a};
            T absorbed2{Refer {a}};

            Any_Helper_TestSame(absorbed1, a);
            Any_Helper_TestSame(absorbed2, a);
            REQUIRE(absorbed1.GetUses() == 3);
            REQUIRE(absorbed2.GetUses() == 3);
         };

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
         auto absorb_construct_move1 = [&](auto& a) {
            T backup = a;
            T absorbed {::std::move(a)};

            Any_CheckState_Default<E>(a);
            Any_CheckState_OwnedFull<E>(absorbed);
            Any_Helper_TestSame(absorbed, backup);
            REQUIRE(absorbed.GetUses() == 2);
         };

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
         auto absorb_construct_move2 = [&](auto& a) {
            T backup = a;
            T absorbed {Move(a)};

            Any_CheckState_Default<E>(a);
            Any_CheckState_OwnedFull<E>(absorbed);
            Any_Helper_TestSame(absorbed, backup);
            REQUIRE(absorbed.GetUses() == 2);
         };

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
         auto absorb_construct_abandon = [&](auto& a) {
            T backup = a;
            T absorbed {Abandon {a}};

            Any_CheckState_Abandoned<E>(a);
            Any_CheckState_OwnedFull<E>(absorbed);
            Any_Helper_TestSame(absorbed, backup);
            REQUIRE(absorbed.GetUses() == 2);
         };

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
         auto absorb_construct_disown = [&](auto& a) {
            T absorbed {Disown {a}};

            Any_CheckState_OwnedFull<E>(a);
            Any_CheckState_DisownedFull<E>(absorbed);
            REQUIRE(absorbed.GetRaw() == a.GetRaw());
            REQUIRE(absorbed.IsExact(a.GetType()));
            REQUIRE(absorbed == a);
            REQUIRE(absorbed.IsDeep() == a.IsDeep());
            REQUIRE(absorbed.IsConstant() != a.IsConstant());
            REQUIRE(absorbed.GetUnconstrainedState() == a.GetUnconstrainedState());
            REQUIRE(absorbed.GetUses() == 0);
         };

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
         const bool managed_sparse = CT::Sparse<E> and Managed;
         auto absorb_construct_copy = [&](auto& a, int entry_refs) {
            T absorbed {Copy {a}};

            Any_CheckState_OwnedFull<E>(a);
            Any_CheckState_OwnedFull<E>(absorbed);
            REQUIRE(absorbed == a);
            REQUIRE(absorbed.GetRaw() != a.GetRaw());
            REQUIRE(absorbed.template As<E>() == a.template As<E>());
            if constexpr (CT::Sparse<E>) {
               auto entry = *absorbed.GetEntries();
               if ((entry_refs) == 0)
                  REQUIRE(entry == nullptr);
               if (entry) //{
                  REQUIRE(entry->GetUses() == (entry_refs));
                  /*if constexpr (CT::Referenced<Decay<E>>) {
                     auto e = absorbed.template As<E>();
                     REQUIRE(DenseCast(e).GetReferences() == (entry_refs + 1));
                  }
               }
               else {*/
                  if constexpr (CT::Referenced<Decay<E>>) {
                     auto e = absorbed.template As<E>();
                     REQUIRE(DenseCast(e).GetReferences() == 9);
                  }
               //}
            }
            REQUIRE(absorbed.GetUses() == 1);
            REQUIRE(a.GetUses() == 1);
         };

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
         auto absorb_construct_clone = [&](auto& a) {
            T absorbed {Clone {a}};

            Any_CheckState_OwnedFull<E>(a);
            Any_CheckState_OwnedFull<E>(absorbed);
            REQUIRE((absorbed == a) == CT::Dense<E>);
            REQUIRE(absorbed.GetUses() == 1);
         };

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
         auto emplace_overwrite = [&](auto& a, [[maybe_unused]] const char* intent) {
            ScopedE i666{666};
            const auto i666backup = *i666;
            decltype(auto) instance = a.Emplace(::std::move(*i666));

            Any_CheckState_OwnedFull<E>(a);
            if constexpr (CT::Handle<decltype(instance)>)
               REQUIRE(instance.CompareOneEqual(i666backup));
            else
               REQUIRE(instance == i666backup);
            REQUIRE(a.GetCount() == 1);
            REQUIRE(a.GetReserved() >= 1);
            if constexpr (CT::Typed<T>) {
               REQUIRE(*a == i666backup);
               if constexpr (CT::Handle<decltype(instance)>)
                  REQUIRE(&*a == &*instance);
               else
                  REQUIRE(&*a == &instance);
            }

            BenchmarkAny(
               std::string("Piecewise/") + intent + "/Emplace(" + static_cast<std::string>(NameOf<E>()) + ")", 30,
               auto movable1 = *element;
               auto movable2 = *originalElement;
               a.Emplace(::std::move(movable1)),      a.Emplace(::std::move(movable2))
            );

            if constexpr (not Managed) {
               // On unmanaged tests i666 will be destroyed at the end of this scope,
               // and the container will be left with a dangling pointer.
               // Make sure this isn't happening. When inserting raw unmanaged pointers, 
               // safety is solely in the hands of the user.
               a.Reset();
            }
         };

         emplace_overwrite(pack_referred1, "Refer");
         emplace_overwrite(pack_copied,    "Copy");
         emplace_overwrite(pack_cloned,    "Clone");
         emplace_overwrite(pack_moved1,    "Move");
         emplace_overwrite(pack_abandoned, "Abandon");
         emplace_overwrite(pack_disowned,  "Disown");
      }

      WHEN("Emplace (overwrite, describe)") {
         auto emplace_overwrite_describe = [&](auto& a, [[maybe_unused]] const char* intent) {
            ScopedE i666{666};
            const auto i666backup = *i666;
            Many descriptor {Piecewise, ::std::move(*i666)};

            if constexpr (CT::DescribeConstructible<E> and not CT::Container<T>) {
               decltype(auto) instance = a.Emplace(Describe{descriptor});

               Any_CheckState_OwnedFull<E>(a);
               REQUIRE(instance.CompareOneEqual(i666backup));
               REQUIRE(a.GetCount() == 1);
               REQUIRE(a.GetReserved() >= 1);

               BenchmarkAny(
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
         auto clear_full = [&](auto& a, [[maybe_unused]] const char* intent) {
            BenchmarkAnyStd(
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
         auto reset_full = [&](auto& a, [[maybe_unused]] const char* intent) {
            BenchmarkAnyStd(
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
            auto reset_and_reallocate = [&](auto& a) {
               const auto memory = a.GetRaw();
               a.Reset();
               a = *element;
               REQUIRE(a.GetRaw() == memory);
            };

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

         auto compared_full = [&](auto& a, [[maybe_unused]] const char* intent) {
            T same_pack {a};

            REQUIRE      (a != another_pack1);
            REQUIRE_FALSE(a == another_pack1);
            REQUIRE      (a != defaulted_pack);
            REQUIRE_FALSE(a == defaulted_pack);
            REQUIRE      (a == same_pack);
            REQUIRE_FALSE(a != same_pack);

            [[maybe_unused]] volatile bool dont_optimize = false;
            BenchmarkAnyStd(
               std::string("Piecewise/") + intent + "/operator==(" + static_cast<std::string>(NameOf<E>()) + ")", 30, 100,
               (void) 0,                                    dont_optimize |= (a == same_pack),
               const ::std::any a_std = *element;
               const ::std::any another_pack1_std = *e1,    dont_optimize |= (std::any_cast<E const&>(a_std) == std::any_cast<E const&>(another_pack1_std))
            );
            BenchmarkAnyStd(
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
         
         auto contains_full = [&](auto& a) {
            REQUIRE      (a.Contains(*originalElement));
            REQUIRE_FALSE(a.Contains(*e1));
         };

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
         BenchmarkAny("Piecewise/Contains(" + NameOf<E>() + ")", 30,
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

         static_assert(not static_cast<bool>(T{}));

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
