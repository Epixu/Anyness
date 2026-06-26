///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "Main.hpp"
#include <Langulus/Lambda.hpp>
#include <Langulus/CT/Noexcept.hpp>
#include <concepts>

using namespace Langulus;

namespace
{
   auto testLambdaNoArgs = []()     -> double { return 5; };
   auto testLambdaOne    = [](int)  -> double { return 5; };
   auto testLambdaOneRef = [](int&) -> double { return 5; };
   auto testLambdaTwo    = [](int,  float) -> double { return 5; };
   auto testLambdaTwoRef = [](int&, float&) noexcept -> double { return 5; };

   struct TestingMethods {
      double NoArgs() { return 5; }
      double OneArg(int) { return 5; }
      double OneArgRef(int&) { return 5; }
      double TwoArgs(int, float) { return 5; }
      double TwoArgsRef(int&, float&) noexcept { return 5; }

      double ConstNoArgs() const { return 5; }
      double ConstOneArg(int) const { return 5; }
      double ConstOneArgRef(int&) const { return 5; }
      double ConstTwoArgs(int, float) const { return 5; }
      double ConstTwoArgsRef(int&, float&) const noexcept { return 5; }
   };

   [[maybe_unused]] int testNoexceptTrue() noexcept { return 1; }
   [[maybe_unused]] int testNoexceptFalse() { return 1; }
   [[maybe_unused]] int testNoexceptMaybe1() noexcept_if(testNoexceptTrue)  { return 1; }
   [[maybe_unused]] int testNoexceptMaybe2() noexcept_if(testNoexceptFalse) { return 1; }
   [[maybe_unused]] int testNoexceptMaybe3() noexcept_if(testLambdaTwo)     { return 1; }
   [[maybe_unused]] int testNoexceptMaybe4() noexcept_if(testLambdaTwoRef)  { return 1; }
}


///                                                                           
/// MARK: ArgumentOf                                                          
///                                                                           
SCENARIO("Testing ArgumentOf") {
   int   suppress_warnings1 = 666;
   float suppress_warnings2 = 666.0f;
   (void) testLambdaNoArgs();
   (void) testLambdaOne(1);
   (void) testLambdaOneRef(suppress_warnings1);
   (void) testLambdaTwo(1, 1.0f);
   (void) testLambdaTwoRef(suppress_warnings1, suppress_warnings2);

   static_assert(::std::same_as<ArgumentOf<decltype(testLambdaNoArgs)>, void>);
   static_assert(::std::same_as<ArgumentOf<decltype(testLambdaOne)>,    int>);
   static_assert(::std::same_as<ArgumentOf<decltype(testLambdaOneRef)>, int&>);
   static_assert(::std::same_as<ArgumentOf<decltype(testLambdaTwo)>,    int>);
   static_assert(::std::same_as<ArgumentOf<decltype(testLambdaTwoRef)>, int&>);

   static_assert(::std::same_as<ArgumentOf<decltype(&TestingMethods::NoArgs)>,     void>);
   static_assert(::std::same_as<ArgumentOf<decltype(&TestingMethods::OneArg)>,     int>);
   static_assert(::std::same_as<ArgumentOf<decltype(&TestingMethods::OneArgRef)>,  int&>);
   static_assert(::std::same_as<ArgumentOf<decltype(&TestingMethods::TwoArgs)>,    int>);
   static_assert(::std::same_as<ArgumentOf<decltype(&TestingMethods::TwoArgsRef)>, int&>);

   static_assert(::std::same_as<ArgumentOf<decltype(&TestingMethods::ConstNoArgs)>,     void>);
   static_assert(::std::same_as<ArgumentOf<decltype(&TestingMethods::ConstOneArg)>,     int>);
   static_assert(::std::same_as<ArgumentOf<decltype(&TestingMethods::ConstOneArgRef)>,  int&>);
   static_assert(::std::same_as<ArgumentOf<decltype(&TestingMethods::ConstTwoArgs)>,    int>);
   static_assert(::std::same_as<ArgumentOf<decltype(&TestingMethods::ConstTwoArgsRef)>, int&>);
}


///                                                                           
/// MARK: ArgumentsOf                                                         
///                                                                           
SCENARIO("Testing ArgumentsOf") {
   static_assert(::std::same_as<ArgumentsOf<decltype(testLambdaNoArgs)>, NoTypes>);
   static_assert(::std::same_as<ArgumentsOf<decltype(testLambdaOne)>,    Types<int>>);
   static_assert(::std::same_as<ArgumentsOf<decltype(testLambdaOneRef)>, Types<int&>>);
   static_assert(::std::same_as<ArgumentsOf<decltype(testLambdaTwo)>,    Types<int, float>>);
   static_assert(::std::same_as<ArgumentsOf<decltype(testLambdaTwoRef)>, Types<int&, float&>>);

   static_assert(::std::same_as<ArgumentsOf<decltype(&TestingMethods::NoArgs)>,     NoTypes>);
   static_assert(::std::same_as<ArgumentsOf<decltype(&TestingMethods::OneArg)>,     Types<int>>);
   static_assert(::std::same_as<ArgumentsOf<decltype(&TestingMethods::OneArgRef)>,  Types<int&>>);
   static_assert(::std::same_as<ArgumentsOf<decltype(&TestingMethods::TwoArgs)>,    Types<int, float>>);
   static_assert(::std::same_as<ArgumentsOf<decltype(&TestingMethods::TwoArgsRef)>, Types<int&, float&>>);

   static_assert(::std::same_as<ArgumentsOf<decltype(&TestingMethods::ConstNoArgs)>,     NoTypes>);
   static_assert(::std::same_as<ArgumentsOf<decltype(&TestingMethods::ConstOneArg)>,     Types<int>>);
   static_assert(::std::same_as<ArgumentsOf<decltype(&TestingMethods::ConstOneArgRef)>,  Types<int&>>);
   static_assert(::std::same_as<ArgumentsOf<decltype(&TestingMethods::ConstTwoArgs)>,    Types<int, float>>);
   static_assert(::std::same_as<ArgumentsOf<decltype(&TestingMethods::ConstTwoArgsRef)>, Types<int&, float&>>);
}


///                                                                           
/// MARK: ReturnOf                                                            
///                                                                           
SCENARIO("Testing ReturnOf") {
   static_assert(::std::same_as<ReturnOf<decltype(testLambdaNoArgs)>, double>);
   static_assert(::std::same_as<ReturnOf<decltype(testLambdaOne)>,    double>);
   static_assert(::std::same_as<ReturnOf<decltype(testLambdaOneRef)>, double>);
   static_assert(::std::same_as<ReturnOf<decltype(testLambdaTwo)>,    double>);
   static_assert(::std::same_as<ReturnOf<decltype(testLambdaTwoRef)>, double>);

   static_assert(::std::same_as<ReturnOf<decltype(&TestingMethods::NoArgs)>,     double>);
   static_assert(::std::same_as<ReturnOf<decltype(&TestingMethods::OneArg)>,     double>);
   static_assert(::std::same_as<ReturnOf<decltype(&TestingMethods::OneArgRef)>,  double>);
   static_assert(::std::same_as<ReturnOf<decltype(&TestingMethods::TwoArgs)>,    double>);
   static_assert(::std::same_as<ReturnOf<decltype(&TestingMethods::TwoArgsRef)>, double>);

   static_assert(::std::same_as<ReturnOf<decltype(&TestingMethods::ConstNoArgs)>,     double>);
   static_assert(::std::same_as<ReturnOf<decltype(&TestingMethods::ConstOneArg)>,     double>);
   static_assert(::std::same_as<ReturnOf<decltype(&TestingMethods::ConstOneArgRef)>,  double>);
   static_assert(::std::same_as<ReturnOf<decltype(&TestingMethods::ConstTwoArgs)>,    double>);
   static_assert(::std::same_as<ReturnOf<decltype(&TestingMethods::ConstTwoArgsRef)>, double>);
}


///                                                                           
/// MARK: IsNoexcept                                                          
///                                                                           
SCENARIO("Testing IsNoexcept") {
   static_assert(not IsNoexcept<decltype(testLambdaNoArgs)>);
   static_assert(not IsNoexcept<decltype(testLambdaOne)>);
   static_assert(not IsNoexcept<decltype(testLambdaOneRef)>);
   static_assert(not IsNoexcept<decltype(testLambdaTwo)>);
   static_assert(    IsNoexcept<decltype(testLambdaTwoRef)>);

   static_assert(not IsNoexcept<decltype(&TestingMethods::NoArgs)>);
   static_assert(not IsNoexcept<decltype(&TestingMethods::OneArg)>);
   static_assert(not IsNoexcept<decltype(&TestingMethods::OneArgRef)>);
   static_assert(not IsNoexcept<decltype(&TestingMethods::TwoArgs)>);
   static_assert(    IsNoexcept<decltype(&TestingMethods::TwoArgsRef)>);

   static_assert(not IsNoexcept<decltype(&TestingMethods::ConstNoArgs)>);
   static_assert(not IsNoexcept<decltype(&TestingMethods::ConstOneArg)>);
   static_assert(not IsNoexcept<decltype(&TestingMethods::ConstOneArgRef)>);
   static_assert(not IsNoexcept<decltype(&TestingMethods::ConstTwoArgs)>);
   static_assert(    IsNoexcept<decltype(&TestingMethods::ConstTwoArgsRef)>);

   static_assert(    IsNoexcept<decltype(testNoexceptMaybe1)>);
   static_assert(not IsNoexcept<decltype(testNoexceptMaybe2)>);
   static_assert(not IsNoexcept<decltype(testNoexceptMaybe3)>);
   static_assert(    IsNoexcept<decltype(testNoexceptMaybe4)>);
   static_assert(not IsNoexcept<decltype(testLambdaTwo)>);
   static_assert(    IsNoexcept<decltype(testLambdaTwoRef)>);
}
