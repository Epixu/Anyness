///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "Main.hpp"
#include <Langulus/Lambda.hpp>
#include <concepts>

using namespace Langulus;

namespace
{
   auto testLambdaNoArgs = []()     -> double { return 5; };
   auto testLambdaOne    = [](int)  -> double { return 5; };
   auto testLambdaOneRef = [](int&) -> double { return 5; };
   auto testLambdaTwo    = [](int,  float)  -> double { return 5; };
   auto testLambdaTwoRef = [](int&, float&) noexcept -> double { return 5; };
}


///                                                                           
/// ArgumentOf                                                                
///                                                                           
SCENARIO("Testing ArgumentOf", "[ct]") {
   int suppress_warnings1 = 666;
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
}


///                                                                           
/// ArgumentsOf                                                               
///                                                                           
SCENARIO("Testing ArgumentsOf", "[ct]") {
   static_assert(::std::same_as<ArgumentsOf<decltype(testLambdaNoArgs)>, Types<void>>);
   static_assert(::std::same_as<ArgumentsOf<decltype(testLambdaOne)>,    Types<int>>);
   static_assert(::std::same_as<ArgumentsOf<decltype(testLambdaOneRef)>, Types<int&>>);
   static_assert(::std::same_as<ArgumentsOf<decltype(testLambdaTwo)>,    Types<int, float>>);
   static_assert(::std::same_as<ArgumentsOf<decltype(testLambdaTwoRef)>, Types<int&, float&>>);
}


///                                                                           
/// ReturnOf                                                                  
///                                                                           
SCENARIO("Testing ReturnOf", "[ct]") {
   static_assert(::std::same_as<ReturnOf<decltype(testLambdaNoArgs)>, double>);
   static_assert(::std::same_as<ReturnOf<decltype(testLambdaOne)>,    double>);
   static_assert(::std::same_as<ReturnOf<decltype(testLambdaOneRef)>, double>);
   static_assert(::std::same_as<ReturnOf<decltype(testLambdaTwo)>,    double>);
   static_assert(::std::same_as<ReturnOf<decltype(testLambdaTwoRef)>, double>);
}


///                                                                           
/// IsNoexcept                                                                
///                                                                           
SCENARIO("Testing IsNoexcept", "[ct]") {
   static_assert(not IsNoexcept<decltype(testLambdaNoArgs)>);
   static_assert(not IsNoexcept<decltype(testLambdaOne)>);
   static_assert(not IsNoexcept<decltype(testLambdaOneRef)>);
   static_assert(not IsNoexcept<decltype(testLambdaTwo)>);
   static_assert(    IsNoexcept<decltype(testLambdaTwoRef)>);
}