///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "Main.hpp"
#include <Langulus/Retype.hpp>

using namespace Langulus;


namespace
{
   struct TypedNotRetypable {
      using CTTI_Typed = int;
   };
   
   struct TypedRetypable {
      using CTTI_Typed = int;
      template<class T>
      using Retype = T;
   };

   template<class T>
   struct Retypable {
      T data;
   };
   
   template<class T>
   struct Retypable2 {
      T data;
   };
   
   template<class T>
   struct RetypableCustom {
      T data;
      template<class T2>
      using Retype = Retypable2<T2>;
   };
}

SCENARIO("Retype") {
   static_assert(::std::same_as<Retype<void, float>, void>);
   static_assert(::std::same_as<Retype<TypedNotRetypable, float>, TypedNotRetypable>);
   static_assert(::std::same_as<Retype<TypedRetypable, float>, float>);
   static_assert(::std::same_as<Retype<Retypable<int>, float>, Retypable<float>>);
   static_assert(::std::same_as<Retype<RetypableCustom<int>, float>, Retypable2<float>>);
}
