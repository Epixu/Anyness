///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#include "TestAnyCommon.hpp"
#include <Langulus/Anyness/SerializeText.hpp>


SCENARIO("Converting Any/TAny") {
   static MemoryState memoryState;

   GIVEN("A container with meta data") {
      Any         pack1 {MetaDataOf<double>()};
      TAny<DMeta> pack2 {MetaDataOf<double>()};

      WHEN("Converted to texts using a templated destination") {
         TAny<Text> converted1, converted2;
         pack1.ConvertTo(converted1);
         pack2.ConvertTo(converted2);

         REQUIRE(converted1 == converted2);
         REQUIRE(converted1.GetCount() == 1);
         REQUIRE(*converted1 == "Double");
      }
   }

   REQUIRE(memoryState.Assert());
   REQUIRE_FALSE(Allocator::CollectGarbage());
}
