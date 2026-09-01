fetch_external_module(
    doctest
    GIT_REPOSITORY  https://github.com/doctest/doctest.git
    GIT_TAG         tags/v2.5.3
    GIT_SHALLOW     TRUE
)

add_langulus_app(LangulusPercist
   SOURCES	   Precist.cpp
   LIBRARIES   LangulusAnyness
)

add_langulus_test(LangulusAnynessTest-Common
   SOURCES	   Main.cpp 
               TestAssume.cpp
               TestHashOf.cpp
               TestIntentOf.cpp
               TestLambda.cpp
               TestLiteral.cpp
               TestLogger.cpp
               TestMetaOf.cpp
               TestNameOf.cpp
               TestRetype.cpp
               TestSequence.cpp
               TestTag.cpp
               TestTypenav.cpp
               TestTypeOf.cpp
               TestTypes.cpp
               
               CT/TestAbstract.cpp
               CT/TestAkin.cpp
               CT/TestAllocatable.cpp
               CT/TestBool.cpp
               CT/TestInteger.cpp
               CT/TestReal.cpp
               CT/TestNumber.cpp
               CT/TestCharacter.cpp
               CT/TestContainer.cpp
               CT/TestConvertible.cpp
               CT/TestDeep.cpp
               CT/TestReflectAs.cpp
               CT/TestSuffix.cpp
               CT/TestUnfold.cpp
               
               Utils/TestRoof.cpp

   LIBRARIES   doctest
               LangulusAnyness
)

add_langulus_test(LangulusAnynessTest-Memory
   SOURCES	   Main.cpp
               $<IF:$<BOOL:${LANGULUS_OPTION_MANAGED_MEMORY}>,Allocator/TestManaged.cpp,Allocator/TestUnmanaged.cpp>
               
   LIBRARIES   doctest
               LangulusAnyness
)

add_langulus_test(LangulusAnynessTest-Text
   SOURCES	   Main.cpp
               Anyness/text/TestText-Empty.cpp
               Anyness/text/TestText-Absorb.cpp
               Anyness/text/TestText-Piecewise.cpp

   LIBRARIES   doctest
               LangulusAnyness
)

add_langulus_test(LangulusAnynessTest-Bytes
   SOURCES	   Main.cpp
               Anyness/bytes/TestBytes-Empty.cpp
               #Anyness/TestBytes.cpp

   LIBRARIES   doctest
               LangulusAnyness
)

add_langulus_test(LangulusAnynessTest-TRef-TOwn
   SOURCES	   Main.cpp
               Anyness/TestTRef.cpp
               Anyness/TestTOwn.cpp

   LIBRARIES   doctest
               LangulusAnyness
)

add_langulus_test(LangulusAnynessTest-Any
   SOURCES	   Main.cpp
               Anyness/any/TestAny-Empty.cpp
               Anyness/any/TestAny-Absorb.cpp
               Anyness/any/TestAny-Piecewise.cpp
               Anyness/any/TestAny-Convert.cpp

   LIBRARIES   doctest
               LangulusAnyness
)

add_langulus_test(LangulusAnynessTest-Many
   SOURCES	   Main.cpp
               Anyness/many/TestMany-Empty.cpp
               Anyness/many/TestMany-Absorb.cpp
               Anyness/many/TestMany-Piecewise.cpp
               Anyness/many/TestMany-Convert.cpp
               Anyness/many/TestMany-CornerCases.cpp

   LIBRARIES   doctest
               LangulusAnyness
)

add_langulus_test(LangulusAnynessTest-Set
   SOURCES	   Main.cpp
               Anyness/set/TestSet-Empty.cpp
               Anyness/set/TestSet-Absorb.cpp
               Anyness/set/TestSet-Piecewise.cpp

   LIBRARIES   doctest
               LangulusAnyness
)

add_langulus_test(LangulusAnynessTest-Pair
   SOURCES	   Main.cpp
               Anyness/pair/TestPair-Empty.cpp
               Anyness/pair/TestPair-Absorb.cpp
               Anyness/pair/TestPair-Piecewise.cpp

   LIBRARIES   doctest
               LangulusAnyness
)

add_langulus_test(LangulusAnynessTest-Map
   SOURCES	   Main.cpp
               Anyness/map/TestMap-Empty.cpp
               Anyness/map/TestMap-Absorb.cpp
               Anyness/map/TestMap-Piecewise.cpp

   LIBRARIES   doctest
               LangulusAnyness
)