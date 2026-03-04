fetch_external_module(
    doctest
    GIT_REPOSITORY  https://github.com/doctest/doctest.git
    GIT_TAG         1da23a3e8119ec5cce4f9388e91b065e20bf06f5 #v2.4.12
)

add_langulus_test(LangulusAnynessTest
   SOURCES	   Main.cpp 
               #TestAssume.cpp
               #TestHashOf.cpp
               #TestIntentOf.cpp
               #TestLambda.cpp
               #TestLiteral.cpp
               #TestLogger.cpp
               #TestMetaOf.cpp
               #TestNameOf.cpp
               #TestRetype.cpp
               #TestSequence.cpp
               #TestTag.cpp
               #TestTypenav.cpp
               #TestTypeOf.cpp
               #TestTypes.cpp
               #
               #CT/TestAbstract.cpp
               #CT/TestAllocatable.cpp
               #CT/TestBool.cpp
               #CT/TestCharacter.cpp
               #CT/TestConvertible.cpp
               #CT/TestDeep.cpp
               #CT/TestSuffix.cpp
               #CT/TestUnfold.cpp
               #
               #Utils/TestRoof.cpp
               #
               #$<IF:$<BOOL:${LANGULUS_OPTION_MANAGED_MEMORY}>,Allocator/TestManaged.cpp,Allocator/TestUnmanaged.cpp>
               #
               #Anyness/TestText.cpp
               #Anyness/TestBytes.cpp
               #Anyness/TestTRef.cpp
               #Anyness/TestTOwn.cpp
               #
               #Anyness/any/TestAny-Empty.cpp
               #Anyness/any/TestAny-Absorb.cpp
               #Anyness/any/TestAny-Piecewise.cpp
               #Anyness/any/TestAny-Convert.cpp
               #
               #Anyness/many/TestMany-Empty.cpp
               #Anyness/many/TestMany-Absorb.cpp
               #Anyness/many/TestMany-Piecewise.cpp
               #Anyness/many/TestMany-Convert.cpp

               Anyness/set/TestSet-Empty.cpp

   LIBRARIES   doctest
               LangulusAnyness
)

target_compile_definitions(LangulusAnynessTest PRIVATE LANGULUS_OPTION_TESTING)
