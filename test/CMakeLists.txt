fetch_external_module(
    Catch2
    GIT_REPOSITORY  https://github.com/catchorg/Catch2.git
    GIT_TAG         ee1450f268dfd5c13aa8670ba97e93cabaf2e15d #v2.x
)

add_langulus_test(LangulusAnynessTest
	SOURCES		Main.cpp 
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
                CT/TestAllocatable.cpp
                CT/TestBool.cpp
                CT/TestCharacter.cpp
                CT/TestSuffix.cpp
                CT/TestUnfold.cpp

                Utils/TestRoof.cpp
        
                $<IF:$<BOOL:${LANGULUS_OPTION_MANAGED_MEMORY}>,Allocator/TestManaged.cpp,Allocator/TestUnmanaged.cpp>

                Anyness/TestText.cpp
                Anyness/TestTRef.cpp
                Anyness/TestTOwn.cpp
        
                Anyness/any/TestAnyDense.cpp
        
	LIBRARIES	Catch2 LangulusAnyness
)

target_compile_definitions(LangulusAnynessTest PRIVATE LANGULUS_OPTION_TESTING)
