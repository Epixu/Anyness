#[[file(GLOB_RECURSE
	LANGULUS_ANYNESS_TEST_SOURCES 
	LIST_DIRECTORIES FALSE CONFIGURE_DEPENDS
	*.cpp
)]]

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
				TestNameOf.cpp
				TestSuffixOf.cpp
	      		TestTypenav.cpp
	      		TestTypeOf.cpp
				#${LANGULUS_ANYNESS_TEST_SOURCES}
	LIBRARIES	Catch2 LangulusAnyness
)

target_compile_definitions(LangulusAnynessTest PRIVATE LANGULUS_OPTION_TESTING)
