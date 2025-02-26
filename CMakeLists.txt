cmake_minimum_required(VERSION 3.28)
project(LangulusAnyness
    VERSION         1.0.0
    DESCRIPTION     "Langulus container library"
    HOMEPAGE_URL    https://langulus.com
)
include(LangulusUtilities.cmake)

option(LANGULUS_SHARED_LIBRARIES 
    "Will build shared libraries (DLL/SO/WASM) if enabled, \
    true by default" ON)

if(LANGULUS_SHARED_LIBRARIES)
	set(LANGULUS_LIBRARY_TYPE SHARED CACHE INTERNAL "" FORCE)
    set(CMAKE_POSITION_INDEPENDENT_CODE ON CACHE INTERNAL "" FORCE)
	message("Building Langulus as shared library")
else()
    set(LANGULUS_LIBRARY_TYPE STATIC CACHE INTERNAL "" FORCE)
	message("Building Langulus as static library")
endif()

# Check if this project is built as standalone, or a part of something else		
if (PROJECT_IS_TOP_LEVEL OR NOT LANGULUS)
	set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)
	set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)

	# Configure {fmt} library                                                   
	fetch_external_module(
		fmt
		GIT_REPOSITORY https://github.com/fmtlib/fmt.git
		GIT_TAG        94ab51cb8c63c93220f0cdd1733bdeef95e77d60 # master branch	
	)
endif()

file(GLOB_RECURSE
	LANGULUS_ANYNESS_SOURCES 
	LIST_DIRECTORIES FALSE CONFIGURE_DEPENDS
	source/*.cpp
)

# Build and install Anyness library												
add_library(LangulusAnyness INTERFACE)
target_compile_features(LangulusAnyness INTERFACE cxx_std_23)
target_include_directories(LangulusAnyness INTERFACE include)
target_link_libraries(LangulusAnyness INTERFACE fmt)

if(LANGULUS_TESTING)
	enable_testing()
	add_subdirectory(test)
endif()