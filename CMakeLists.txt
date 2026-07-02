cmake_minimum_required(VERSION 3.30)
project(LangulusAnyness
    VERSION         1.0.0
    DESCRIPTION     "Langulus container library"
    HOMEPAGE_URL    https://langulus.com
)
include(LangulusUtilities.cmake)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Langulus can be very demanding, and clang spawns too many compiler instances, each about 8 GB big.
# This is here to remedy the situation and avoid bricking lower spec machines while compiling.
set(CMAKE_BUILD_PARALLEL_LEVEL 4)

if (MSVC)
    set(CMAKE_CXX_FLAGS             "${CMAKE_CXX_FLAGS}             ${LANGULUS_COVERAGE_COMPILER_FLAGS_MSVC}    ${LANGULUS_SANITIZE_COMPILER_FLAGS_MSVC}")
    set(CMAKE_C_FLAGS               "${CMAKE_C_FLAGS}               ${LANGULUS_COVERAGE_COMPILER_FLAGS_MSVC}    ${LANGULUS_SANITIZE_COMPILER_FLAGS_MSVC}")
    set(CMAKE_EXE_LINKER_FLAGS      "${CMAKE_EXE_LINKER_FLAGS}      ${LANGULUS_COVERAGE_LINKER_FLAGS_MSVC}      ${LANGULUS_SANITIZE_LINKER_FLAGS_MSVC}")
    set(CMAKE_SHARED_LINKER_FLAGS   "${CMAKE_SHARED_LINKER_FLAGS}   ${LANGULUS_SANITIZE_LINKER_FLAGS_MSVC}")
    #set(CMAKE_MODULE_LINKER_FLAGS  "${CMAKE_MODULE_LINKER_FLAGS}   ${LANGULUS_SANITIZE_LINKER_FLAGS_MSVC}")
    set(CMAKE_STATIC_LINKER_FLAGS   "${CMAKE_STATIC_LINKER_FLAGS}   ${LANGULUS_SANITIZE_LINKER_FLAGS_MSVC}")
else()
    set(CMAKE_CXX_FLAGS             "${CMAKE_CXX_FLAGS}             ${LANGULUS_COVERAGE_COMPILER_FLAGS}         ${LANGULUS_SANITIZE_COMPILER_FLAGS}")
    set(CMAKE_C_FLAGS               "${CMAKE_C_FLAGS}               ${LANGULUS_COVERAGE_COMPILER_FLAGS}         ${LANGULUS_SANITIZE_COMPILER_FLAGS}")
    set(CMAKE_EXE_LINKER_FLAGS      "${CMAKE_EXE_LINKER_FLAGS}      ${LANGULUS_COVERAGE_LINKER_FLAGS}           ${LANGULUS_SANITIZE_LINKER_FLAGS}")
    set(CMAKE_SHARED_LINKER_FLAGS   "${CMAKE_SHARED_LINKER_FLAGS}   ${LANGULUS_SANITIZE_LINKER_FLAGS}")
    #set(CMAKE_MODULE_LINKER_FLAGS  "${CMAKE_MODULE_LINKER_FLAGS}   ${LANGULUS_SANITIZE_LINKER_FLAGS}")
    set(CMAKE_STATIC_LINKER_FLAGS   "${CMAKE_STATIC_LINKER_FLAGS}   ${LANGULUS_SANITIZE_LINKER_FLAGS}")
endif()

if(CMAKE_CXX_FLAGS)
    # Make sure trigraphs are disabled                                        
    STRING(FIND ${CMAKE_CXX_FLAGS} "/Zc:trigraphs" TRIGRAPHS_ENABLED1)
    STRING(FIND ${CMAKE_CXX_FLAGS} "-trigraphs"    TRIGRAPHS_ENABLED2)
    if (${TRIGRAPHS_ENABLED1} GREATER -1 OR ${TRIGRAPHS_ENABLED2} GREATER -1)
        message(FATAL_ERROR "Langulus can't be built with trigraphs enabled")
    endif()
endif()

# Reflected options                                                           
option(LANGULUS_OPTION_SHARED_LIBRARIES 
    "Will build shared libraries (DLL/SO/WASM) if enabled, \
    true by default" ON)
    
option(LANGULUS_OPTION_SAFE_MODE 
    "Overrides additional error checking and sanity checks, \
    incurs a serious runtime overhead, disabled by default" OFF)

option(LANGULUS_OPTION_PARANOIA 
    "Make sure that any unused memory is nullified, \
    incurs a serious runtime overhead, disabled by default" OFF)

option(LANGULUS_OPTION_TESTING 
    "Builds tests, disabled by default - Langulus tests are very exhaustive \
    and might increase your build time and RAM usage significantly" OFF)
    
option(LANGULUS_OPTION_BENCHMARK
    "Enable benchmarking in tests, disabled by default. Will increase test time tenfold! Will disable LANGULUS_OPTION_SAFE_MODE in order to avoid overhead!" OFF)

option(LANGULUS_OPTION_DEBUGGING 
    "Explicitly define debug mode, in the case that default flags are \
    not detected by some odd reason, disabled by default" OFF)

option(LANGULUS_OPTION_NEWDELETE 
    "Overrides new/delete operators for anything statically linked to this library, \
    or provides LANGULUS_MONOPOLIZE_MEMORY() macro for you to use to override them, if dynamically linked. \
    This feature can be enabled only if LANGULUS_OPTION_MANAGED_MEMORY is enabled" OFF)

option(LANGULUS_OPTION_UNICODE
    "Provides UTF8, UTF16, UTF32 tools and conversions inside Text containers (WIP)" OFF)

option(LANGULUS_OPTION_COMPRESSION
    "Links to zstd library and provides memory compression for use with containers (WIP)" OFF)

option(LANGULUS_OPTION_ENCRYPTION
    "Allows for memory encryption for use with any container of your liking (WIP)" OFF)

option(LANGULUS_OPTION_PROFILING
    "Performance measurements at runtime" OFF)

option(LANGULUS_OPTION_STACKTRACE
    "Enables logging of stacktrace on errors and assumptions (currently not available on GCC 14.2+)" OFF)

option(LANGULUS_OPTION_LOGGING              "Master logger switch" ON)
option(LANGULUS_LOGGER_DISABLE_FATALERRORS  "Disables fatal error logging via Logger::Fatal()" OFF)
option(LANGULUS_LOGGER_DISABLE_ERRORS       "Disables error logging via Logger::Error()" OFF)
option(LANGULUS_LOGGER_DISABLE_WARNINGS     "Disables warning logging via Logger::Warning()" OFF)
option(LANGULUS_LOGGER_DISABLE_INFOS        "Disables information logging via Logger::Info()" OFF)
option(LANGULUS_LOGGER_DISABLE_MESSAGES     "Disables message logging via Logger::Message()" OFF)
option(LANGULUS_LOGGER_DISABLE_PROMPTS      "Disables prompts logging via Logger::Prompt()" OFF)
option(LANGULUS_LOGGER_DISABLE_VERBOSE      "Disables verbose logging via Logger::Verbose()" OFF)
option(LANGULUS_LOGGER_DISABLE_SPECIALS     "Disables special message logging via Logger::Special()" OFF)
option(LANGULUS_LOGGER_DISABLE_FLOWS        "Disables flow logging via Logger::Flow()" OFF)
option(LANGULUS_LOGGER_DISABLE_INPUTS       "Disables input logging via Logger::Input()" OFF)
option(LANGULUS_LOGGER_DISABLE_NETWORKS     "Disables network logging via Logger::Network()" OFF)
option(LANGULUS_LOGGER_DISABLE_OS           "Disables OS logging via Logger::OS()" OFF)

option(LANGULUS_OPTION_MANAGED_MEMORY
    "Memory will be pooled and recycled when freed, and you can safely push any kind of pointer, \
    as long as it was allocated by the memory manager, or by the overridden new/delete feature. Enabled by default" ON)

option(LANGULUS_OPTION_MANAGED_REFLECTION
    "Reflections will be kept in a centralized location, when reflected, which speeds up \
    type comparisons, and allows you to dynamically modify the reflection at runtime. \
    This feature can be enabled only if LANGULUS_OPTION_MANAGED_MEMORY is enabled. Enabled by default" ON)

option(LANGULUS_OPTION_MEMORY_STATISTICS
    "Memory manager shall keep track of statistics, for the price of little overhead. Disabled by default" OFF)

if (LANGULUS_OPTION_SHARED_LIBRARIES)
    set(LANGULUS_LIBRARY_TYPE SHARED CACHE INTERNAL "" FORCE)
    set(CMAKE_POSITION_INDEPENDENT_CODE ON CACHE INTERNAL "" FORCE)
    message("Building Langulus as shared library")
else()
    set(LANGULUS_LIBRARY_TYPE STATIC CACHE INTERNAL "" FORCE)
    message("Building Langulus as static library")
endif()

# Check if this project is built as standalone, or as a part of something else
if (PROJECT_IS_TOP_LEVEL OR NOT LANGULUS)
    # It is very important these are set before any targets are introduced		
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)
    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)

    if (LANGULUS_OPTION_LOGGING)
        # Configure and link {fmt} library                                    
        fetch_external_module(
            fmt
            GIT_REPOSITORY https://github.com/fmtlib/fmt.git
            GIT_TAG        94ab51cb8c63c93220f0cdd1733bdeef95e77d60 # master	
        )
    endif()
endif()

set(logger_related_cpp_files     source/logger/Logger.cpp
                                 source/logger/HTML.cpp
                                 source/logger/TXT.cpp)
set(fractalloc_related_cpp_files source/fractalloc/Allocator.cpp
                                 source/fractalloc/Pool.cpp)
set(statistic_enabled "$<AND:$<BOOL:${LANGULUS_OPTION_MEMORY_STATISTICS}>,$<BOOL:${LANGULUS_OPTION_MANAGED_MEMORY}>>")

add_langulus_library(LangulusAnyness
   SOURCES $<$<BOOL:${LANGULUS_OPTION_LOGGING}>:${logger_related_cpp_files}>
           $<$<BOOL:${LANGULUS_OPTION_MANAGED_MEMORY}>:${fractalloc_related_cpp_files}>
           $<$<BOOL:${statistic_enabled}>:source/fractalloc/Statistics.cpp>
           $<$<BOOL:${LANGULUS_OPTION_MANAGED_REFLECTION}>:source/rtti/Registry.cpp>
)

if(MSVC AND NOT CMAKE_CXX_COMPILER_ID MATCHES "Clang|GNU")
    target_compile_options(LangulusAnyness PUBLIC /Zc:preprocessor)
elseif(CMAKE_CXX_COMPILER_ID MATCHES "Clang|GNU" AND NOT MSVC)
    target_compile_options(LangulusAnyness PUBLIC -Wno-trigraphs)
    target_compile_options(LangulusAnyness PRIVATE -fvisibility=hidden -fvisibility-inlines-hidden)
endif()

target_compile_features(LangulusAnyness PUBLIC cxx_std_23)
target_include_directories(LangulusAnyness PUBLIC include)
target_include_directories(LangulusAnyness PUBLIC .)
target_compile_definitions(LangulusAnyness PRIVATE LANGULUS_EXPORT_ALL)
target_link_libraries(LangulusAnyness PUBLIC $<$<BOOL:${LANGULUS_OPTION_LOGGING}>:fmt>)
#target_compile_options(LangulusAnyness PUBLIC -Wl,--icf=all)

set(LANGULUS_ALIGNMENT 16 CACHE STRING "Default langulus alignment (in bytes)")
message(STATUS "[FEATURE] Alignment was set to ${LANGULUS_ALIGNMENT} bytes")
target_compile_definitions(LangulusAnyness PUBLIC LANGULUS_ALIGNMENT=${LANGULUS_ALIGNMENT})

set(LANGULUS_HASHSIZE 32 CACHE STRING "Default langulus hash size (in bits)")
message(STATUS "[FEATURE] Hash size was set to ${LANGULUS_HASHSIZE}bit")
target_compile_definitions(LangulusAnyness PUBLIC LANGULUS_HASHSIZE=${LANGULUS_HASHSIZE})

set(LANGULUS_FPU 32 CACHE STRING "Default langulus real number size (in bits)")
message(STATUS "[FEATURE] Real number size was set to ${LANGULUS_FPU}bit")
target_compile_definitions(LangulusAnyness PUBLIC LANGULUS_FPU=${LANGULUS_FPU})

# Include tests                                                               
if (LANGULUS_OPTION_TESTING)
    enable_testing()
    add_subdirectory(test)
endif()

# Include profiler                                                            
if (LANGULUS_OPTION_BENCHMARK OR LANGULUS_OPTION_PROFILING)
   fetch_external_module(
       ctrack
       GIT_REPOSITORY  https://github.com/Epixu/ctrack.git
       GIT_TAG         persistence
   )
   target_link_libraries(LangulusAnyness PUBLIC ctrack)
   set(LANGULUS_OPTION_SAFE_MODE OFF CACHE BOOL "Disabled due to LANGULUS_OPTION_BENCHMARK or LANGULUS_OPTION_PROFILING being enabled" FORCE)

   # If there's a highscore provided with the source, use it as baseline      
   # when testing for performance regression                                  
   if(        EXISTS "ctrack_highscore.txt"
      AND NOT EXISTS "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/ctrack_highscore.txt")
      file(
         COPY        "ctrack_highscore.txt"
         DESTINATION "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}"
      )
   endif()
endif()

# Reflect all the options, so that code is aware of them                      
macro(reflect_option OPTION MESSAGE)
    if (${OPTION})
        message(STATUS "[FEATURE] ${MESSAGE} (${OPTION})")
        target_compile_definitions(LangulusAnyness PUBLIC "$<$<BOOL:${OPTION}>:${OPTION}>")
    endif()
endmacro()

reflect_option(LANGULUS_OPTION_SAFE_MODE           "Safe mode enabled")
reflect_option(LANGULUS_OPTION_PARANOIA            "Paranoid mode enabled")
reflect_option(LANGULUS_OPTION_TESTING             "Tests enabled")
reflect_option(LANGULUS_OPTION_BENCHMARK           "Benchmarking enabled")
reflect_option(LANGULUS_OPTION_DEBUGGING           "Debugging enabled")
reflect_option(LANGULUS_OPTION_LOGGING             "Logging enabled")
reflect_option(LANGULUS_OPTION_PROFILING           "Profiling enabled")
reflect_option(LANGULUS_OPTION_STACKTRACE          "Stacktracing enabled")
reflect_option(LANGULUS_OPTION_NEWDELETE           "New/Delete operator override enabled")
reflect_option(LANGULUS_OPTION_UNICODE             "Unicode text containers enabled")
reflect_option(LANGULUS_OPTION_COMPRESSION         "Compressable memory enabled")
reflect_option(LANGULUS_OPTION_ENCRYPTION          "Encryptable memory enabled")
reflect_option(LANGULUS_OPTION_MANAGED_MEMORY      "Managed memory enabled")
reflect_option(LANGULUS_OPTION_MANAGED_REFLECTION  "Managed reflection enabled")
reflect_option(LANGULUS_OPTION_MEMORY_STATISTICS   "Memory statistics enabled")
reflect_option(LANGULUS_OPTION_SHARED_LIBRARIES    "Building shared libraries enabled")

reflect_option(LANGULUS_LOGGER_DISABLE_FATALERRORS "Logger: fatal errors disabled")
reflect_option(LANGULUS_LOGGER_DISABLE_ERRORS      "Logger: regular errors disabled")
reflect_option(LANGULUS_LOGGER_DISABLE_WARNINGS    "Logger: warnings disabled")
reflect_option(LANGULUS_LOGGER_DISABLE_INFOS       "Logger: information messages disabled")
reflect_option(LANGULUS_LOGGER_DISABLE_MESSAGES    "Logger: user messages disabled")
reflect_option(LANGULUS_LOGGER_DISABLE_PROMPTS     "Logger: prompts and console input disabled")
reflect_option(LANGULUS_LOGGER_DISABLE_VERBOSE     "Logger: verbose messages disabled")
reflect_option(LANGULUS_LOGGER_DISABLE_SPECIALS    "Logger: special messages disabled")
reflect_option(LANGULUS_LOGGER_DISABLE_FLOWS       "Logger: flow messages disabled")
reflect_option(LANGULUS_LOGGER_DISABLE_INPUTS      "Logger: input event messages disabled")
reflect_option(LANGULUS_LOGGER_DISABLE_NETWORKS    "Logger: network messages disabled")
reflect_option(LANGULUS_LOGGER_DISABLE_OS          "Logger: operating system messages disabled")
