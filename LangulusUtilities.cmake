include(FetchContent)

# Utility for fetching external libraries using FetchContent                  
function(fetch_external_module NAME GIT_REPOSITORY REPO GIT_TAG TAG)
    if(NOT DEFINED LANGULUS_EXTERNAL_DIRECTORY)
        set(LANGULUS_EXTERNAL_DIRECTORY "${CMAKE_SOURCE_DIR}/external" CACHE PATH
            "Place where external dependencies will be downloaded")
        message(STATUS "[LANGULUS] LANGULUS_EXTERNAL_DIRECTORY not defined, \
using default: ${LANGULUS_EXTERNAL_DIRECTORY}")
    endif()

   # Completely avoid downloading or updating anything, once the appropriate  
   # folder exists                                                            
   string(TOUPPER ${NAME} UPPERCASE_NAME)
   if (EXISTS "${LANGULUS_EXTERNAL_DIRECTORY}/${NAME}-src")
      set(FETCHCONTENT_SOURCE_DIR_${UPPERCASE_NAME} "${LANGULUS_EXTERNAL_DIRECTORY}/${NAME}-src" CACHE INTERNAL "" FORCE)
      message(STATUS "[LANGULUS] Reusing the cached external library ${NAME}")
      message(STATUS "[LANGULUS] (delete ${LANGULUS_EXTERNAL_DIRECTORY}/${NAME}-src \
manually if you want to redownload)")
   else()
      unset(FETCHCONTENT_SOURCE_DIR_${UPPERCASE_NAME} CACHE)
      message(STATUS "[LANGULUS] Freshly downloading external library ${NAME} from ${REPO} ...")
   endif()

   FetchContent_Declare(
      ${NAME}
      GIT_REPOSITORY  ${REPO}
      GIT_TAG         ${TAG}
      SOURCE_DIR      "${LANGULUS_EXTERNAL_DIRECTORY}/${NAME}-src"
      SUBBUILD_DIR    "${CMAKE_BINARY_DIR}/external/${NAME}-subbuild"
      ${ARGN}
      EXCLUDE_FROM_ALL
   )
   FetchContent_MakeAvailable(${NAME})

   string(TOLOWER ${NAME} LOWERCASE_NAME)
   set(${NAME}_SOURCE_DIR "${${LOWERCASE_NAME}_SOURCE_DIR}" CACHE INTERNAL "${NAME} source directory")
   set(${NAME}_BINARY_DIR "${${LOWERCASE_NAME}_BINARY_DIR}" CACHE INTERNAL "${NAME} binary directory")
endfunction()

# Create a library dependent on build configuration                           
function(add_langulus_library NAME)
   set(multiValueArgs SOURCES LIBRARIES DEPENDENCIES EMSCRIPTEN_COMPILE_FLAGS EMSCRIPTEN_LINK_FLAGS)
   cmake_parse_arguments(PARSE_ARGV 0 arg "" "" "${multiValueArgs}")

   if (EMSCRIPTEN AND LANGULUS_OPTION_SHARED_LIBRARIES)
      # When building for emscripten, we "fake" a shared library by           
      # creating an executable with exports and no entry point                
      # This "fake" shared library has to be linked in a specific way         
      # only from wasm MAIN_MODULEs, so that both LangulusCore                
      # interface is inherited, and the shared library is loaded in           
      # at startup. Here's an example:                                        
      #	target_link_libraries(LangulusLoggerTest PRIVATE                     
      #		$<TARGET_FILE:LangulusLogger> LangulusLogger                      
      #		^ 							  ^                                          
      #		+- Links the *.wasm file      +- Inherits interface               
      # https://github.com/emscripten-core/emscripten/issues/17804            
      add_executable(${NAME} ${arg_SOURCES})
      set_target_properties(${NAME} PROPERTIES
         ENABLE_EXPORTS ON
         COMPILE_FLAGS  "-sSIDE_MODULE --no-entry -fPIC ${arg_EMSCRIPTEN_COMPILE_FLAGS}"
         LINK_FLAGS     "-sSIDE_MODULE -sWASM=1 --no-entry -fPIC ${arg_EMSCRIPTEN_LINK_FLAGS}"
         SUFFIX		   ".wasm"
      )

      # When building for emscripten, our shared libraries are "fake"         
      # and have to be linked in a specific way from wasm MAIN_MODULEs        
      # - once by using the *.wasm file, and once by using the shared         
      # library target.                                                       
      # Any *.so files on the other hand must be packed in a *.data           
      # file by using --preload-file with all the required mods, like         
      # so: ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/LangulusMod*.so                 
      foreach(ITEM ${arg_LIBRARIES})
         target_link_libraries(${NAME}
            PRIVATE $<IF:$<TARGET_EXISTS:${ITEM}>,$<TARGET_FILE:${ITEM}>, >	
                     ${ITEM}
         )
      endforeach()
   else()
      add_library(${NAME} ${LANGULUS_LIBRARY_TYPE} ${arg_SOURCES})
      target_link_libraries(${NAME} PRIVATE ${arg_LIBRARIES})
   endif()
      
   foreach(ITEM ${arg_DEPENDENCIES})
      add_dependencies(${NAME} ${ITEM})
   endforeach()
endfunction()

# Create an executable															
function(add_langulus_app NAME)
   set(multiValueArgs SOURCES LIBRARIES DEPENDENCIES EMSCRIPTEN_COMPILE_FLAGS EMSCRIPTEN_LINK_FLAGS)
   cmake_parse_arguments(PARSE_ARGV 0 arg "" "" "${multiValueArgs}")
	add_executable(${NAME} ${arg_SOURCES})

   if (EMSCRIPTEN)
      # Pack all dependencies into a *.data file                              
      foreach(ITEM ${arg_DEPENDENCIES})
         string(APPEND arg_EMSCRIPTEN_LINK_FLAGS
            " --preload-file ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${ITEM}.so@${ITEM}.so")
      endforeach()

      # attention: -fwasm-exception are currently supported in several
      # major web browsers, but may not be supported in all			
      # WebAssembly engines yet. Also -sASYNCIFY isn't compatible!	
      set_target_properties(${NAME} PROPERTIES
         COMPILE_FLAGS  "-sMAIN_MODULE -fwasm-exceptions ${arg_EMSCRIPTEN_COMPILE_FLAGS}"
         LINK_FLAGS     "-sMAIN_MODULE -sWASM=1 --emrun -sALLOW_MEMORY_GROWTH -fwasm-exceptions ${arg_EMSCRIPTEN_LINK_FLAGS}"
         SUFFIX         ".html"
      )

      # When building for emscripten, our shared libraries are "fake"         
      # and have to be linked in a specific way from wasm MAIN_MODULEs        
      # - once by using the *.wasm file, and once by using the shared         
      # library target.                                                       
      # Any *.so files on the other hand must be packed in a *.data           
      # file by using --preload-file with all the required mods, like         
      # so: ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/LangulusMod*.so                 
      foreach(ITEM ${arg_LIBRARIES})
         target_link_libraries(${NAME}
            PRIVATE  $<IF:$<TARGET_EXISTS:${ITEM}>,$<TARGET_FILE:${ITEM}>, >	
                     ${ITEM}
         )
      endforeach()
   else()
      target_link_libraries(${NAME} PRIVATE ${arg_LIBRARIES})
   endif()

   foreach(ITEM ${arg_DEPENDENCIES})
      add_dependencies(${NAME} ${ITEM})
   endforeach()
endfunction()

# Create a test executable if tests are enabled                               
function(add_langulus_test NAME)
   add_langulus_app(${NAME} ${ARGN})
   add_test(
      NAME              ${NAME}
      COMMAND           ${NAME}
      WORKING_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
   )
   target_compile_definitions(${NAME}
      PRIVATE     LANGULUS_OPTION_TESTING
                  DOCTEST_CONFIG_SUPER_FAST_ASSERTS
   )
   message(STATUS "[LANGULUS] Test added: ${NAME}")
endfunction()