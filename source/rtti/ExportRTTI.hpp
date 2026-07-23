#pragma once
#include <Langulus/Core.hpp>

#if defined(LANGULUS_EXPORT_ALL) or defined(LANGULUS_EXPORT_RTTI)
   #define LANGULUS_API_RTTI() LANGULUS_EXPORT()
#else
   #define LANGULUS_API_RTTI() LANGULUS_IMPORT()
#endif

/// Make the rest of the code is aware, that Langulus::RTTI has been included 
#define LANGULUS_LIBRARY_RTTI() 1