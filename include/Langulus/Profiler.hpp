///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Core.hpp"

#if LANGULUS(BENCHMARK) or LANGULUS(PROFILING)
	#include <ctrack.hpp>
#else
	#define CTRACK_PROD
	#define CTRACK_PROD_PERSIST
	#define CTRACK_PROD_NAME(name)
	#define CTRACK_PROD_NAME_PERSIST(name)
	#define CTRACK_DEV
	#define CTRACK_DEV_PERSIST
	#define CTRACK_DEV_NAME(name)
	#define CTRACK_DEV_NAME_PERSIST(name)
	#define CTRACK
	#define CTRACK_PERSIST
	#define CTRACK_NAME(name)
	#define CTRACK_NAME_PERSIST(name)
#endif