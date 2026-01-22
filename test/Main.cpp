///                                                                           
/// Langulus::Tester                                                          
/// Copyright (c) 2025 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include <Langulus/Core.hpp>
LANGULUS_BOUNDARY(nullptr)
#define DOCTEST_CONFIG_IMPLEMENT
#include <doctest/doctest.h>

#if LANGULUS(BENCHMARK)
   #include <windows.h>
#endif

int main(int argc, char* argv[]) {
   doctest::Context context;
   context.applyCommandLine(argc, argv);

   #if LANGULUS(BENCHMARK)
      // Programatically dedicate a CPU that is unlikely to be used,    
      // and elevate process priority to minimize benchmarking noise    
      HANDLE process = GetCurrentProcess();
      std::cout << "Current Priority Class: " << GetPriorityClass(process) << std::endl;
      SetPriorityClass(process, HIGH_PRIORITY_CLASS);
      std::cout << "Priority set to HIGH_PRIORITY_CLASS" << std::endl;
      // A mask of 1 (0x1 in hex) corresponds to CPU core 0.            
      // For core 1, the mask is 2 (0x2). For core 2, it's 4 (0x4),     
      // and so on (2^n).                                               
      DWORD_PTR affinityMask = 0x100;                       // Core 8   
      if (SetProcessAffinityMask(process, affinityMask))
         std::cout << "Successfully set process affinity to CPU core 8." << std::endl;
      else
         std::cerr << "Failed to set process affinity. Error code: " << GetLastError() << std::endl;
   #endif

   int result = context.run();

   #if LANGULUS(BENCHMARK)
      SetPriorityClass(process, NORMAL_PRIORITY_CLASS);
      CloseHandle(process);
   #endif
   return result;
}