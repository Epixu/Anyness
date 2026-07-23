///                                                                           
/// Langulus::Tester                                                          
/// Copyright (c) 2025 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include <Langulus/Assume.hpp>
#include <Langulus/Logger.hpp>
#include <filesystem>
#include <fstream>

using namespace Langulus;
namespace FS = std::filesystem;

int main(int argc, char* argv[]) {
   LglsAssert(argc == 5, "Wrong number of arguments");
   FS::path report;
   FS::path canon;
   std::fstream canon_accumulator;
   std::ofstream final_report;

   int i = 1;
   while (i < argc) {
      if (0 == strcmp(argv[i], "-f")) {
         report = argv[++i];
         LglsAssert(not report.empty(), "Invalid report file");
         LglsAssert(FS::exists(report), "File doesn't exist: ", report.c_str());
         Logger::Info("Source: ", report.c_str());
         ++i;
      }
      else if (0 == strcmp(argv[i], "-c")) {
         canon = argv[++i];
         LglsAssert(not canon.empty(), "Invalid canon");

         if (not FS::exists(canon)) {
            Logger::Info("No canon file available at: ", canon.c_str());
            Logger::Info("A new one will start to accumulate");
         }
         else Logger::Info("Canon: ", canon.c_str());
         ++i;
      }
      else LglsError("Syntax error");
   }

   return 0;
}