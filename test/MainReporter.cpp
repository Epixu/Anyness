///                                                                           
/// Langulus::Tester                                                          
/// Copyright (c) 2025 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include <Langulus/Assume.hpp>
#include <Langulus/Logger.hpp>
#include <charconv>
#include <filesystem>
#include <fstream>
#include <unordered_map>

using namespace Langulus;
namespace FS = std::filesystem;

struct tool {
   struct data {
      size_t total_time_microseconds;
      size_t user_time_microseconds;
      size_t peak_memory_kb;
   };

   std::unordered_map<std::string, data> data_per_file;
};

void populate_tool_map(std::unordered_map<std::string, tool>& map, FS::path const& file);

int main(int argc, char* argv[]) {
   LglsAssert(argc == 5, "Wrong number of arguments");
   FS::path report;
   FS::path canon;

   // Read the command line arguments                                         
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

   // Populate the map of records                                             
   std::unordered_map<std::string, tool> canon_tool_map;
   populate_tool_map(canon_tool_map, canon);

   std::unordered_map<std::string, tool> dirty_tool_map;
   populate_tool_map(dirty_tool_map, report);


   std::ofstream final_report;


   return 0;
}



///                                                                           
/// IMPLEMENTATION DETAILS                                                    
///                                                                           
void populate_tool_map(std::unordered_map<std::string, tool>& map, FS::path const& file) {
   std::ifstream input(file);
   if (not input.is_open())
      return;

   for (std::string line; std::getline(input, line); ) {
      // Line example:                                                  
      // "cl.exe","HTML.cpp.obj",1859375,1640625,198256                 
      // ^         ^             ^            ^       ^                 
      // tool     file      total time    user time   RAM usage in KB   
      size_t tool_end       = line.find_first_of(',');
      size_t file_end       = line.find_first_of(',', tool_end+1);
      size_t total_time_end = line.find_first_of(',', file_end+1);
      size_t user_time_end  = line.find_first_of(',', total_time_end+1);
      size_t ram_usage_end  = line.size();

      tool& t = map[line.substr(0, tool_end)];
      tool::data& d = t.data_per_file[line.substr(tool_end+1, file_end - tool_end - 1)];
      std::from_chars(line.data() + file_end + 1,       line.data() + total_time_end, d.total_time_microseconds);
      std::from_chars(line.data() + total_time_end + 1, line.data() + user_time_end, d.user_time_microseconds);
      std::from_chars(line.data() + user_time_end + 1,  line.data() + ram_usage_end, d.peak_memory_kb);
   }
}
