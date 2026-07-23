///                                                                           
/// Langulus::Tester                                                          
/// Copyright (c) 2025 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include <Langulus/Assume.hpp>
#include <Langulus/Logger.hpp>
#include <Langulus/Logger/HTML.hpp>
#include <charconv>
#include <filesystem>
#include <fstream>
#include <unordered_map>

using namespace Langulus;
namespace FS = std::filesystem;

struct tool {
   struct data {
      long long total_time_microseconds;
      long long user_time_microseconds;
      long long peak_memory_kb;
   };

   std::unordered_map<std::string, data> data_per_file;
   data total;
};

using tool_map = std::unordered_map<std::string, tool>;

struct difference {
   tool_map changes;
   tool::data total;
   tool_map anomalies;
};

void populate_tool_map(tool_map& map, FS::path const& file);
bool compare_or_canonize(tool_map& dst, tool_map const& src, difference& diff, float tolerance = 0.3f);
void write_new_canon(tool_map const& canon, FS::path const& file);
int report_anomalies(difference const& diff, FS::path const& file);

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
            LglsAssert(FS::create_directories(canon.parent_path()), "Can't create persist directory");
         }
         else Logger::Info("Canon: ", canon.c_str());
         ++i;
      }
      else LglsError("Syntax error");
   }

   // Populate the map of records                                       
   tool_map canon_tool_map;
   populate_tool_map(canon_tool_map, canon);

   tool_map dirty_tool_map;
   populate_tool_map(dirty_tool_map, report);

   difference diff;
   bool rewrite_canon_file = compare_or_canonize(canon_tool_map, dirty_tool_map, diff);

   if (rewrite_canon_file)
      write_new_canon(canon_tool_map, canon);

   return report_anomalies(diff, "anomalies.htm");
}



///                                                                           
/// IMPLEMENTATION DETAILS                                                    
///                                                                           


/// Reads a file into a map                                                   
void populate_tool_map(tool_map& map, FS::path const& file) {
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

      t.total.total_time_microseconds += d.total_time_microseconds;
      t.total.user_time_microseconds += d.user_time_microseconds;
      t.total.peak_memory_kb += d.peak_memory_kb;
   }
}

/// Compares two maps, adds overhead to final report, while missing entries   
/// get filled inside canon without registering a difference.                 
///   @return true if new entries were canonized                              
bool compare_or_canonize(tool_map& canon, tool_map const& dirty, difference& diff, float tolerance) {
   bool new_canonized_entries = false;

   for (auto& d : dirty) {
      auto found = canon.find(d.first);
      if (found != canon.end()) {
         // Tool was found - compare files                              
         for (auto& f : d.second.data_per_file) {
            auto found_f = found->second.data_per_file.find(f.first);
            if (found_f != found->second.data_per_file.end()) {
               // File was found - compare and fill report              
               auto& diff_total = diff.changes[d.first];
               auto& diff_file  = diff_total.data_per_file[f.first];

               diff_file.total_time_microseconds = f.second.total_time_microseconds - found_f->second.total_time_microseconds;
               diff_file.user_time_microseconds  = f.second.user_time_microseconds  - found_f->second.user_time_microseconds;
               diff_file.peak_memory_kb          = f.second.peak_memory_kb          - found_f->second.peak_memory_kb;

               diff_total.total.total_time_microseconds += diff_file.total_time_microseconds;
               diff_total.total.user_time_microseconds  += diff_file.user_time_microseconds;
               diff_total.total.peak_memory_kb          += diff_file.peak_memory_kb;

               diff.total.total_time_microseconds += diff_file.total_time_microseconds;
               diff.total.user_time_microseconds  += diff_file.user_time_microseconds;
               diff.total.peak_memory_kb          += diff_file.peak_memory_kb;

               double total_time_score = 0;
               double user_time_score = 0;
               double peak_memory_score = 0;

               if (f.second.total_time_microseconds)
                  total_time_score = 1.0f - static_cast<double>(found_f->second.total_time_microseconds) / static_cast<double>(f.second.total_time_microseconds);
               if (f.second.user_time_microseconds)
                  user_time_score = 1.0f - static_cast<double>(found_f->second.user_time_microseconds) / static_cast<double>(f.second.user_time_microseconds);
               if (f.second.peak_memory_kb)
                  peak_memory_score = 1.0f - static_cast<double>(found_f->second.peak_memory_kb) / static_cast<double>(f.second.peak_memory_kb);

               if (fabs(total_time_score)  > tolerance
               or  fabs(user_time_score)   > tolerance
               or  fabs(peak_memory_score) > tolerance) {
                  auto& anomalous_tool = diff.anomalies[d.first];
                  auto& anomalous_file = anomalous_tool.data_per_file[f.first];

                  anomalous_tool.total = diff_total.total;
                  anomalous_file = diff_file;
               }
            }
            else {
               // File was not found, canonize it in particular.        
               // Don't register a difference.                          
               canon[d.first].data_per_file[f.first] = f.second;
               new_canonized_entries = true;
            }
         }
      }
      else {
         // Tool was not found, canonize all files in there.            
         // Don't register a difference.                                
         canon[d.first] = d.second;
         new_canonized_entries = true;
      }
   }

   return new_canonized_entries;
}

/// Write a new canon                                                         
void write_new_canon(tool_map const& canon, FS::path const& file) {
   std::ofstream output(file);
   LglsAssert(output.is_open(), "Can't open file: ", file.c_str());

   for (auto& t : canon) {
      for (auto& f : t.second.data_per_file) {
         output << t.first << ',';
         output << f.first << ',';
         output << f.second.total_time_microseconds << ',';
         output << f.second.user_time_microseconds << ',';
         output << f.second.peak_memory_kb << '\n';
      }
   }
}

/// Report anomalies                                                          
int report_anomalies(difference const& diff, FS::path const& file) {
   if (diff.anomalies.empty())
      return 0;

   Logger::ToHTML html_anomalies {file.string()};
   Logger::AttachDuplicator(&html_anomalies);

   std::ofstream output(file);
   LglsAssert(output.is_open(), "Can't open file: ", file.c_str());

   for (auto& t : diff.anomalies) {
      auto _ = Logger::InfoSection("Anomalies in ", t.first, ":");
      for (auto& f : t.second.data_per_file) {
         Logger::Line(f.first, ": ");

         if (f.second.total_time_microseconds < 0)
            Logger::Append(Logger::Green, "build time improved by ", f.second.total_time_microseconds, " microseconds");
         else
            Logger::Append(Logger::Red, "build time worsened by ", f.second.total_time_microseconds, " microseconds");

         if (f.second.peak_memory_kb < 0)
            Logger::Append(Logger::Green, "; build RAM usage reduced by ", f.second.peak_memory_kb, " KB");
         else
            Logger::Append(Logger::Red, "; build RAM usage increased by ", f.second.peak_memory_kb, " KB");
      }
   }

   Logger::DettachDuplicator(&html_anomalies);
   return diff.anomalies.size();
}
