///                                                                           
/// Langulus::Logger                                                          
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Core.hpp"

#if LANGULUS_FEATURE(LOGGING)

#include <fmt/format.h>
#include <fmt/color.h>
#include <fmt/chrono.h>
#include <array>


namespace Langulus::CT
{

   /// Anything formattable by fmt is also loggable                           
   /// You can extend this concept by specializing fmt::formatter yourself    
   template<class...T>
   concept Loggable = Inner::CheckSize<T...>()
       and (::fmt::is_formattable<T>::value and ...);

   template<class...T>
   concept NotLoggable = Inner::CheckSize<T...>()
       and ((not Loggable<T>) and ...);

} // namespace Langulus::CT

#endif 

namespace Langulus::Logger
{

   /// Color codes, consistent with ANSI/VT100 escapes                        
   /// Also consistent with fmt::terminal_color                               
   enum class Color : unsigned {
      NoForeground = 0,
      NoBackground = 1,

      Black = 30,
      DarkRed,
      DarkGreen,
      DarkYellow,
      DarkBlue,
      DarkPurple,
      DarkCyan,
      Gray,

      BlackBgr = 40,
      DarkRedBgr,
      DarkGreenBgr,
      DarkYellowBgr,
      DarkBlueBgr,
      DarkPurpleBgr,
      DarkCyanBgr,
      GrayBgr,

      DarkGray = 90,
      Red,
      Green,
      Yellow,
      Blue,
      Purple,
      Cyan,
      White,

      DarkGrayBgr = 100,
      RedBgr,
      GreenBgr,
      YellowBgr,
      BlueBgr,
      PurpleBgr,
      CyanBgr,
      WhiteBgr
   };

   /// Some formatting styles, consistent with fmt::emphasis                  
   enum class Emphasis : uint8_t {
      Default     = 0,			
      Bold        = 1,			// Not working on windows                 
      Faint       = 1 << 1,	// Not working on windows                 
      Italic      = 1 << 2,	// Not working on windows                 
      Underline   = 1 << 3,
      Blink       = 1 << 4,	// Not working on windows                 
      Reverse     = 1 << 5,
      Conceal     = 1 << 6,	// Not working on windows                 
      Strike      = 1 << 7,	// Not working on windows                 
   };

   constexpr bool operator & (const Emphasis& lhs, const Emphasis& rhs) noexcept {
      return (static_cast<uint8_t>(lhs) & static_cast<uint8_t>(rhs))
          ==  static_cast<uint8_t>(rhs);
   }

   /// Console commands                                                       
   enum class Command : uint8_t {
      Clear,		// Clear the console                                  
      NewLine,		// Write a new line, with a timestamp and tabulation  
      Invert,		// Inverts background and foreground colors           
      Reset,		// Reset the style                                    
      Stylize,    // Apply the last style                               
      Time,			// Write a short timestamp                            
      ExactTime,	// Write an exhaustive timestamp                      
   };
   
   /// Types of predefined messages, each with its unique style and search    
   /// patterns.                                                              
   enum class Intent {
      FatalError = 0,
      Error,
      Warning,
      Verbose,
      Info,
      Message,
      Special,
      Flow,
      Input,
      Network,
      OS,
      Prompt,
      Ignore,

      Counter
   };
   
   /// GCC equates templates with enum types as their underlying type, so we  
   /// are forced to define these anums as enum class, and then do using enum 
   using enum Color;
   using enum Emphasis;
   using enum Command;

#if LANGULUS_FEATURE(LOGGING)

   /// Text style, with background color, foreground color, and emphasis      
   using Style = fmt::text_style;

   /// Can be used to specify each intent's style and grep patterns           
   struct IntentProperties {
      char  prefix[5];
      Style style;
      bool  silenced = false;
   };

   /// Default intent styling                                                 
   constexpr IntentProperties DefaultIntentStyle[static_cast<int>(Intent::Counter)] = {
      {"|F| ", fmt::fg(fmt::terminal_color::red           )},  // FatalError  
      {"|E| ", fmt::fg(fmt::terminal_color::bright_red    )},  // Error       
      {"|W| ", fmt::fg(fmt::terminal_color::yellow        )},  // Warning     
      {"|V| ", fmt::fg(fmt::terminal_color::bright_black  )},  // Verbose     
      {"|I| ", fmt::fg(fmt::terminal_color::white         )},  // Info        
      {"|M| ", fmt::fg(fmt::terminal_color::bright_white  )},  // Message     
      {"|S| ", fmt::fg(fmt::terminal_color::bright_magenta)},  // Special     
      {"|L| ", fmt::fg(fmt::terminal_color::cyan          )},  // Flow        
      {"|N| ", fmt::fg(fmt::terminal_color::bright_blue   )},  // Input       
      {"|T| ", fmt::fg(fmt::terminal_color::bright_yellow )},  // Network     
      {"|O| ", fmt::fg(fmt::terminal_color::blue          )},  // OS          
      {"|P| ", fmt::fg(fmt::terminal_color::bright_green  )},  // Prompt      
      {"| | ", fmt::fg(fmt::terminal_color::bright_green  )}   // Ignore      
   };

   // Tabulator color and formatting customization                      
   constexpr Intent DefaultIntent = Intent::Info;
   constexpr Style  DefaultStyle  = fmt::fg(fmt::terminal_color::bright_black);

   namespace Detail
   {

      /// Write styling escape sequence to stdout                             
      LANGULUS(INLINED)
      void FmtPrintStyle(const Style& style) noexcept {
         if (style.has_emphasis()) {
            const auto e = fmt::detail::make_emphasis<char>(style.get_emphasis());
            fmt::print("{}", e.begin());
         }

         if (style.has_foreground()) {
            const auto f = fmt::detail::make_foreground_color<char>(style.get_foreground());
            fmt::print("{}", f.begin());
         }

         if (style.has_background()) {
            const auto b = fmt::detail::make_background_color<char>(style.get_background());
            fmt::print("{}", b.begin());
         }
      }

      /// Write a short timestamp in the current system time zone             
      LANGULUS(INLINED)
      void FmtPrintTime() noexcept {
         try {
            using Clock = ::std::chrono::system_clock;
            const auto now = Clock::to_time_t(Clock::now());
            fmt::print("{:%T}", fmt::localtime(now));
         }
         catch (...) { fmt::print("<timestamp error>"); }
      }

      /// Write anything stringifiable to stdout                              
      ///   @param data - the data to write                                   
      LANGULUS(INLINED)
      void FmtWrite(const auto& data) noexcept {
         try { fmt::print("{}", data); }
         catch (...) { fmt::print("<stringification error>"); }
         fflush(stdout);
      }

   } // namespace Langulus::Logger::Detail

#endif
   
   /// Generate hexadecimal string from a given value                         
   ///   @param from - the argument                                           
   ///   @return the hex string in the form of std::array                     
   auto Hex(const auto& from) {
      ::std::array<char, sizeof(from) * 2> result {};
      auto from_bytes = reinterpret_cast<const std::byte*>(&from);
      auto to_bytes = result.data();
      for (size_t i = 0; i < sizeof(from); ++i)
         ::fmt::format_to_n(to_bytes + i * 2, 2, "{:02X}", from_bytes[i]);
      return result;
   }
   
   /// A general new-line write function that continues the last style        
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void LineRaw(T&&...arguments) noexcept {
      #if LANGULUS_FEATURE(LOGGING)
         if constexpr (TOGGLE) {
            if not consteval {
               Detail::FmtWrite("\n            ");
               (Detail::FmtWrite(FWD(arguments)), ...);
            }
         }
      #else
         LANGULUS(NOOP);
      #endif
   }

   /// A general same-line write function that continues the last style       
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void AppendRaw(T&&...arguments) noexcept {
      #if LANGULUS_FEATURE(LOGGING)
         if constexpr (TOGGLE) {
            if not consteval {
               (Detail::FmtWrite(FWD(arguments)), ...);
            }
         }
      #else
         LANGULUS(NOOP);
      #endif
   }

   /// Write a new-line fatal error                                           
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void FatalRaw([[maybe_unused]] T&&...arguments) noexcept {
      #if defined(LANGULUS_LOGGER_DISABLE_FATALERRORS) or not LANGULUS_FEATURE(LOGGING)
         LANGULUS(NOOP);
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               Detail::FmtWrite("\n");
               Detail::FmtPrintStyle(DefaultStyle);
               Detail::FmtPrintTime();
               auto& style = DefaultIntentStyle[static_cast<int>(Intent::FatalError)];
               Detail::FmtWrite(style.prefix);
               Detail::FmtPrintStyle(style.style);
               (Detail::FmtWrite(FWD(arguments)), ...);
            }
         }
      #endif
   }

   /// Write a new-line error                                                 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void ErrorRaw([[maybe_unused]] T&&...arguments) noexcept {
      #if defined(LANGULUS_LOGGER_DISABLE_ERRORS) or not LANGULUS_FEATURE(LOGGING)
         LANGULUS(NOOP);
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               Detail::FmtWrite("\n");
               Detail::FmtPrintStyle(DefaultStyle);
               Detail::FmtPrintTime();
               auto& style = DefaultIntentStyle[static_cast<int>(Intent::Error)];
               Detail::FmtWrite(style.prefix);
               Detail::FmtPrintStyle(style.style);
               (Detail::FmtWrite(FWD(arguments)), ...);
            }
         }
      #endif
   }

   /// Write a new-line warning                                               
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void WarningRaw([[maybe_unused]] T&&...arguments) noexcept {
      #if defined(LANGULUS_LOGGER_DISABLE_WARNINGS) or not LANGULUS_FEATURE(LOGGING)
         LANGULUS(NOOP);
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               Detail::FmtWrite("\n");
               Detail::FmtPrintStyle(DefaultStyle);
               Detail::FmtPrintTime();
               auto& style = DefaultIntentStyle[static_cast<int>(Intent::Warning)];
               Detail::FmtWrite(style.prefix);
               Detail::FmtPrintStyle(style.style);
               (Detail::FmtWrite(FWD(arguments)), ...);
            }
         }
      #endif
   }

   /// Write a new-line with verbose information                              
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void VerboseRaw([[maybe_unused]] T&&...arguments) noexcept {
      #if defined(LANGULUS_LOGGER_DISABLE_VERBOSE) or not LANGULUS_FEATURE(LOGGING)
         LANGULUS(NOOP);
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               Detail::FmtWrite("\n");
               Detail::FmtPrintStyle(DefaultStyle);
               Detail::FmtPrintTime();
               auto& style = DefaultIntentStyle[static_cast<int>(Intent::Verbose)];
               Detail::FmtWrite(style.prefix);
               Detail::FmtPrintStyle(style.style);
               (Detail::FmtWrite(FWD(arguments)), ...);
            }
         }
      #endif
   }

   /// Write a new-line with information                                      
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void InfoRaw([[maybe_unused]] T&&...arguments) noexcept {
      #if defined(LANGULUS_LOGGER_DISABLE_INFOS) or not LANGULUS_FEATURE(LOGGING)
         LANGULUS(NOOP);
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               Detail::FmtWrite("\n");
               Detail::FmtPrintStyle(DefaultStyle);
               Detail::FmtPrintTime();
               auto& style = DefaultIntentStyle[static_cast<int>(Intent::Info)];
               Detail::FmtWrite(style.prefix);
               Detail::FmtPrintStyle(style.style);
               (Detail::FmtWrite(FWD(arguments)), ...);
            }
         }
      #endif
   }

   /// Write a new-line with a personal message                               
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void MessageRaw([[maybe_unused]] T&&...arguments) noexcept {
      #if defined(LANGULUS_LOGGER_DISABLE_MESSAGES) or not LANGULUS_FEATURE(LOGGING)
         LANGULUS(NOOP);
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               Detail::FmtWrite("\n");
               Detail::FmtPrintStyle(DefaultStyle);
               Detail::FmtPrintTime();
               auto& style = DefaultIntentStyle[static_cast<int>(Intent::Message)];
               Detail::FmtWrite(style.prefix);
               Detail::FmtPrintStyle(style.style);
               (Detail::FmtWrite(FWD(arguments)), ...);
            }
         }
      #endif
   }

   /// Write a new-line with special text                                     
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void SpecialRaw([[maybe_unused]] T&&...arguments) noexcept {
      #if defined(LANGULUS_LOGGER_DISABLE_SPECIALS) or not LANGULUS_FEATURE(LOGGING)
         LANGULUS(NOOP);
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               Detail::FmtWrite("\n");
               Detail::FmtPrintStyle(DefaultStyle);
               Detail::FmtPrintTime();
               auto& style = DefaultIntentStyle[static_cast<int>(Intent::Special)];
               Detail::FmtWrite(style.prefix);
               Detail::FmtPrintStyle(style.style);
               (Detail::FmtWrite(FWD(arguments)), ...);
            }
         }
      #endif
   }

   /// Write a new-line with flow information                                 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void FlowRaw([[maybe_unused]] T&&...arguments) noexcept {
      #if defined(LANGULUS_LOGGER_DISABLE_FLOWS) or not LANGULUS_FEATURE(LOGGING)
         LANGULUS(NOOP);
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               Detail::FmtWrite("\n");
               Detail::FmtPrintStyle(DefaultStyle);
               Detail::FmtPrintTime();
               auto& style = DefaultIntentStyle[static_cast<int>(Intent::Flow)];
               Detail::FmtWrite(style.prefix);
               Detail::FmtPrintStyle(style.style);
               (Detail::FmtWrite(FWD(arguments)), ...);
            }
         }
      #endif
   }

   /// Write a new-line on user input                                         
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void InputRaw([[maybe_unused]] T&&...arguments) noexcept {
      #if defined(LANGULUS_LOGGER_DISABLE_INPUTS) or not LANGULUS_FEATURE(LOGGING)
         LANGULUS(NOOP);
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               Detail::FmtWrite("\n");
               Detail::FmtPrintStyle(DefaultStyle);
               Detail::FmtPrintTime();
               auto& style = DefaultIntentStyle[static_cast<int>(Intent::Input)];
               Detail::FmtWrite(style.prefix);
               Detail::FmtPrintStyle(style.style);
               (Detail::FmtWrite(FWD(arguments)), ...);
            }
         }
      #endif
   }

   /// Write a new-line with network message                                  
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void NetworkRaw([[maybe_unused]] T&&...arguments) noexcept {
      #if defined(LANGULUS_LOGGER_DISABLE_NETWORKS) or not LANGULUS_FEATURE(LOGGING)
         LANGULUS(NOOP);
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               Detail::FmtWrite("\n");
               Detail::FmtPrintStyle(DefaultStyle);
               Detail::FmtPrintTime();
               auto& style = DefaultIntentStyle[static_cast<int>(Intent::Network)];
               Detail::FmtWrite(style.prefix);
               Detail::FmtPrintStyle(style.style);
               (Detail::FmtWrite(FWD(arguments)), ...);
            }
         }
      #endif
   }

   /// Write a new-line with a message from OS                                
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void OSRaw([[maybe_unused]] T&&...arguments) noexcept {
      #if defined(LANGULUS_LOGGER_DISABLE_OS) or not LANGULUS_FEATURE(LOGGING)
         LANGULUS(NOOP);
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               Detail::FmtWrite("\n");
               Detail::FmtPrintStyle(DefaultStyle);
               Detail::FmtPrintTime();
               auto& style = DefaultIntentStyle[static_cast<int>(Intent::OS)];
               Detail::FmtWrite(style.prefix);
               Detail::FmtPrintStyle(style.style);
               (Detail::FmtWrite(FWD(arguments)), ...);
            }
         }
      #endif
   }

   /// Write a new-line with an input prompt                                  
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void PromptRaw([[maybe_unused]] T&&...arguments) noexcept {
      #if defined(LANGULUS_LOGGER_DISABLE_PROMPTS) or not LANGULUS_FEATURE(LOGGING)
         LANGULUS(NOOP);
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               Detail::FmtWrite("\n");
               Detail::FmtPrintStyle(DefaultStyle);
               Detail::FmtPrintTime();
               auto& style = DefaultIntentStyle[static_cast<int>(Intent::Prompt)];
               Detail::FmtWrite(style.prefix);
               Detail::FmtPrintStyle(style.style);
               (Detail::FmtWrite(FWD(arguments)), ...);
            }
         }
      #endif
   }

   /// Gets a size stringified in a human readable way as KB, MB, GB, etc.    
   struct Size {
		size_t bytes;

      ::std::string format() const {
         ::std::ostringstream oss;
         oss << ::std::setprecision(3);
         
         if (bytes < 1'000LL)
            oss << bytes << " B";
         else if (bytes < 1'000'000LL)
            oss << (bytes * 1. / 1000LL) << " KB";
         else if (bytes < 1'000'000'000LL)
            oss << (bytes * 1. / 1000'000LL) << " MB";
         else if (bytes < 1'000'000'000'000LL)
            oss << (bytes * 1. / 1000'000'000LL) << " GB";
         else if (bytes < 1'000'000'000'000'000LL)
            oss << (bytes * 1. / 1000'000'000'000LL) << " TB";
         else
            oss << (bytes * 1. / 1000'000'000'000'000LL) << " PB";
         
         return oss.str();
      }
	};

   // bytes only with integer
   constexpr Size operator"" _B(unsigned long long int num) noexcept {
      return {num};
   }

   // floating-point numbers, like 5.5_kB
   constexpr Size operator"" _KiB(long double num) noexcept {
      return {static_cast<size_t>((1LL << 10) * num)};
   }
   constexpr Size operator"" _MiB(long double num) noexcept {
      return {static_cast<size_t>((1LL << 20) * num)};
   }
   constexpr Size operator"" _GiB(long double num) noexcept {
      return {static_cast<size_t>((1LL << 30) * num)};
   }
   constexpr Size operator"" _TiB(long double num) noexcept {
      return {static_cast<size_t>((1LL << 40) * num)};
   }
   constexpr Size operator"" _PiB(long double num) noexcept {
      return {static_cast<size_t>((1LL << 50) * num)};
   }
   constexpr Size operator"" _KB(long double num) noexcept {
      return {static_cast<size_t>(1'000LL * num)};
   }
   constexpr Size operator"" _MB(long double num) noexcept {
      return {static_cast<size_t>(1'000'000LL * num)};
   }
   constexpr Size operator"" _GB(long double num) noexcept {
      return {static_cast<size_t>(1'000'000'000LL * num)};
   }
   constexpr Size operator"" _TB(long double num) noexcept {
      return {static_cast<size_t>(1'000'000'000'000LL * num)};
   }
   constexpr Size operator"" _PB(long double num) noexcept {
      return {static_cast<size_t>(1'000'000'000'000'000LL * num)};
   }

   // repeated for integer literals so that e.g. 5_kB works
   constexpr Size operator"" _KiB(unsigned long long num) noexcept {
      return {(1LL << 10) * num};
   }
   constexpr Size operator"" _MiB(unsigned long long num) noexcept {
      return {(1LL << 20) * num};
   }
   constexpr Size operator"" _GiB(unsigned long long num) noexcept {
      return {(1LL << 30) * num};
   }
   constexpr Size operator"" _TiB(unsigned long long num) noexcept {
      return {(1LL << 40) * num};
   }
   constexpr Size operator"" _PiB(unsigned long long num) noexcept {
      return {(1LL << 50) * num};
   }
   constexpr Size operator"" _KB(unsigned long long num) noexcept {
      return {1'000LL * num};
   }
   constexpr Size operator"" _MB(unsigned long long num) noexcept {
      return {1'000'000LL * num};
   }
   constexpr Size operator"" _GB(unsigned long long num) noexcept {
      return {1'000'000'000LL * num};
   }
   constexpr Size operator"" _TB(unsigned long long num) noexcept {
      return {1'000'000'000'000LL * num};
   }
   constexpr Size operator"" _PB(unsigned long long num) noexcept {
      return {1'000'000'000'000'000LL * num};
   }

} // namespace Langulus::Logger

#if LANGULUS_FEATURE(LOGGING)

///                                                                           
/// Extend FMT to be capable of logging Logger::Color                         
///                                                                           
template<>
struct ::fmt::formatter<::Langulus::Logger::Color> {
   using Color = ::Langulus::Logger::Color;

   template<class CONTEXT>
   constexpr auto parse(CONTEXT& ctx) {
      return ctx.begin();
   }

   template<class CONTEXT> LANGULUS(INLINED)
   auto format(Color const& c, CONTEXT& ctx) const {
      text_style style = {};

      if (c == Color::NoForeground or c == Color::NoBackground) {
         return ctx.out();
      }
      else if ((c >= Color::Black    and c < Color::BlackBgr)
            or (c >= Color::DarkGray and c < Color::DarkGrayBgr)) {
         // Create a new foreground color style                         
         style = fg(static_cast<terminal_color>(c));
         const auto ansi = detail::make_foreground_color<char>(style.get_foreground());
         return format_to(ctx.out(), "{}", static_cast<const char*>(ansi));
      }
      else {
         // Create a new background color style                         
         style = bg(static_cast<terminal_color>(static_cast<uint8_t>(c) - 10));
         const auto ansi = detail::make_background_color<char>(style.get_background());
         return format_to(ctx.out(), "{}", static_cast<const char*>(ansi));
      }
   }
};

///                                                                           
/// Extend FMT to be capable of logging Logger::Size                          
///                                                                           
template<>
struct ::fmt::formatter<::Langulus::Logger::Size> {
   template<class CONTEXT>
   constexpr auto parse(CONTEXT& ctx) {
      return ctx.begin();
   }

   template<class CONTEXT> LANGULUS(INLINED)
   auto format(::Langulus::Logger::Size const& bs, CONTEXT& ctx) const {
      return format_to(ctx.out(), "{}", bs.format());
   }
};

///                                                                           
/// Extend FMT to be capable of logging std::array of characters              
///                                                                           
template<size_t N>
struct ::fmt::formatter<::std::array<char, N>> {
   template<class CONTEXT>
   constexpr auto parse(CONTEXT& ctx) {
      return ctx.begin();
   }

   template<class CONTEXT> LANGULUS(INLINED)
   auto format(::std::array<char, N> const& a, CONTEXT& ctx) const {
      return format_to(ctx.out(), "{}", ::std::string_view(a.data(), a.size()));
   }
};

#endif
