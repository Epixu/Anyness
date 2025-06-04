///                                                                           
/// Langulus::Logger                                                          
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include <fmt/format.h>
#include <fmt/color.h>
#include <fmt/chrono.h>


namespace Langulus::CT
{

   /// Anything formattable by fmt is also loggable                           
   /// You can extend this concept by specializing fmt::formatter yourself    
   template<class...T>
   concept Loggable = (::fmt::is_formattable<T>::value and ...);

   template<class...T>
   concept NotLoggable = ((not Loggable<T>) and ...);

} // namespace Langulus::CT


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
   enum class Emphasis : ::std::underlying_type_t<fmt::emphasis> {
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
      using T = ::std::underlying_type_t<fmt::emphasis>;
      return (static_cast<T>(lhs) & static_cast<T>(rhs)) == static_cast<T>(rhs);
   }

   /// Text style, with background color, foreground color, and emphasis      
   using Style = fmt::text_style;

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

   /// Can be used to specify each intent's style and grep patterns           
   struct IntentProperties {
      char  prefix[5];
      Style style;
      bool  silenced = false;
   };

   /// Default intent styling                                                 
   constexpr IntentProperties DefaultIntentStyle[int(Intent::Counter)] = {
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


   /// A general new-line write function that continues the last intent/style 
   ///   @tparam ...T - a sequence of elements to log (deducible)             
   ///   @return a reference to the logger for chaining                       
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void LineRaw(T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
         if (not ::std::is_constant_evaluated()) {
            Detail::FmtWrite("\n            ");
            (Detail::FmtWrite(::std::forward<T>(arguments)), ...);
         }
      }
   }

   /// A general same-line write function that continues the last style/intent
   ///   @tparam ...T - a sequence of elements to log (deducible)             
   ///   @return a reference to the logger for chaining                       
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void AppendRaw(T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
         if (not ::std::is_constant_evaluated()) {
            (Detail::FmtWrite(::std::forward<T>(arguments)), ...);
         }
      }
   }

   /// Write a new-line fatal error                                           
   ///   @tparam ...T - a sequence of elements to log (deducible)             
   ///   @return a reference to the logger for chaining                       
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void FatalRaw([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
      #ifdef LANGULUS_LOGGER_DISABLE_FATALERRORS
         LANGULUS(NOOP);
      #else
         if (not ::std::is_constant_evaluated()) {
            Detail::FmtWrite("\n");
            Detail::FmtPrintStyle(DefaultStyle);
            Detail::FmtPrintTime();
            Detail::FmtWrite(DefaultIntentStyle[int(Intent::FatalError)].prefix);
            Detail::FmtPrintStyle(DefaultIntentStyle[int(Intent::FatalError)].style);
            (Detail::FmtWrite(::std::forward<T>(arguments)), ...);
         }
      #endif
      }
   }

   /// Write a new-line error                                                 
   ///   @tparam ...T - a sequence of elements to log (deducible)             
   ///   @return a reference to the logger for chaining                       
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void ErrorRaw([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
      #ifdef LANGULUS_LOGGER_DISABLE_ERRORS
         LANGULUS(NOOP);
      #else
         if (not ::std::is_constant_evaluated()) {
            Detail::FmtWrite("\n");
            Detail::FmtPrintStyle(DefaultStyle);
            Detail::FmtPrintTime();
            Detail::FmtWrite(DefaultIntentStyle[int(Intent::Error)].prefix);
            Detail::FmtPrintStyle(DefaultIntentStyle[int(Intent::Error)].style);
            (Detail::FmtWrite(::std::forward<T>(arguments)), ...);
         }
      #endif
      }
   }

   /// Write a new-line warning                                               
   ///   @tparam ...T - a sequence of elements to log (deducible)             
   ///   @return a reference to the logger for chaining                       
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void WarningRaw([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
      #ifdef LANGULUS_LOGGER_DISABLE_WARNINGS
         LANGULUS(NOOP);
      #else
         if (not ::std::is_constant_evaluated()) {
            Detail::FmtWrite("\n");
            Detail::FmtPrintStyle(DefaultStyle);
            Detail::FmtPrintTime();
            Detail::FmtWrite(DefaultIntentStyle[int(Intent::Warning)].prefix);
            Detail::FmtPrintStyle(DefaultIntentStyle[int(Intent::Warning)].style);
            (Detail::FmtWrite(::std::forward<T>(arguments)), ...);
         }
      #endif
      }
   }

   /// Write a new-line with verbose information                              
   ///   @tparam ...T - a sequence of elements to log (deducible)             
   ///   @return a reference to the logger for chaining                       
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void VerboseRaw([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
      #ifdef LANGULUS_LOGGER_DISABLE_VERBOSE
         LANGULUS(NOOP);
      #else
         if (not ::std::is_constant_evaluated()) {
            Detail::FmtWrite("\n");
            Detail::FmtPrintStyle(DefaultStyle);
            Detail::FmtPrintTime();
            Detail::FmtWrite(DefaultIntentStyle[int(Intent::Verbose)].prefix);
            Detail::FmtPrintStyle(DefaultIntentStyle[int(Intent::Verbose)].style);
            (Detail::FmtWrite(::std::forward<T>(arguments)), ...);
         }
      #endif
      }
   }

   /// Write a new-line with information                                      
   ///   @tparam ...T - a sequence of elements to log (deducible)             
   ///   @return a reference to the logger for chaining                       
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void InfoRaw([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
      #ifdef LANGULUS_LOGGER_DISABLE_INFOS
         LANGULUS(NOOP);
      #else
         if (not ::std::is_constant_evaluated()) {
            Detail::FmtWrite("\n");
            Detail::FmtPrintStyle(DefaultStyle);
            Detail::FmtPrintTime();
            Detail::FmtWrite(DefaultIntentStyle[int(Intent::Info)].prefix);
            Detail::FmtPrintStyle(DefaultIntentStyle[int(Intent::Info)].style);
            (Detail::FmtWrite(::std::forward<T>(arguments)), ...);
         }
      #endif
      }
   }

   /// Write a new-line with a personal message                               
   ///   @tparam ...T - a sequence of elements to log (deducible)             
   ///   @return a reference to the logger for chaining                       
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void MessageRaw([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
      #ifdef LANGULUS_LOGGER_DISABLE_MESSAGES
         LANGULUS(NOOP);
      #else
         if (not ::std::is_constant_evaluated()) {
            Detail::FmtWrite("\n");
            Detail::FmtPrintStyle(DefaultStyle);
            Detail::FmtPrintTime();
            Detail::FmtWrite(DefaultIntentStyle[int(Intent::Message)].prefix);
            Detail::FmtPrintStyle(DefaultIntentStyle[int(Intent::Message)].style);
            (Detail::FmtWrite(::std::forward<T>(arguments)), ...);
         }
      #endif
      }
   }

   /// Write a new-line with special text                                     
   ///   @tparam ...T - a sequence of elements to log (deducible)             
   ///   @return a reference to the logger for chaining                       
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void SpecialRaw([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
      #ifdef LANGULUS_LOGGER_DISABLE_SPECIALS
         LANGULUS(NOOP);
      #else
         if (not ::std::is_constant_evaluated()) {
            Detail::FmtWrite("\n");
            Detail::FmtPrintStyle(DefaultStyle);
            Detail::FmtPrintTime();
            Detail::FmtWrite(DefaultIntentStyle[int(Intent::Special)].prefix);
            Detail::FmtPrintStyle(DefaultIntentStyle[int(Intent::Special)].style);
            (Detail::FmtWrite(::std::forward<T>(arguments)), ...);
         }
      #endif
      }
   }

   /// Write a new-line with flow information                                 
   ///   @tparam ...T - a sequence of elements to log (deducible)             
   ///   @return a reference to the logger for chaining                       
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void FlowRaw([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
      #ifdef LANGULUS_LOGGER_DISABLE_FLOWS
         LANGULUS(NOOP);
      #else
         if (not ::std::is_constant_evaluated()) {
            Detail::FmtWrite("\n");
            Detail::FmtPrintStyle(DefaultStyle);
            Detail::FmtPrintTime();
            Detail::FmtWrite(DefaultIntentStyle[int(Intent::Flow)].prefix);
            Detail::FmtPrintStyle(DefaultIntentStyle[int(Intent::Flow)].style);
            (Detail::FmtWrite(::std::forward<T>(arguments)), ...);
         }
      #endif
      }
   }

   /// Write a new-line on user input                                         
   ///   @tparam ...T - a sequence of elements to log (deducible)             
   ///   @return a reference to the logger for chaining                       
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void InputRaw([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
      #ifdef LANGULUS_LOGGER_DISABLE_INPUTS
         LANGULUS(NOOP);
      #else
         if (not ::std::is_constant_evaluated()) {
            Detail::FmtWrite("\n");
            Detail::FmtPrintStyle(DefaultStyle);
            Detail::FmtPrintTime();
            Detail::FmtWrite(DefaultIntentStyle[int(Intent::Input)].prefix);
            Detail::FmtPrintStyle(DefaultIntentStyle[int(Intent::Input)].style);
            (Detail::FmtWrite(::std::forward<T>(arguments)), ...);
         }
      #endif
      }
   }

   /// Write a new-line with network message                                  
   ///   @tparam ...T - a sequence of elements to log (deducible)             
   ///   @return a reference to the logger for chaining                       
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void NetworkRaw([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
      #ifdef LANGULUS_LOGGER_DISABLE_NETWORKS
         LANGULUS(NOOP);
      #else
         if (not ::std::is_constant_evaluated()) {
            Detail::FmtWrite("\n");
            Detail::FmtPrintStyle(DefaultStyle);
            Detail::FmtPrintTime();
            Detail::FmtWrite(DefaultIntentStyle[int(Intent::Network)].prefix);
            Detail::FmtPrintStyle(DefaultIntentStyle[int(Intent::Network)].style);
            (Detail::FmtWrite(::std::forward<T>(arguments)), ...);
         }
      #endif
      }
   }

   /// Write a new-line with a message from OS                                
   ///   @tparam ...T - a sequence of elements to log (deducible)             
   ///   @return a reference to the logger for chaining                       
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void OSRaw([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
      #ifdef LANGULUS_LOGGER_DISABLE_OS
         LANGULUS(NOOP);
      #else
         if (not ::std::is_constant_evaluated()) {
            Detail::FmtWrite("\n");
            Detail::FmtPrintStyle(DefaultStyle);
            Detail::FmtPrintTime();
            Detail::FmtWrite(DefaultIntentStyle[int(Intent::OS)].prefix);
            Detail::FmtPrintStyle(DefaultIntentStyle[int(Intent::OS)].style);
            (Detail::FmtWrite(::std::forward<T>(arguments)), ...);
         }
      #endif
      }
   }

   /// Write a new-line with an input prompt                                  
   ///   @tparam ...T - a sequence of elements to log (deducible)             
   ///   @return a reference to the logger for chaining                       
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void PromptRaw([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
      #ifdef LANGULUS_LOGGER_DISABLE_PROMPTS
         LANGULUS(NOOP);
      #else
         if (not ::std::is_constant_evaluated()) {
            Detail::FmtWrite("\n");
            Detail::FmtPrintStyle(DefaultStyle);
            Detail::FmtPrintTime();
            Detail::FmtWrite(DefaultIntentStyle[int(Intent::Prompt)].prefix);
            Detail::FmtPrintStyle(DefaultIntentStyle[int(Intent::Prompt)].style);
            (Detail::FmtWrite(::std::forward<T>(arguments)), ...);
         }
      #endif
      }
   }

} // namespace Langulus::Logger

namespace fmt
{
   
   ///                                                                        
   /// Extend FMT to be capable of logging Logger::Color                      
   ///                                                                        
   template<>
   struct formatter<::Langulus::Logger::Color> {
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

} // namespace fmt