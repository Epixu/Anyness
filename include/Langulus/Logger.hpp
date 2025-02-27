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
      WhiteBgr,

      // Bits that dictate how to mix the colors                        
      // Color is always mixed with the currently set one, unless the   
      // 'PreviousColor' bit is on, in which case the style is popped   
      // before applying color                                          
      PreviousColor     = 128,

      // Color is always mixed with the currently set one, unless the   
      // 'NextColor' bit is on, in which case the style is pushed       
      // before applying color                                          
      NextColor         = 256,

      // Colors that mix with the previous color                        
      PopNoForeground   = NoForeground    | PreviousColor,
      PopNoBackground   = NoBackground    | PreviousColor,

      PopBlack          = Black           | PreviousColor,
      PopDarkRed        = DarkRed         | PreviousColor,
      PopDarkGreen      = DarkGreen       | PreviousColor,
      PopDarkYellow     = DarkYellow      | PreviousColor,
      PopDarkBlue       = DarkBlue        | PreviousColor,
      PopDarkPurple     = DarkPurple      | PreviousColor,
      PopDarkCyan       = DarkCyan        | PreviousColor,
      PopGray           = Gray            | PreviousColor,

      PopBlackBgr       = BlackBgr        | PreviousColor,
      PopDarkRedBgr     = DarkRedBgr      | PreviousColor,
      PopDarkGreenBgr   = DarkGreenBgr    | PreviousColor,
      PopDarkYellowBgr  = DarkYellowBgr   | PreviousColor,
      PopDarkBlueBgr    = DarkBlueBgr     | PreviousColor,
      PopDarkPurpleBgr  = DarkPurpleBgr   | PreviousColor,
      PopDarkCyanBgr    = DarkCyanBgr     | PreviousColor,
      PopGrayBgr        = GrayBgr         | PreviousColor,

      PopDarkGray       = DarkGray        | PreviousColor,
      PopRed            = Red             | PreviousColor,
      PopGreen          = Green           | PreviousColor,
      PopYellow         = Yellow          | PreviousColor,
      PopBlue           = Blue            | PreviousColor,
      PopPurple         = Purple          | PreviousColor,
      PopCyan           = Cyan            | PreviousColor,
      PopWhite          = White           | PreviousColor,

      PopDarkGrayBgr    = DarkGrayBgr     | PreviousColor,
      PopRedBgr         = RedBgr          | PreviousColor,
      PopGreenBgr       = GreenBgr        | PreviousColor,
      PopYellowBgr      = YellowBgr       | PreviousColor,
      PopBlueBgr        = BlueBgr         | PreviousColor,
      PopPurpleBgr      = PurpleBgr       | PreviousColor,
      PopCyanBgr        = CyanBgr         | PreviousColor,
      PopWhiteBgr       = WhiteBgr        | PreviousColor,

      // Colors that mix with the next color                            
      PushNoForeground  = NoForeground    | NextColor,
      PushNoBackground  = NoBackground    | NextColor,

      PushBlack         = Black           | NextColor,
      PushDarkRed       = DarkRed         | NextColor,
      PushDarkGreen     = DarkGreen       | NextColor,
      PushDarkYellow    = DarkYellow      | NextColor,
      PushDarkBlue      = DarkBlue        | NextColor,
      PushDarkPurple    = DarkPurple      | NextColor,
      PushDarkCyan      = DarkCyan        | NextColor,
      PushGray          = Gray            | NextColor,

      PushBlackBgr      = BlackBgr        | NextColor,
      PushDarkRedBgr    = DarkRedBgr      | NextColor,
      PushDarkGreenBgr  = DarkGreenBgr    | NextColor,
      PushDarkYellowBgr = DarkYellowBgr   | NextColor,
      PushDarkBlueBgr   = DarkBlueBgr     | NextColor,
      PushDarkPurpleBgr = DarkPurpleBgr   | NextColor,
      PushDarkCyanBgr   = DarkCyanBgr     | NextColor,
      PushGrayBgr       = GrayBgr         | NextColor,

      PushDarkGray      = DarkGray        | NextColor,
      PushRed           = Red             | NextColor,
      PushGreen         = Green           | NextColor,
      PushYellow        = Yellow          | NextColor,
      PushBlue          = Blue            | NextColor,
      PushPurple        = Purple          | NextColor,
      PushCyan          = Cyan            | NextColor,
      PushWhite         = White           | NextColor,

      PushDarkGrayBgr   = DarkGrayBgr     | NextColor,
      PushRedBgr        = RedBgr          | NextColor,
      PushGreenBgr      = GreenBgr        | NextColor,
      PushYellowBgr     = YellowBgr       | NextColor,
      PushBlueBgr       = BlueBgr         | NextColor,
      PushPurpleBgr     = PurpleBgr       | NextColor,
      PushCyanBgr       = CyanBgr         | NextColor,
      PushWhiteBgr      = WhiteBgr        | NextColor,

      // Colors that reset to previous color, push and mix              
      PopAndPushNoForeground  = NoForeground    | NextColor | PreviousColor,
      PopAndPushNoBackground  = NoBackground    | NextColor | PreviousColor,

      PopAndPushBlack         = Black           | NextColor | PreviousColor,
      PopAndPushDarkRed       = DarkRed         | NextColor | PreviousColor,
      PopAndPushDarkGreen     = DarkGreen       | NextColor | PreviousColor,
      PopAndPushDarkYellow    = DarkYellow      | NextColor | PreviousColor,
      PopAndPushDarkBlue      = DarkBlue        | NextColor | PreviousColor,
      PopAndPushDarkPurple    = DarkPurple      | NextColor | PreviousColor,
      PopAndPushDarkCyan      = DarkCyan        | NextColor | PreviousColor,
      PopAndPushGray          = Gray            | NextColor | PreviousColor,

      PopAndPushBlackBgr      = BlackBgr        | NextColor | PreviousColor,
      PopAndPushDarkRedBgr    = DarkRedBgr      | NextColor | PreviousColor,
      PopAndPushDarkGreenBgr  = DarkGreenBgr    | NextColor | PreviousColor,
      PopAndPushDarkYellowBgr = DarkYellowBgr   | NextColor | PreviousColor,
      PopAndPushDarkBlueBgr   = DarkBlueBgr     | NextColor | PreviousColor,
      PopAndPushDarkPurpleBgr = DarkPurpleBgr   | NextColor | PreviousColor,
      PopAndPushDarkCyanBgr   = DarkCyanBgr     | NextColor | PreviousColor,
      PopAndPushGrayBgr       = GrayBgr         | NextColor | PreviousColor,

      PopAndPushDarkGray      = DarkGray        | NextColor | PreviousColor,
      PopAndPushRed           = Red             | NextColor | PreviousColor,
      PopAndPushGreen         = Green           | NextColor | PreviousColor,
      PopAndPushYellow        = Yellow          | NextColor | PreviousColor,
      PopAndPushBlue          = Blue            | NextColor | PreviousColor,
      PopAndPushPurple        = Purple          | NextColor | PreviousColor,
      PopAndPushCyan          = Cyan            | NextColor | PreviousColor,
      PopAndPushWhite         = White           | NextColor | PreviousColor,

      PopAndPushDarkGrayBgr   = DarkGrayBgr     | NextColor | PreviousColor,
      PopAndPushRedBgr        = RedBgr          | NextColor | PreviousColor,
      PopAndPushGreenBgr      = GreenBgr        | NextColor | PreviousColor,
      PopAndPushYellowBgr     = YellowBgr       | NextColor | PreviousColor,
      PopAndPushBlueBgr       = BlueBgr         | NextColor | PreviousColor,
      PopAndPushPurpleBgr     = PurpleBgr       | NextColor | PreviousColor,
      PopAndPushCyanBgr       = CyanBgr         | NextColor | PreviousColor,
      PopAndPushWhiteBgr      = WhiteBgr        | NextColor | PreviousColor
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
      Pop,			// Pop the style, and apply previous style            
      Push,			// Push the current style                             
      PopAndPush, // Pop the style and push another, don't stylize yet  
      Invert,		// Inverts background and foreground colors           
      Reset,		// Reset the style                                    
      Stylize,    // Apply the last style                               
      Tab,			// Tab once on a new line after this command          
      Untab,		// Untab once, again on a new line after this command 
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

   /// Can be used to specify each intent's style and search patterns         
   struct IntentProperties {
      char  prefix[5];
      Style style;
      bool  silenced = false;
   };

   /// Default intent styling                                                 
   constexpr IntentProperties DefaultIntentStyle[int(Intent::Counter)] = {
      {"|F| ", fmt::fg(fmt::terminal_color::red)},             // FatalError  
      {"|E| ", fmt::fg(fmt::terminal_color::bright_red)},      // Error       
      {"|W| ", fmt::fg(fmt::terminal_color::yellow)},          // Warning     
      {"|V| ", fmt::fg(fmt::terminal_color::bright_black)},    // Verbose     
      {"|I| ", fmt::fg(fmt::terminal_color::white)},           // Info        
      {"|M| ", fmt::fg(fmt::terminal_color::bright_white)},    // Message     
      {"|S| ", fmt::fg(fmt::terminal_color::bright_magenta)},  // Special     
      {"|L| ", fmt::fg(fmt::terminal_color::cyan)},            // Flow        
      {"|N| ", fmt::fg(fmt::terminal_color::bright_blue)},     // Input       
      {"|T| ", fmt::fg(fmt::terminal_color::bright_yellow)},   // Network     
      {"|O| ", fmt::fg(fmt::terminal_color::blue)},            // OS          
      {"|P| ", fmt::fg(fmt::terminal_color::bright_green)},    // Prompt      
      {"| | ", fmt::fg(fmt::terminal_color::bright_green)}     // Ignore      
   };

   // Tabulator color and formatting customization                      
   constexpr Intent DefaultIntent = Intent::Info;
   constexpr Style  DefaultStyle  = fmt::fg(fmt::terminal_color::bright_black);

   template<class...T> void Line   (T&&...) noexcept;
   template<class...T> void Append (T&&...) noexcept;

   template<class...T> void Fatal  (T&&...) noexcept;
   template<class...T> void Error  (T&&...) noexcept;
   template<class...T> void Warning(T&&...) noexcept;
   template<class...T> void Verbose(T&&...) noexcept;
   template<class...T> void Info   (T&&...) noexcept;
   template<class...T> void Message(T&&...) noexcept;
   template<class...T> void Special(T&&...) noexcept;
   template<class...T> void Flow   (T&&...) noexcept;
   template<class...T> void Input  (T&&...) noexcept;
   template<class...T> void Network(T&&...) noexcept;
   template<class...T> void OS     (T&&...) noexcept;
   template<class...T> void Prompt (T&&...) noexcept;







   ///                                                                        
   /// Implementation details                                                 
   ///                                                                        
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
   }

   /// A general new-line write function that continues the last intent/style 
   ///   @tparam ...T - a sequence of elements to log (deducible)             
   ///   @return a reference to the logger for chaining                       
   template<class...T> LANGULUS(INLINED)
   void Line(T&&...arguments) noexcept {
      // Clear formatting, add new line, simple time stamp, and tabs    
      Detail::FmtWrite("\n            ");
      (Detail::FmtWrite(::std::forward<T>(arguments)), ...);
   }

   /// A general same-line write function that continues the last style/intent
   ///   @tparam ...T - a sequence of elements to log (deducible)             
   ///   @return a reference to the logger for chaining                       
   template<class...T> LANGULUS(INLINED)
   void Append(T&&...arguments) noexcept {
      (Detail::FmtWrite(::std::forward<T>(arguments)), ...);
   }

   /// Write a new-line fatal error                                           
   ///   @tparam ...T - a sequence of elements to log (deducible)             
   ///   @return a reference to the logger for chaining                       
   template<class...T> LANGULUS(INLINED)
   void Fatal([[maybe_unused]] T&&...arguments) noexcept {
      #ifdef LANGULUS_LOGGER_DISABLE_FATALERRORS
         LANGULUS(NOOP);
      #else
         fmt::print("\n");
         Detail::FmtPrintStyle(DefaultStyle);
         Detail::FmtPrintTime();
         Detail::FmtWrite(DefaultIntentStyle[int(Intent::FatalError)].prefix);
         Detail::FmtPrintStyle(DefaultIntentStyle[int(Intent::FatalError)].style);
         (Detail::FmtWrite(::std::forward<T>(arguments)), ...);
      #endif
   }

   /// Write a new-line error                                                 
   ///   @tparam ...T - a sequence of elements to log (deducible)             
   ///   @return a reference to the logger for chaining                       
   template<class...T> LANGULUS(INLINED)
   void Error([[maybe_unused]] T&&...arguments) noexcept {
      #ifdef LANGULUS_LOGGER_DISABLE_ERRORS
         LANGULUS(NOOP);
      #else
         fmt::print("\n");
         Detail::FmtPrintStyle(DefaultStyle);
         Detail::FmtPrintTime();
         Detail::FmtWrite(DefaultIntentStyle[int(Intent::Error)].prefix);
         Detail::FmtPrintStyle(DefaultIntentStyle[int(Intent::Error)].style);
         (Detail::FmtWrite(::std::forward<T>(arguments)), ...);
      #endif
   }

   /// Write a new-line warning                                               
   ///   @tparam ...T - a sequence of elements to log (deducible)             
   ///   @return a reference to the logger for chaining                       
   template<class...T> LANGULUS(INLINED)
   void Warning([[maybe_unused]] T&&...arguments) noexcept {
      #ifdef LANGULUS_LOGGER_DISABLE_WARNINGS
         LANGULUS(NOOP);
      #else
         fmt::print("\n");
         Detail::FmtPrintStyle(DefaultStyle);
         Detail::FmtPrintTime();
         Detail::FmtWrite(DefaultIntentStyle[int(Intent::Warning)].prefix);
         Detail::FmtPrintStyle(DefaultIntentStyle[int(Intent::Warning)].style);
         (Detail::FmtWrite(::std::forward<T>(arguments)), ...);
      #endif
   }

   /// Write a new-line with verbose information                              
   ///   @tparam ...T - a sequence of elements to log (deducible)             
   ///   @return a reference to the logger for chaining                       
   template<class...T> LANGULUS(INLINED)
   void Verbose([[maybe_unused]] T&&...arguments) noexcept {
      #ifdef LANGULUS_LOGGER_DISABLE_VERBOSE
         LANGULUS(NOOP);
      #else
         fmt::print("\n");
         Detail::FmtPrintStyle(DefaultStyle);
         Detail::FmtPrintTime();
         Detail::FmtWrite(DefaultIntentStyle[int(Intent::Verbose)].prefix);
         Detail::FmtPrintStyle(DefaultIntentStyle[int(Intent::Verbose)].style);
         (Detail::FmtWrite(::std::forward<T>(arguments)), ...);
      #endif
   }

   /// Write a new-line with information                                      
   ///   @tparam ...T - a sequence of elements to log (deducible)             
   ///   @return a reference to the logger for chaining                       
   template<class...T> LANGULUS(INLINED)
   void Info([[maybe_unused]] T&&...arguments) noexcept {
      #ifdef LANGULUS_LOGGER_DISABLE_INFOS
         LANGULUS(NOOP);
      #else
         fmt::print("\n");
         Detail::FmtPrintStyle(DefaultStyle);
         Detail::FmtPrintTime();
         Detail::FmtWrite(DefaultIntentStyle[int(Intent::Info)].prefix);
         Detail::FmtPrintStyle(DefaultIntentStyle[int(Intent::Info)].style);
         (Detail::FmtWrite(::std::forward<T>(arguments)), ...);
      #endif
   }

   /// Write a new-line with a personal message                               
   ///   @tparam ...T - a sequence of elements to log (deducible)             
   ///   @return a reference to the logger for chaining                       
   template<class...T> LANGULUS(INLINED)
   void Message([[maybe_unused]] T&&...arguments) noexcept {
      #ifdef LANGULUS_LOGGER_DISABLE_MESSAGES
         LANGULUS(NOOP);
      #else
         fmt::print("\n");
         Detail::FmtPrintStyle(DefaultStyle);
         Detail::FmtPrintTime();
         Detail::FmtWrite(DefaultIntentStyle[int(Intent::Message)].prefix);
         Detail::FmtPrintStyle(DefaultIntentStyle[int(Intent::Message)].style);
         (Detail::FmtWrite(::std::forward<T>(arguments)), ...);
      #endif
   }

   /// Write a new-line with special text                                     
   ///   @tparam ...T - a sequence of elements to log (deducible)             
   ///   @return a reference to the logger for chaining                       
   template<class...T> LANGULUS(INLINED)
   void Special([[maybe_unused]] T&&...arguments) noexcept {
      #ifdef LANGULUS_LOGGER_DISABLE_SPECIALS
         LANGULUS(NOOP);
      #else
         fmt::print("\n");
         Detail::FmtPrintStyle(DefaultStyle);
         Detail::FmtPrintTime();
         Detail::FmtWrite(DefaultIntentStyle[int(Intent::Special)].prefix);
         Detail::FmtPrintStyle(DefaultIntentStyle[int(Intent::Special)].style);
         (Detail::FmtWrite(::std::forward<T>(arguments)), ...);
      #endif
   }

   /// Write a new-line with flow information                                 
   ///   @tparam ...T - a sequence of elements to log (deducible)             
   ///   @return a reference to the logger for chaining                       
   template<class...T> LANGULUS(INLINED)
   void Flow([[maybe_unused]] T&&...arguments) noexcept {
      #ifdef LANGULUS_LOGGER_DISABLE_FLOWS
         LANGULUS(NOOP);
      #else
         fmt::print("\n");
         Detail::FmtPrintStyle(DefaultStyle);
         Detail::FmtPrintTime();
         Detail::FmtWrite(DefaultIntentStyle[int(Intent::Flow)].prefix);
         Detail::FmtPrintStyle(DefaultIntentStyle[int(Intent::Flow)].style);
         (Detail::FmtWrite(::std::forward<T>(arguments)), ...);
      #endif
   }

   /// Write a new-line on user input                                         
   ///   @tparam ...T - a sequence of elements to log (deducible)             
   ///   @return a reference to the logger for chaining                       
   template<class...T> LANGULUS(INLINED)
   void Input([[maybe_unused]] T&&...arguments) noexcept {
      #ifdef LANGULUS_LOGGER_DISABLE_INPUTS
         LANGULUS(NOOP);
      #else
         fmt::print("\n");
         Detail::FmtPrintStyle(DefaultStyle);
         Detail::FmtPrintTime();
         Detail::FmtWrite(DefaultIntentStyle[int(Intent::Input)].prefix);
         Detail::FmtPrintStyle(DefaultIntentStyle[int(Intent::Input)].style);
         (Detail::FmtWrite(::std::forward<T>(arguments)), ...);
      #endif
   }

   /// Write a new-line with network message                                  
   ///   @tparam ...T - a sequence of elements to log (deducible)             
   ///   @return a reference to the logger for chaining                       
   template<class...T> LANGULUS(INLINED)
   void Network([[maybe_unused]] T&&...arguments) noexcept {
      #ifdef LANGULUS_LOGGER_DISABLE_NETWORKS
         LANGULUS(NOOP);
      #else
         fmt::print("\n");
         Detail::FmtPrintStyle(DefaultStyle);
         Detail::FmtPrintTime();
         Detail::FmtWrite(DefaultIntentStyle[int(Intent::Network)].prefix);
         Detail::FmtPrintStyle(DefaultIntentStyle[int(Intent::Network)].style);
         (Detail::FmtWrite(::std::forward<T>(arguments)), ...);
      #endif
   }

   /// Write a new-line with a message from OS                                
   ///   @tparam ...T - a sequence of elements to log (deducible)             
   ///   @return a reference to the logger for chaining                       
   template<class...T> LANGULUS(INLINED)
   void OS([[maybe_unused]] T&&...arguments) noexcept {
      #ifdef LANGULUS_LOGGER_DISABLE_OS
         LANGULUS(NOOP);
      #else
         fmt::print("\n");
         Detail::FmtPrintStyle(DefaultStyle);
         Detail::FmtPrintTime();
         Detail::FmtWrite(DefaultIntentStyle[int(Intent::OS)].prefix);
         Detail::FmtPrintStyle(DefaultIntentStyle[int(Intent::OS)].style);
         (Detail::FmtWrite(::std::forward<T>(arguments)), ...);
      #endif
   }

   /// Write a new-line with an input prompt                                  
   ///   @tparam ...T - a sequence of elements to log (deducible)             
   ///   @return a reference to the logger for chaining                       
   template<class...T> LANGULUS(INLINED)
   void Prompt([[maybe_unused]] T&&...arguments) noexcept {
      #ifdef LANGULUS_LOGGER_DISABLE_PROMPTS
         LANGULUS(NOOP);
      #else
         fmt::print("\n");
         Detail::FmtPrintStyle(DefaultStyle);
         Detail::FmtPrintTime();
         Detail::FmtWrite(DefaultIntentStyle[int(Intent::Prompt)].prefix);
         Detail::FmtPrintStyle(DefaultIntentStyle[int(Intent::Prompt)].style);
         (Detail::FmtWrite(::std::forward<T>(arguments)), ...);
      #endif
   }

} // namespace Langulus::Logger