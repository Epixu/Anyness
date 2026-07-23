///                                                                           
/// Langulus::Logger                                                          
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "simdutf/implementation.h"
#include <Langulus/Logger.hpp>
#include <string>
#include <simdutf.h>

#if LANGULUS_OS(WINDOWS)
   #define WIN32_LEAN_AND_MEAN
   #ifndef NOMINMAX
      #define NOMINMAX
   #endif
   #include <windows.h>
#endif

using Clock = ::std::chrono::system_clock;

namespace Langulus::Logger
{
   State GlobalState {};
}

using namespace Langulus;
using namespace Langulus::Logger;


/// Logger construction                                                       
State::State() {
   #if LANGULUS_OS(WINDOWS)
      auto stdout_handle = GetStdHandle(STD_OUTPUT_HANDLE);
      DWORD out_mode = 0;
      GetConsoleMode(stdout_handle, &out_mode);
      out_mode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
      //out_mode |= DISABLE_NEWLINE_AUTO_RETURN;
      SetConsoleMode(stdout_handle, out_mode);
   #endif

   for (int i = 0; i < static_cast<int>(Intent::Counter); ++i)
      mIntentStyle[i] = DefaultIntentStyle[i];
}

/// Logger destruction                                                        
State::~State() {}

/// Generate an exhaustive timestamp in the current system time zone          
///   @return the timestamp text as {:%F %T %Z}                               
::std::string Interface::GetAdvancedTime() noexcept {
   try {
      const auto now = Clock::to_time_t(Clock::now());
      return fmt::format("{:%F %T %Z}", fmt::localtime(now));
   }
   catch (...) { return "<advanced time error>"; }
}

/// Generate a short timestamp in the current system time zone                
///   @return the timestamp text as {:%T}                                     
::std::string Interface::GetSimpleTime() noexcept {
   try {
      const auto now = Clock::to_time_t(Clock::now());
      return fmt::format("{:%T}", fmt::localtime(now));
   }
   catch (...) { return "<simple time error>"; }
}

/// Write text                                                                
///   @attention will always flush file/console                               
///   @param stdString - the text view to write                               
void State::Write(const ::std::string_view& stdString) const noexcept {
   if (mCurrentIntent == Intent::Ignore)
      return;

   // Dispatch to redirectors                                           
   if (not mRedirectors.empty()) {
      for (auto attachment : mRedirectors)
         attachment->Write(stdString);

      // The presence of a redirector blocks console printing           
      return;
   }

   try { fmt::print("{}", stdString); }
   catch (...) {
      Logger::Append("<logger error>");
   }

   // Always flush                                                      
   fflush(stdout);

   // Dispatch to duplicators                                           
   for (auto attachment : mDuplicators)
      attachment->Write(stdString);
}

/// Write text (convert wide characters to UTF8)                              
///   @attention will always flush file/console                               
///   @param stdString - the text view to write                               
void State::Write(const ::std::wstring_view& stdString) const noexcept {
   ::std::string buffer;
   size_t conversion_result = 0;

   if constexpr (sizeof(wchar_t) == 2) {
      auto src = reinterpret_cast<char16_t const*>(stdString.data());
      buffer.resize(simdutf::utf8_length_from_utf16(src, stdString.size()));
      conversion_result = simdutf::convert_utf16_to_utf8_safe(
         src, stdString.size(),
         buffer.data(), buffer.size()
      );
   }
   else if constexpr (sizeof(wchar_t) == 4) {
      auto src = reinterpret_cast<char32_t const*>(stdString.data());
      buffer.resize(simdutf::utf8_length_from_utf32(src, stdString.size()));
      conversion_result = simdutf::convert_utf32_to_utf8(
         src, stdString.size(),
         buffer.data()
      );
   }

   if (not conversion_result)
      Write("<invalid wide string>");
   else
      Write(buffer);
}

/// Apply a style                                                             
///   @attention will never flush file/console                                
///   @param s - the style                                                    
void State::Write(Style s) const noexcept {
   if (mCurrentIntent == Intent::Ignore)
      return;

   // Dispatch to redirectors                                           
   if (not mRedirectors.empty()) {
      for (auto attachment : mRedirectors)
         attachment->Write(s);

      // The presence of a redirector blocks console printing           
      return;
   }

   Detail::FmtPrintStyle(s);

   // Dispatch to duplicators                                           
   for (auto attachment : mDuplicators)
      attachment->Write(s);
}

/// Add a new line, tabulating properly, but continuing the previous style    
///   @attention will never flush file/console                                
void State::NewLine() const noexcept {
   if (mCurrentIntent == Intent::Ignore)
      return;

   // Dispatch to redirectors                                           
   if (not mRedirectors.empty()) {
      for (auto attachment : mRedirectors)
         attachment->NewLine();

      // The presence of a redirector blocks console printing           
      return;
   }

   // Clear formatting, add new line, simple time stamp, and tabs       
   Detail::FmtPrintStyle(DefaultStyle);
   fmt::print("\n");
   fmt::print("{}{}", GetSimpleTime(), mIntentStyle[GetCurrentIntent()].prefix);

   if (mStyleStack.empty())
      mStyleStack.push(GetCurrentStyle());
   Detail::FmtPrintStyle(mStyleStack.top());

   auto tabs = mTabulator;
   while (tabs) {
      fmt::print("{}", mTabString);
      --tabs;
   }

   // Dispatch to duplicators                                           
   for (auto attachment : mDuplicators)
      attachment->NewLine();
}

/// Clear the entire log (clear the console window or file)                   
void State::Clear() const noexcept {
   // Dispatch to redirectors                                           
   if (not mRedirectors.empty()) {
      for (auto attachment : mRedirectors)
         attachment->Clear();

      // The presence of a redirector blocks console printing           
      return;
   }

   // Clear the window                                                  
   fmt::print("{}", "\x1b[2J");

   if (mStyleStack.empty())
      mStyleStack.push(GetCurrentStyle());

   Detail::FmtPrintStyle(mStyleStack.top());

   // Dispatch to duplicators                                           
   for (auto attachment : mDuplicators)
      attachment->Clear();
}

/// Returns the output filename, in this case, it's just the terminal         
auto State::GetFilename() const noexcept -> ::std::string_view {
   return "<console output>";
}

/// Execute a logger command                                                  
///   @param c - the command to execute                                       
/*void State::Write(Command c) const noexcept {
   switch (c) {
   case Command::Clear:
      Clear();
      break;
   case Command::NewLine:
      NewLine();
      break;
   case Command::Invert:
      SetEmphasis(Emphasis::Reverse);
      Write(mStyleStack.top());
      break;
   case Command::Time:
      Write(GetSimpleTime());
      break;
   case Command::ExactTime:
      Write(GetAdvancedTime());
      break;
   case Command::Stylize:
      if (mStyleStack.empty())
         mStyleStack.push(GetCurrentStyle());

      Write(mStyleStack.top());
      break;
   }
}*/

/// Execute a logger command (extended)                                       
///   @param c - the command to execute                                       
void State::Write(CommandExt c) const noexcept {
   switch (c) {
   case CommandExt::Pop:
      // Pop the style stack and write the previous style               
      if (mCurrentIntent == Intent::Ignore)
         return;

      if (not mStyleStack.empty())
         mStyleStack.pop();

      if (mStyleStack.empty())
         mStyleStack.push(GetCurrentStyle());

      Write(mStyleStack.top());
      break;
   case CommandExt::Push:
      // Duplicate the current style                                    
      if (mCurrentIntent == Intent::Ignore)
         return;

      mStyleStack.push(mStyleStack.top());
      break;
   case CommandExt::Tab:
      // Increate indentation                                           
      if (mCurrentIntent == Intent::Ignore)
         return;

      ++mTabulator;
      break;
   case CommandExt::Untab:
      // Decrease indentation                                           
      if (mCurrentIntent == Intent::Ignore)
         return;

      if (mTabulator > 0)
         --mTabulator;
      break;
   case CommandExt::Reset:
      // Reset logger state                                             
      mTabulator = 0;
      mCurrentIntent = mDefaultIntent;
      while (not mStyleStack.empty())
         mStyleStack.pop();
      break;
   case CommandExt::Stylize:
      if (mCurrentIntent == Intent::Ignore)
         return;

      if (mStyleStack.empty())
         mStyleStack.push(GetCurrentStyle());

      Write(mStyleStack.top());
      break;
   }
}

/// Change the foreground/background color by modifying the current style     
///   @param c_with_flags - the color with optional mixing flags              
///   @return the last style, with coloring applied                           
void State::Write(ColorExt c_with_flags) const noexcept {
   if (mCurrentIntent == Intent::Ignore)
      return;

   if (mStyleStack.empty())
      mStyleStack.push(GetCurrentStyle());

   if (static_cast<uint>(c_with_flags)
     & static_cast<uint>(ColorExt::PreviousColor)) {
      // We have to pop                                                 
      if (mStyleStack.size() > 1)
         mStyleStack.pop();
   }

   if (static_cast<uint>(c_with_flags)
     & static_cast<uint>(ColorExt::NextColor)) {
      // We have to push                                                
      mStyleStack.push(mStyleStack.top());
   }

   // Strip the mixing bits from the color                              
   const Color c = static_cast<Color>(
      static_cast<uint>(c_with_flags) & (~(
          static_cast<uint>(ColorExt::PreviousColor)
        | static_cast<uint>(ColorExt::NextColor)
      ))
   );

   // Mix...                                                            
   auto& style = mStyleStack.top();
   const auto oldStyle = style;
   if (c == Color::NoForeground) {
      // Reset the foreground color                                     
      style = {};
      if (oldStyle.has_background())
         style |= fmt::bg(oldStyle.get_background());
   }
   else if (c == Color::NoBackground) {
      // Reset the background color                                     
      style = {};
      if (oldStyle.has_foreground())
         style |= fmt::fg(oldStyle.get_foreground());
   }
   else if ((c >= Color::Black    and c < Color::BlackBgr) 
        or  (c >= Color::DarkGray and c < Color::DarkGrayBgr)) {
      // Create a new foreground color style                            
      style = fmt::fg(static_cast<fmt::terminal_color>(c));
      if (oldStyle.has_background())
         style |= fmt::bg(oldStyle.get_background());
   }
   else {
      // Create a new background color style                            
      style = fmt::bg(static_cast<fmt::terminal_color>(static_cast<uint8_t>(c) - 10));
      if (oldStyle.has_foreground())
         style |= fmt::fg(oldStyle.get_foreground());
   }

   if (oldStyle.has_emphasis())
      style |= oldStyle.get_emphasis();

   // Dispatch the new style                                            
   Write(style);
}

/// Push a number of tabs                                                     
///   @attention the effect will be observable on the next new line           
///   @param tabs - the tabs to push                                          
void State::Write(const Tabs& tabs) const noexcept {
   if (not tabs.mTabs)
      return;

   if (tabs.mTabs < 0) {
      if (static_cast<size_t>(tabs.mTabs) > mTabulator)
         mTabulator = 0;
      else
         mTabulator -= tabs.mTabs;
      return;
   }

   mTabulator += tabs.mTabs;
}

/// Change the emphasis by modifying the current style                        
///   @param e - the emphasis                                                 
void State::Write(Emphasis e) const noexcept {
   if (mCurrentIntent == Intent::Ignore)
      return;

   if (mStyleStack.empty())
      mStyleStack.push(GetCurrentStyle());

   auto& style = mStyleStack.top();
   style |= static_cast<fmt::emphasis>(e);

   // Dispatch the new style                                            
   Write(style);
}

/// Sets the current intent, and stylizes accordingly, unless Intent::Ignore  
///   @attention intent grep markers (like |W|) will update on next new line  
///   @param i - the intent                                                   
void State::Write(Intent i) const noexcept {
   if (i >= Intent::Counter)
      return;

   GlobalState.mCurrentIntent = i;

   if (i != Intent::Ignore) {
      if (mStyleStack.empty())
         mStyleStack.emplace(GlobalState.mIntentStyle[static_cast<int>(i)].style);
      else
         mStyleStack.top() = GlobalState.mIntentStyle[static_cast<int>(i)].style;

      // Dispatch the new style                                         
      Write(mStyleStack.top());
   }
}

/// Indents and returns a scoped variable that unindents at end of scope      
///   @return the scope                                                       
auto State::NewScope() const noexcept -> Scope {
   ++mTabulator;
   return Scope {1};
}

/// Get the current style                                                     
///   @returns either the top of the style stack, the style of the current    
///      intent, or a default style if current intent is Intent::Ignore       
Style State::GetCurrentStyle() const noexcept {
   if (mStyleStack.empty()) {
      if (GlobalState.mCurrentIntent != Intent::Ignore)
         return GlobalState.mIntentStyle[GlobalState.GetCurrentIntent()].style;
      return {};
   }
   return mStyleStack.top();
}

/// Get the current intent                                                    
int State::GetCurrentIntent() const noexcept {
   return static_cast<int>(GlobalState.mCurrentIntent);
}

/// Attach another logger, if no redirectors are attached, any logging        
/// will be duplicated to the provided interface                              
///   @attention the logger doesn't have ownership of the attachment          
///   @param duplicator - the logger to attach                                
void State::AttachDuplicator(Interface* duplicator) noexcept {
   mDuplicators.push_back(duplicator);
}

/// Dettach a duplicator                                                      
///   @attention the logger doesn't have ownership of the attachment          
///   @param duplicator - the duplicator to dettach                           
void State::DettachDuplicator(Interface* duplicator) noexcept {
   mDuplicators.remove(duplicator);
}

/// Attach another logger, that will receive any logging, but also consume    
/// it, so that it doesn't reach the console or any attached duplicators      
///   @attention the logger doesn't have ownership of the attachment          
///   @param redirector - the logger to attach                                
void State::AttachRedirector(Interface* redirector) noexcept {
   mRedirectors.push_back(redirector);
}

/// Dettach a redirector                                                      
///   @attention the logger doesn't have ownership of the attachment          
///   @param redirector - the duplicator to dettach                           
void State::DettachRedirector(Interface* redirector) noexcept {
   mRedirectors.remove(redirector);
}
