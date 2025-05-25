///                                                                           
/// Langulus::Logger                                                          
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include <Langulus/Core.hpp>
#include <Langulus/Logger.hpp>
#include <stack>
#include <list>
#include <string_view>

#if defined(LANGULUS_EXPORT_ALL) or defined(LANGULUS_EXPORT_LOGGER)
   #define LANGULUS_API_LOGGER() LANGULUS_EXPORT()
#else
   #define LANGULUS_API_LOGGER() LANGULUS_IMPORT()
#endif


namespace Langulus::Logger
{

   /// Additional commands                                                    
   enum class CommandExt : uint8_t {
      Pop,			// Pop the style, and apply previous style            
      Push,			// Push the current style                             
      PopAndPush, // Pop the style and push another, don't stylize yet  
      Tab,			// Tab once on a new line after this command          
      Untab 		// Untab once, again on a new line after this command 
   };
   
   /// Tabulation marker (can be pushed to log)                               
   struct Tabs {
      int mTabs = 0;

      constexpr Tabs() noexcept = default;
      constexpr Tabs(const Tabs&) noexcept = default;
      constexpr Tabs(Tabs&& other) noexcept
         : mTabs {other.mTabs} { other.mTabs = 0; }
      constexpr explicit Tabs(int tabs) noexcept
         : mTabs {tabs} {}
   };

   /// Scoped tabulation marker that restores tabbing when destroyed          
   struct Scope : Tabs {
      using Tabs::Tabs;
      constexpr Scope(Scope&& other) noexcept
         : Tabs {::std::forward<Tabs>(other)} {}
      LANGULUS_API(LOGGER) ~Scope() noexcept;
   };

   /// Returned from disabled scoped functions, should be optimized-out       
   struct [[maybe_unused]] UnusedScope {};


   ///                                                                        
   ///   The abstract logger interface - override this to define attachments  
   ///                                                                        
   struct Interface {
      Interface& operator = (const Interface&) = delete;
      Interface& operator = (Interface&&) = delete;

      virtual void Write(::std::string_view const&) const noexcept = 0;
      virtual void Write(Style) const noexcept = 0;
      virtual void NewLine() const noexcept = 0;
      virtual void Clear() const noexcept = 0;
   };


   ///                                                                        
   ///   The global logger state                                              
   ///                                                                        
   /// Supports a style stack, can relay messages to a list of attachments    
   ///                                                                        
   struct State final : Interface {
   private:
      // Style stack                                                    
      ::std::stack<Style> mStyleStack;
      // Number of tabulations                                          
      size_t mTabulator = 0;

      // Redirectors                                                    
      ::std::list<Interface*> mRedirectors;
      // Duplicators                                                    
      ::std::list<Interface*> mDuplicators;

   public:
      // Current intent                                                 
      Intent mCurrentIntent = DefaultIntent;

      // Intent style customization point                               
      IntentTable mIntentStyle = DefaultIntentStyle;

      // Tabulator color and formatting customization                   
      Intent mDefaultIntent = DefaultIntent;
      Style  mDefaultStyle = DefaultStyle;
      ::std::string_view mTabString = "|  ";

      size_t GetTabs() const noexcept { return mTabulator; }

      LANGULUS_API(LOGGER)  State();
      LANGULUS_API(LOGGER) ~State();

      ///                                                                     
      /// Interface override                                                  
      ///                                                                     
      LANGULUS_API(LOGGER) void Write(::std::string_view const&) const noexcept;
      LANGULUS_API(LOGGER) void Write(Style) const noexcept;
      LANGULUS_API(LOGGER) void NewLine() const noexcept;
      LANGULUS_API(LOGGER) auto NewTab() const noexcept -> Scope;
      LANGULUS_API(LOGGER) void Clear() const noexcept;

      ///                                                                     
      /// State changers                                                      
      ///                                                                     
      LANGULUS_API(LOGGER) void RunCommand(Command) noexcept;
      LANGULUS_API(LOGGER) auto GetCurrentStyle() const noexcept -> Style;
      LANGULUS_API(LOGGER) auto SetStyle(Style) noexcept -> const Style&;
      LANGULUS_API(LOGGER) auto SetColor(Color) noexcept -> const Style&;
      LANGULUS_API(LOGGER) auto SetEmphasis(Emphasis) noexcept -> const Style&;
      LANGULUS_API(LOGGER) void SetIntent(Intent) noexcept;

      ///                                                                     
      /// Attachments                                                         
      ///                                                                     
      LANGULUS_API(LOGGER) void AttachDuplicator(Interface*) noexcept;
      LANGULUS_API(LOGGER) void DettachDuplicator(Interface*) noexcept;

      LANGULUS_API(LOGGER) void AttachRedirector(Interface*) noexcept;
      LANGULUS_API(LOGGER) void DettachRedirector(Interface*) noexcept;
   };


   ///                                                                        
   /// The global logger state                                                
   ///                                                                        
   LANGULUS_API(LOGGER) extern State GlobalState;

   
   /// A general new-line write function that continues the last intent/style 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void Line(T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
         if (not ::std::is_constant_evaluated()) {
            GlobalState.NewLine();
            (GlobalState.Write(::std::forward<T>(arguments)), ...);
         }
      }
   }

   /// A general same-line write function that continues the last style/intent
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void Append(T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
         if (not ::std::is_constant_evaluated()) {
            (GlobalState.Write(::std::forward<T>(arguments)), ...);
         }
      }
   }

   /// Write a section on a new line, tab all consecutive lines, bold it,     
   /// and return the scoped tabs, that will be	untabbed automatically at the 
   /// scope's end. Section color is context dependent on the current style   
   ///   @return a scoped tab, that will untab when destroyed                 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr auto Section(T&&...arguments) noexcept {
      if constexpr (TOGGLE and sizeof...(arguments) > 0) {
         if (not ::std::is_constant_evaluated()) {
            const auto currentStyle = GlobalState.GetCurrentStyle();
            GlobalState.NewLine();
            GlobalState.Write(GlobalState.mDefaultStyle);
            GlobalState.Write(" ");
            GlobalState.Write(currentStyle);
            GlobalState.SetEmphasis(Emphasis::Underline);
            (GlobalState.Write(::std::forward<T>(arguments)), ...);
            GlobalState.Write(GlobalState.mDefaultStyle);
            return GlobalState.NewTab();
         }
         else return UnusedScope {};
      }
      else return UnusedScope {};
   }

   /// Write a new-line fatal error                                           
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void Fatal([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
         if (not ::std::is_constant_evaluated()) {
            #ifdef LANGULUS_LOGGER_DISABLE_FATALERRORS
               GlobalState.SetIntent(Intent::Ignore);
            #else
               GlobalState.SetIntent(Intent::FatalError);
               GlobalState.NewLine();
               (GlobalState.Write(::std::forward<T>(arguments)), ...);
            #endif
         }
      }
   }

   /// Write a new-line fatal error and tab all next lines                    
   ///   @return a scoped tab, that will untab when destroyed                 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr auto FatalScoped([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
         if (not ::std::is_constant_evaluated()) {
            #ifdef LANGULUS_LOGGER_DISABLE_FATALERRORS
               GlobalState.SetIntent(Intent::Ignore);
               return UnusedScope {};
            #else
               GlobalState.SetIntent(Intent::FatalError);
               GlobalState.NewLine();
               (GlobalState.Write(::std::forward<T>(arguments)), ...);
               return GlobalState.NewTab();
            #endif
         }
         else return UnusedScope {};
      }
      else return UnusedScope {};
   }

   /// Write a new-line error                                                 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void Error([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
         if (not ::std::is_constant_evaluated()) {
            #ifdef LANGULUS_LOGGER_DISABLE_ERRORS
               GlobalState.SetIntent(Intent::Ignore);
            #else
               GlobalState.SetIntent(Intent::Error);
               GlobalState.NewLine();
               (GlobalState.Write(::std::forward<T>(arguments)), ...);
            #endif
         }
      }
   }

   /// Write a new-line error and tab all next lines                          
   ///   @return a scoped tab, that will untab when destroyed                 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr auto ErrorScoped([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
         if (not ::std::is_constant_evaluated()) {
            #ifdef LANGULUS_LOGGER_DISABLE_ERRORS
               GlobalState.SetIntent(Intent::Ignore);
               return UnusedScope {};
            #else
               GlobalState.SetIntent(Intent::Error);
               GlobalState.NewLine();
               (GlobalState.Write(::std::forward<T>(arguments)), ...);
               return GlobalState.NewTab();
            #endif
         }
         else return UnusedScope {};
      }
      else return UnusedScope {};
   }

   /// Write a new-line warning                                               
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void Warning([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
         if (not ::std::is_constant_evaluated()) {
            #ifdef LANGULUS_LOGGER_DISABLE_WARNINGS
               GlobalState.SetIntent(Intent::Ignore);
            #else
               GlobalState.SetIntent(Intent::Warning);
               GlobalState.NewLine();
               (GlobalState.Write(::std::forward<T>(arguments)), ...);
            #endif
         }
      }
   }

   /// Write a new-line warning and tab all next lines                        
   ///   @return a scoped tab, that will untab when destroyed                 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr auto WarningScoped([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
         if (not ::std::is_constant_evaluated()) {
            #ifdef LANGULUS_LOGGER_DISABLE_WARNINGS
               GlobalState.SetIntent(Intent::Ignore);
               return UnusedScope {};
            #else
               GlobalState.SetIntent(Intent::Warning);
               GlobalState.NewLine();
               (GlobalState.Write(::std::forward<T>(arguments)), ...);
               return GlobalState.NewTab();
            #endif
         }
         else return UnusedScope {};
      }
      else return UnusedScope {};
   }

   /// Write a new-line with verbose information                              
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void Verbose([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
         if (not ::std::is_constant_evaluated()) {
            #ifdef LANGULUS_LOGGER_DISABLE_VERBOSE
               GlobalState.SetIntent(Intent::Ignore);
            #else
               GlobalState.SetIntent(Intent::Verbose);
               GlobalState.NewLine();
               (GlobalState.Write(::std::forward<T>(arguments)), ...);
            #endif
         }
      }
   }

   /// Write a new-line verbose and tab all next lines                        
   ///   @return a scoped tab, that will untab when destroyed                 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr auto VerboseScoped([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
         if (not ::std::is_constant_evaluated()) {
            #ifdef LANGULUS_LOGGER_DISABLE_VERBOSE
               GlobalState.SetIntent(Intent::Ignore);
               return UnusedScope {};
            #else
               GlobalState.SetIntent(Intent::Verbose);
               GlobalState.NewLine();
               (GlobalState.Write(::std::forward<T>(arguments)), ...);
               return GlobalState.NewTab();
            #endif
         }
         else return UnusedScope {};
      }
      else return UnusedScope {};
   }

   /// Write a new-line with information                                      
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void Info([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
         if (not ::std::is_constant_evaluated()) {
            #ifdef LANGULUS_LOGGER_DISABLE_INFOS
               GlobalState.SetIntent(Intent::Ignore);
            #else
               GlobalState.SetIntent(Intent::Info);
               GlobalState.NewLine();
               (GlobalState.Write(::std::forward<T>(arguments)), ...);
            #endif
         }
      }
   }

   /// Write a new-line info and tab all next lines                           
   ///   @return a scoped tab, that will untab when destroyed                 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr auto InfoScoped([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
         if (not ::std::is_constant_evaluated()) {
            #ifdef LANGULUS_LOGGER_DISABLE_INFOS
               GlobalState.SetIntent(Intent::Ignore);
               return UnusedScope {};
            #else
               GlobalState.SetIntent(Intent::Info);
               GlobalState.NewLine();
               (GlobalState.Write(::std::forward<T>(arguments)), ...);
               return GlobalState.NewTab();
            #endif
         }
         else return UnusedScope {};
      }
      else return UnusedScope {};
   }

   /// Write a new-line with a personal message                               
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void Message([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
         if (not ::std::is_constant_evaluated()) {
            #ifdef LANGULUS_LOGGER_DISABLE_MESSAGES
               GlobalState.SetIntent(Intent::Ignore);
            #else
               GlobalState.SetIntent(Intent::Message);
               GlobalState.NewLine();
               (GlobalState.Write(::std::forward<T>(arguments)), ...);
            #endif
         }
      }
   }

   /// Write a new-line message and tab all next lines                        
   ///   @return a scoped tab, that will untab when destroyed                 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr auto MessageScoped([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
         if (not ::std::is_constant_evaluated()) {
            #ifdef LANGULUS_LOGGER_DISABLE_MESSAGES
               GlobalState.SetIntent(Intent::Ignore);
               return UnusedScope {};
            #else
               GlobalState.SetIntent(Intent::Message);
               GlobalState.NewLine();
               (GlobalState.Write(::std::forward<T>(arguments)), ...);
               return GlobalState.NewTab();
            #endif
         }
         else return UnusedScope {};
      }
      else return UnusedScope {};
   }

   /// Write a new-line with special text                                     
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void Special([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
         if (not ::std::is_constant_evaluated()) {
            #ifdef LANGULUS_LOGGER_DISABLE_SPECIALS
               GlobalState.SetIntent(Intent::Ignore);
            #else
               GlobalState.SetIntent(Intent::Special);
               GlobalState.NewLine();
               (GlobalState.Write(::std::forward<T>(arguments)), ...);
            #endif
         }
      }
   }

   /// Write a new-line special and tab all next lines                        
   ///   @return a scoped tab, that will untab when destroyed                 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr auto SpecialScoped([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
         if (not ::std::is_constant_evaluated()) {
            #ifdef LANGULUS_LOGGER_DISABLE_SPECIALS
               GlobalState.SetIntent(Intent::Ignore);
               return UnusedScope {};
            #else
               GlobalState.SetIntent(Intent::Special);
               GlobalState.NewLine();
               (GlobalState.Write(::std::forward<T>(arguments)), ...);
               return GlobalState.NewTab();
            #endif
         }
         else return UnusedScope {};
      }
      else return UnusedScope {};
   }

   /// Write a new-line with flow information                                 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void Flow([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
         if (not ::std::is_constant_evaluated()) {
            #ifdef LANGULUS_LOGGER_DISABLE_FLOWS
               GlobalState.SetIntent(Intent::Ignore);
            #else
               GlobalState.SetIntent(Intent::Flow);
               GlobalState.NewLine();
               (GlobalState.Write(::std::forward<T>(arguments)), ...);
            #endif
         }
      }
   }

   /// Write a new-line flow and tab all next lines                           
   ///   @return a scoped tab, that will untab when destroyed                 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr auto FlowScoped([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
         if (not ::std::is_constant_evaluated()) {
            #ifdef LANGULUS_LOGGER_DISABLE_FLOWS
               GlobalState.SetIntent(Intent::Ignore);
               return UnusedScope {};
            #else
               GlobalState.SetIntent(Intent::Flow);
               GlobalState.NewLine();
               (GlobalState.Write(::std::forward<T>(arguments)), ...);
               return GlobalState.NewTab();
            #endif
         }
         else return UnusedScope {};
      }
      else return UnusedScope {};
   }

   /// Write a new-line on user input                                         
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void Input([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
         if (not ::std::is_constant_evaluated()) {
            #ifdef LANGULUS_LOGGER_DISABLE_INPUTS
               GlobalState.SetIntent(Intent::Ignore);
            #else
               GlobalState.SetIntent(Intent::Input);
               GlobalState.NewLine();
               (GlobalState.Write(::std::forward<T>(arguments)), ...);
            #endif
         }
      }
   }

   /// Write a new-line input and tab all next lines                          
   ///   @return a scoped tab, that will untab when destroyed                 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr auto InputScoped([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
         if (not ::std::is_constant_evaluated()) {
            #ifdef LANGULUS_LOGGER_DISABLE_INPUTS
               GlobalState.SetIntent(Intent::Ignore);
               return UnusedScope {};
            #else
               GlobalState.SetIntent(Intent::Input);
               GlobalState.NewLine();
               (GlobalState.Write(::std::forward<T>(arguments)), ...);
               return GlobalState.NewTab();
            #endif
         }
         else return UnusedScope {};
      }
      else return UnusedScope {};
   }

   /// Write a new-line with network message                                  
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void Network([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
         if (not ::std::is_constant_evaluated()) {
            #ifdef LANGULUS_LOGGER_DISABLE_NETWORKS
               GlobalState.SetIntent(Intent::Ignore);
            #else
               GlobalState.SetIntent(Intent::Network);
               GlobalState.NewLine();
               (GlobalState.Write(::std::forward<T>(arguments)), ...);
            #endif
         }
      }
   }

   /// Write a new-line network and tab all next lines                        
   ///   @return a scoped tab, that will untab when destroyed                 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr auto NetworkScoped([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
         if (not ::std::is_constant_evaluated()) {
            #ifdef LANGULUS_LOGGER_DISABLE_NETWORKS
               GlobalState.SetIntent(Intent::Ignore);
               return UnusedScope {};
            #else
               GlobalState.SetIntent(Intent::Network);
               GlobalState.NewLine();
               (GlobalState.Write(::std::forward<T>(arguments)), ...);
               return GlobalState.NewTab();
            #endif
         }
         else return UnusedScope {};
      }
      else return UnusedScope {};
   }

   /// Write a new-line with a message from OS                                
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void OS([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
         if (not ::std::is_constant_evaluated()) {
            #ifdef LANGULUS_LOGGER_DISABLE_OS
               GlobalState.SetIntent(Intent::Ignore);
            #else
               GlobalState.SetIntent(Intent::OS);
               GlobalState.NewLine();
               (GlobalState.Write(::std::forward<T>(arguments)), ...);
            #endif
         }
      }
   }

   /// Write a new-line OS and tab all next lines                             
   ///   @return a scoped tab, that will untab when destroyed                 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr auto OSScoped([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
         if (not ::std::is_constant_evaluated()) {
            #ifdef LANGULUS_LOGGER_DISABLE_OS
               GlobalState.SetIntent(Intent::Ignore);
               return UnusedScope {};
            #else
               GlobalState.SetIntent(Intent::OS);
               GlobalState.NewLine();
               (GlobalState.Write(::std::forward<T>(arguments)), ...);
               return GlobalState.NewTab();
            #endif
         }
         else return UnusedScope {};
      }
      else return UnusedScope {};
   }

   /// Write a new-line with an input prompt                                  
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void Prompt([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
         if (not ::std::is_constant_evaluated()) {
            #ifdef LANGULUS_LOGGER_DISABLE_PROMPTS
               GlobalState.SetIntent(Intent::Ignore);
            #else
               GlobalState.SetIntent(Intent::Prompt);
               GlobalState.NewLine();
               (GlobalState.Write(::std::forward<T>(arguments)), ...);
            #endif
         }
      }
   }
   
   /// Write a new-line prompt and tab all next lines                         
   ///   @return a scoped tab, that will untab when destroyed                 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr auto PromptScoped([[maybe_unused]] T&&...arguments) noexcept {
      if constexpr (TOGGLE) {
         if (not ::std::is_constant_evaluated()) {
            #ifdef LANGULUS_LOGGER_DISABLE_PROMPTS
               GlobalState.SetIntent(Intent::Ignore);
               return UnusedScope {};
            #else
               GlobalState.SetIntent(Intent::Prompt);
               GlobalState.NewLine();
               (GlobalState.Write(::std::forward<T>(arguments)), ...);
               return GlobalState.NewTab();
            #endif
         }
         else return UnusedScope {};
      }
      else return UnusedScope {};
   }

} // namespace Langulus::Logger