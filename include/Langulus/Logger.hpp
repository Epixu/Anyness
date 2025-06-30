///                                                                           
/// Langulus::Logger                                                          
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "LoggerStateless.hpp"
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
   using enum CommandExt;
   
   /// Colors combined with pushes and pop commands, for shorter code         
   enum class ColorExt : unsigned {
      // Bits that dictate how to mix the colors                        
      // Color is always mixed with the currently set one, unless the   
      // 'PreviousColor' bit is on, in which case the style is popped   
      // before applying color                                          
      PreviousColor           = 128,

      // Color is always mixed with the currently set one, unless the   
      // 'NextColor' bit is on, in which case the style is pushed       
      // before applying color                                          
      NextColor               = 256,

      // Colors that mix with the previous color                        
      PopNoForeground         = static_cast<unsigned>(NoForeground ) | PreviousColor,
      PopNoBackground         = static_cast<unsigned>(NoBackground ) | PreviousColor,
       
      PopBlack                = static_cast<unsigned>(Black        ) | PreviousColor,
      PopDarkRed              = static_cast<unsigned>(DarkRed      ) | PreviousColor,
      PopDarkGreen            = static_cast<unsigned>(DarkGreen    ) | PreviousColor,
      PopDarkYellow           = static_cast<unsigned>(DarkYellow   ) | PreviousColor,
      PopDarkBlue             = static_cast<unsigned>(DarkBlue     ) | PreviousColor,
      PopDarkPurple           = static_cast<unsigned>(DarkPurple   ) | PreviousColor,
      PopDarkCyan             = static_cast<unsigned>(DarkCyan     ) | PreviousColor,
      PopGray                 = static_cast<unsigned>(Gray         ) | PreviousColor,
      
      PopBlackBgr             = static_cast<unsigned>(BlackBgr     ) | PreviousColor,
      PopDarkRedBgr           = static_cast<unsigned>(DarkRedBgr   ) | PreviousColor,
      PopDarkGreenBgr         = static_cast<unsigned>(DarkGreenBgr ) | PreviousColor,
      PopDarkYellowBgr        = static_cast<unsigned>(DarkYellowBgr) | PreviousColor,
      PopDarkBlueBgr          = static_cast<unsigned>(DarkBlueBgr  ) | PreviousColor,
      PopDarkPurpleBgr        = static_cast<unsigned>(DarkPurpleBgr) | PreviousColor,
      PopDarkCyanBgr          = static_cast<unsigned>(DarkCyanBgr  ) | PreviousColor,
      PopGrayBgr              = static_cast<unsigned>(GrayBgr      ) | PreviousColor,
      
      PopDarkGray             = static_cast<unsigned>(DarkGray     ) | PreviousColor,
      PopRed                  = static_cast<unsigned>(Red          ) | PreviousColor,
      PopGreen                = static_cast<unsigned>(Green        ) | PreviousColor,
      PopYellow               = static_cast<unsigned>(Yellow       ) | PreviousColor,
      PopBlue                 = static_cast<unsigned>(Blue         ) | PreviousColor,
      PopPurple               = static_cast<unsigned>(Purple       ) | PreviousColor,
      PopCyan                 = static_cast<unsigned>(Cyan         ) | PreviousColor,
      PopWhite                = static_cast<unsigned>(White        ) | PreviousColor,
      
      PopDarkGrayBgr          = static_cast<unsigned>(DarkGrayBgr  ) | PreviousColor,
      PopRedBgr               = static_cast<unsigned>(RedBgr       ) | PreviousColor,
      PopGreenBgr             = static_cast<unsigned>(GreenBgr     ) | PreviousColor,
      PopYellowBgr            = static_cast<unsigned>(YellowBgr    ) | PreviousColor,
      PopBlueBgr              = static_cast<unsigned>(BlueBgr      ) | PreviousColor,
      PopPurpleBgr            = static_cast<unsigned>(PurpleBgr    ) | PreviousColor,
      PopCyanBgr              = static_cast<unsigned>(CyanBgr      ) | PreviousColor,
      PopWhiteBgr             = static_cast<unsigned>(WhiteBgr     ) | PreviousColor,

      // Colors that mix with the next color                            
      PushNoForeground        = static_cast<unsigned>(NoForeground ) | NextColor,
      PushNoBackground        = static_cast<unsigned>(NoBackground ) | NextColor,
      
      PushBlack               = static_cast<unsigned>(Black        ) | NextColor,
      PushDarkRed             = static_cast<unsigned>(DarkRed      ) | NextColor,
      PushDarkGreen           = static_cast<unsigned>(DarkGreen    ) | NextColor,
      PushDarkYellow          = static_cast<unsigned>(DarkYellow   ) | NextColor,
      PushDarkBlue            = static_cast<unsigned>(DarkBlue     ) | NextColor,
      PushDarkPurple          = static_cast<unsigned>(DarkPurple   ) | NextColor,
      PushDarkCyan            = static_cast<unsigned>(DarkCyan     ) | NextColor,
      PushGray                = static_cast<unsigned>(Gray         ) | NextColor,
      
      PushBlackBgr            = static_cast<unsigned>(BlackBgr     ) | NextColor,
      PushDarkRedBgr          = static_cast<unsigned>(DarkRedBgr   ) | NextColor,
      PushDarkGreenBgr        = static_cast<unsigned>(DarkGreenBgr ) | NextColor,
      PushDarkYellowBgr       = static_cast<unsigned>(DarkYellowBgr) | NextColor,
      PushDarkBlueBgr         = static_cast<unsigned>(DarkBlueBgr  ) | NextColor,
      PushDarkPurpleBgr       = static_cast<unsigned>(DarkPurpleBgr) | NextColor,
      PushDarkCyanBgr         = static_cast<unsigned>(DarkCyanBgr  ) | NextColor,
      PushGrayBgr             = static_cast<unsigned>(GrayBgr      ) | NextColor,
      
      PushDarkGray            = static_cast<unsigned>(DarkGray     ) | NextColor,
      PushRed                 = static_cast<unsigned>(Red          ) | NextColor,
      PushGreen               = static_cast<unsigned>(Green        ) | NextColor,
      PushYellow              = static_cast<unsigned>(Yellow       ) | NextColor,
      PushBlue                = static_cast<unsigned>(Blue         ) | NextColor,
      PushPurple              = static_cast<unsigned>(Purple       ) | NextColor,
      PushCyan                = static_cast<unsigned>(Cyan         ) | NextColor,
      PushWhite               = static_cast<unsigned>(White        ) | NextColor,
      
      PushDarkGrayBgr         = static_cast<unsigned>(DarkGrayBgr  ) | NextColor,
      PushRedBgr              = static_cast<unsigned>(RedBgr       ) | NextColor,
      PushGreenBgr            = static_cast<unsigned>(GreenBgr     ) | NextColor,
      PushYellowBgr           = static_cast<unsigned>(YellowBgr    ) | NextColor,
      PushBlueBgr             = static_cast<unsigned>(BlueBgr      ) | NextColor,
      PushPurpleBgr           = static_cast<unsigned>(PurpleBgr    ) | NextColor,
      PushCyanBgr             = static_cast<unsigned>(CyanBgr      ) | NextColor,
      PushWhiteBgr            = static_cast<unsigned>(WhiteBgr     ) | NextColor,

      // Colors that reset to previous color, push and mix              
      PopAndPushNoForeground  = static_cast<unsigned>(NoForeground ) | NextColor | PreviousColor,
      PopAndPushNoBackground  = static_cast<unsigned>(NoBackground ) | NextColor | PreviousColor,

      PopAndPushBlack         = static_cast<unsigned>(Black        ) | NextColor | PreviousColor,
      PopAndPushDarkRed       = static_cast<unsigned>(DarkRed      ) | NextColor | PreviousColor,
      PopAndPushDarkGreen     = static_cast<unsigned>(DarkGreen    ) | NextColor | PreviousColor,
      PopAndPushDarkYellow    = static_cast<unsigned>(DarkYellow   ) | NextColor | PreviousColor,
      PopAndPushDarkBlue      = static_cast<unsigned>(DarkBlue     ) | NextColor | PreviousColor,
      PopAndPushDarkPurple    = static_cast<unsigned>(DarkPurple   ) | NextColor | PreviousColor,
      PopAndPushDarkCyan      = static_cast<unsigned>(DarkCyan     ) | NextColor | PreviousColor,
      PopAndPushGray          = static_cast<unsigned>(Gray         ) | NextColor | PreviousColor,

      PopAndPushBlackBgr      = static_cast<unsigned>(BlackBgr     ) | NextColor | PreviousColor,
      PopAndPushDarkRedBgr    = static_cast<unsigned>(DarkRedBgr   ) | NextColor | PreviousColor,
      PopAndPushDarkGreenBgr  = static_cast<unsigned>(DarkGreenBgr ) | NextColor | PreviousColor,
      PopAndPushDarkYellowBgr = static_cast<unsigned>(DarkYellowBgr) | NextColor | PreviousColor,
      PopAndPushDarkBlueBgr   = static_cast<unsigned>(DarkBlueBgr  ) | NextColor | PreviousColor,
      PopAndPushDarkPurpleBgr = static_cast<unsigned>(DarkPurpleBgr) | NextColor | PreviousColor,
      PopAndPushDarkCyanBgr   = static_cast<unsigned>(DarkCyanBgr  ) | NextColor | PreviousColor,
      PopAndPushGrayBgr       = static_cast<unsigned>(GrayBgr      ) | NextColor | PreviousColor,

      PopAndPushDarkGray      = static_cast<unsigned>(DarkGray     ) | NextColor | PreviousColor,
      PopAndPushRed           = static_cast<unsigned>(Red          ) | NextColor | PreviousColor,
      PopAndPushGreen         = static_cast<unsigned>(Green        ) | NextColor | PreviousColor,
      PopAndPushYellow        = static_cast<unsigned>(Yellow       ) | NextColor | PreviousColor,
      PopAndPushBlue          = static_cast<unsigned>(Blue         ) | NextColor | PreviousColor,
      PopAndPushPurple        = static_cast<unsigned>(Purple       ) | NextColor | PreviousColor,
      PopAndPushCyan          = static_cast<unsigned>(Cyan         ) | NextColor | PreviousColor,
      PopAndPushWhite         = static_cast<unsigned>(White        ) | NextColor | PreviousColor,

      PopAndPushDarkGrayBgr   = static_cast<unsigned>(DarkGrayBgr  ) | NextColor | PreviousColor,
      PopAndPushRedBgr        = static_cast<unsigned>(RedBgr       ) | NextColor | PreviousColor,
      PopAndPushGreenBgr      = static_cast<unsigned>(GreenBgr     ) | NextColor | PreviousColor,
      PopAndPushYellowBgr     = static_cast<unsigned>(YellowBgr    ) | NextColor | PreviousColor,
      PopAndPushBlueBgr       = static_cast<unsigned>(BlueBgr      ) | NextColor | PreviousColor,
      PopAndPushPurpleBgr     = static_cast<unsigned>(PurpleBgr    ) | NextColor | PreviousColor,
      PopAndPushCyanBgr       = static_cast<unsigned>(CyanBgr      ) | NextColor | PreviousColor,
      PopAndPushWhiteBgr      = static_cast<unsigned>(WhiteBgr     ) | NextColor | PreviousColor
   };
   using enum ColorExt;
   
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
      IntentProperties mIntentStyle[static_cast<int>(Intent::Counter)];

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
      LANGULUS_API(LOGGER) void Clear() const noexcept;

      /// Additional services                                                 
      LANGULUS_API(LOGGER) void Write(const CT::Loggable auto&) const noexcept;
      LANGULUS_API(LOGGER) void Write(CommandExt) const noexcept;
      LANGULUS_API(LOGGER) void Write(ColorExt) const noexcept;
      LANGULUS_API(LOGGER) auto NewTab() const noexcept -> Scope;

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
      #if LANGULUS_FEATURE(LOGGING)
         if constexpr (TOGGLE) {
            if not consteval {
               GlobalState.NewLine();
               (GlobalState.Write(FWD(arguments)), ...);
            }
         }
      #else
         LANGULUS(NOOP);
      #endif
   }

   /// A general same-line write function that continues the last style/intent
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void Append(T&&...arguments) noexcept {
      #if LANGULUS_FEATURE(LOGGING)
         if constexpr (TOGGLE) {
            if not consteval {
               (GlobalState.Write(FWD(arguments)), ...);
            }
         }
      #else
         LANGULUS(NOOP);
      #endif
   }

   /// Write a section on a new line, tab all consecutive lines, bold it,     
   /// and return the scoped tabs, that will be	untabbed automatically at the 
   /// scope's end. Section color is context dependent on the current style   
   ///   @return a scoped tab, that will untab when destroyed                 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr auto Section(T&&...arguments) noexcept {
      #if LANGULUS_FEATURE(LOGGING)
         if constexpr (TOGGLE and sizeof...(arguments) > 0) {
            if not consteval {
               const auto currentStyle = GlobalState.GetCurrentStyle();
               GlobalState.NewLine();
               GlobalState.Write(GlobalState.mDefaultStyle);
               GlobalState.Write(" ");
               GlobalState.Write(currentStyle);
               GlobalState.SetEmphasis(Underline);
               (GlobalState.Write(FWD(arguments)), ...);
               GlobalState.Write(GlobalState.mDefaultStyle);
               return GlobalState.NewTab();
            }
            else {
               return Scope {0};
            }
         }
         else return UnusedScope {};
      #else
         return UnusedScope {};
      #endif
   }

   /// Write a new-line fatal error                                           
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void Fatal([[maybe_unused]] T&&...arguments) noexcept {
      #if not LANGULUS_FEATURE(LOGGING)
         LANGULUS(NOOP);
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               #ifdef LANGULUS_LOGGER_DISABLE_FATALERRORS
                  GlobalState.SetIntent(Intent::Ignore);
               #else
                  GlobalState.SetIntent(Intent::FatalError);
                  GlobalState.NewLine();
                  (GlobalState.Write(FWD(arguments)), ...);
               #endif
            }
         }
      #endif
   }

   /// Write a new-line fatal error and tab all next lines                    
   ///   @return a scoped tab, that will untab when destroyed                 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr auto FatalScoped([[maybe_unused]] T&&...arguments) noexcept {
      #if not LANGULUS_FEATURE(LOGGING)
         return UnusedScope {};
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               #ifdef LANGULUS_LOGGER_DISABLE_FATALERRORS
                  GlobalState.SetIntent(Intent::Ignore);
                  return UnusedScope {};
               #else
                  GlobalState.SetIntent(Intent::FatalError);
                  GlobalState.NewLine();
                  (GlobalState.Write(FWD(arguments)), ...);
                  return GlobalState.NewTab();
               #endif
            }
            else {
               return UnusedScope {};
            }
         }
         else return UnusedScope {};
      #endif
   }

   /// Write a new-line error                                                 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void Error([[maybe_unused]] T&&...arguments) noexcept {
      #if not LANGULUS_FEATURE(LOGGING)
         LANGULUS(NOOP);
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               #ifdef LANGULUS_LOGGER_DISABLE_ERRORS
                  GlobalState.SetIntent(Intent::Ignore);
               #else
                  GlobalState.SetIntent(Intent::Error);
                  GlobalState.NewLine();
                  (GlobalState.Write(FWD(arguments)), ...);
               #endif
            }
         }
      #endif
   }

   /// Write a new-line error and tab all next lines                          
   ///   @return a scoped tab, that will untab when destroyed                 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr auto ErrorScoped([[maybe_unused]] T&&...arguments) noexcept {
      #if not LANGULUS_FEATURE(LOGGING)
         return UnusedScope {};
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               #ifdef LANGULUS_LOGGER_DISABLE_ERRORS
                  GlobalState.SetIntent(Intent::Ignore);
                  return UnusedScope {};
               #else
                  GlobalState.SetIntent(Intent::Error);
                  GlobalState.NewLine();
                  (GlobalState.Write(FWD(arguments)), ...);
                  return GlobalState.NewTab();
               #endif
            }
            else {
               return Scope {0};
            }
         }
         else return UnusedScope {};
      #endif
   }

   /// Write a new-line warning                                               
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void Warning([[maybe_unused]] T&&...arguments) noexcept {
      #if not LANGULUS_FEATURE(LOGGING)
         LANGULUS(NOOP);
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               #ifdef LANGULUS_LOGGER_DISABLE_WARNINGS
                  GlobalState.SetIntent(Intent::Ignore);
               #else
                  GlobalState.SetIntent(Intent::Warning);
                  GlobalState.NewLine();
                  (GlobalState.Write(FWD(arguments)), ...);
               #endif
            }
         }
      #endif
   }

   /// Write a new-line warning and tab all next lines                        
   ///   @return a scoped tab, that will untab when destroyed                 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr auto WarningScoped([[maybe_unused]] T&&...arguments) noexcept {
      #if not LANGULUS_FEATURE(LOGGING)
         return UnusedScope {};
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               #ifdef LANGULUS_LOGGER_DISABLE_WARNINGS
                  GlobalState.SetIntent(Intent::Ignore);
                  return UnusedScope {};
               #else
                  GlobalState.SetIntent(Intent::Warning);
                  GlobalState.NewLine();
                  (GlobalState.Write(FWD(arguments)), ...);
                  return GlobalState.NewTab();
               #endif
            }
            else {
               return UnusedScope {};
            }
         }
         else return UnusedScope {};
      #endif
   }

   /// Write a new-line with verbose information                              
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void Verbose([[maybe_unused]] T&&...arguments) noexcept {
      #if not LANGULUS_FEATURE(LOGGING)
         LANGULUS(NOOP);
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               #ifdef LANGULUS_LOGGER_DISABLE_VERBOSE
                  GlobalState.SetIntent(Intent::Ignore);
               #else
                  GlobalState.SetIntent(Intent::Verbose);
                  GlobalState.NewLine();
                  (GlobalState.Write(FWD(arguments)), ...);
               #endif
            }
         }
      #endif
   }

   /// Write a new-line verbose and tab all next lines                        
   ///   @return a scoped tab, that will untab when destroyed                 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr auto VerboseScoped([[maybe_unused]] T&&...arguments) noexcept {
      #if not LANGULUS_FEATURE(LOGGING)
         return UnusedScope {};
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               #ifdef LANGULUS_LOGGER_DISABLE_VERBOSE
                  GlobalState.SetIntent(Intent::Ignore);
                  return UnusedScope {};
               #else
                  GlobalState.SetIntent(Intent::Verbose);
                  GlobalState.NewLine();
                  (GlobalState.Write(FWD(arguments)), ...);
                  return GlobalState.NewTab();
               #endif
            }
            else {
               return UnusedScope {};
            }
         }
         else return UnusedScope {};
      #endif
   }

   /// Write a new-line with information                                      
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void Info([[maybe_unused]] T&&...arguments) noexcept {
      #if not LANGULUS_FEATURE(LOGGING)
         LANGULUS(NOOP);
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               #ifdef LANGULUS_LOGGER_DISABLE_INFOS
                  GlobalState.SetIntent(Intent::Ignore);
               #else
                  GlobalState.SetIntent(Intent::Info);
                  GlobalState.NewLine();
                  (GlobalState.Write(FWD(arguments)), ...);
               #endif
            }
         }
      #endif
   }

   /// Write a new-line info and tab all next lines                           
   ///   @return a scoped tab, that will untab when destroyed                 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr auto InfoScoped([[maybe_unused]] T&&...arguments) noexcept {
      #if not LANGULUS_FEATURE(LOGGING)
         return UnusedScope {};
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               #ifdef LANGULUS_LOGGER_DISABLE_INFOS
                  GlobalState.SetIntent(Intent::Ignore);
                  return UnusedScope {};
               #else
                  GlobalState.SetIntent(Intent::Info);
                  GlobalState.NewLine();
                  (GlobalState.Write(FWD(arguments)), ...);
                  return GlobalState.NewTab();
               #endif
            }
            else {
               return Scope {0};
            }
         }
         else return UnusedScope {};
      #endif
   }

   /// Write a new-line with a personal message                               
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void Message([[maybe_unused]] T&&...arguments) noexcept {
      #if not LANGULUS_FEATURE(LOGGING)
         LANGULUS(NOOP);
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               #ifdef LANGULUS_LOGGER_DISABLE_MESSAGES
                  GlobalState.SetIntent(Intent::Ignore);
               #else
                  GlobalState.SetIntent(Intent::Message);
                  GlobalState.NewLine();
                  (GlobalState.Write(FWD(arguments)), ...);
               #endif
            }
         }
      #endif
   }

   /// Write a new-line message and tab all next lines                        
   ///   @return a scoped tab, that will untab when destroyed                 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr auto MessageScoped([[maybe_unused]] T&&...arguments) noexcept {
      #if not LANGULUS_FEATURE(LOGGING)
         return UnusedScope {};
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               #ifdef LANGULUS_LOGGER_DISABLE_MESSAGES
                  GlobalState.SetIntent(Intent::Ignore);
                  return UnusedScope {};
               #else
                  GlobalState.SetIntent(Intent::Message);
                  GlobalState.NewLine();
                  (GlobalState.Write(FWD(arguments)), ...);
                  return GlobalState.NewTab();
               #endif
            }
            else {
               return UnusedScope {};
            }
         }
         else return UnusedScope {};
      #endif
   }

   /// Write a new-line with special text                                     
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void Special([[maybe_unused]] T&&...arguments) noexcept {
      #if not LANGULUS_FEATURE(LOGGING)
         LANGULUS(NOOP);
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               #ifdef LANGULUS_LOGGER_DISABLE_SPECIALS
                  GlobalState.SetIntent(Intent::Ignore);
               #else
                  GlobalState.SetIntent(Intent::Special);
                  GlobalState.NewLine();
                  (GlobalState.Write(FWD(arguments)), ...);
               #endif
            }
         }
      #endif
   }

   /// Write a new-line special and tab all next lines                        
   ///   @return a scoped tab, that will untab when destroyed                 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr auto SpecialScoped([[maybe_unused]] T&&...arguments) noexcept {
      #if not LANGULUS_FEATURE(LOGGING)
         return UnusedScope {};
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               #ifdef LANGULUS_LOGGER_DISABLE_SPECIALS
                  GlobalState.SetIntent(Intent::Ignore);
                  return UnusedScope {};
               #else
                  GlobalState.SetIntent(Intent::Special);
                  GlobalState.NewLine();
                  (GlobalState.Write(FWD(arguments)), ...);
                  return GlobalState.NewTab();
               #endif
            }
            else {
               return UnusedScope {};
            }
         }
         else return UnusedScope {};
      #endif
   }

   /// Write a new-line with flow information                                 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void Flow([[maybe_unused]] T&&...arguments) noexcept {
      #if not LANGULUS_FEATURE(LOGGING)
         LANGULUS(NOOP);
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               #ifdef LANGULUS_LOGGER_DISABLE_FLOWS
                  GlobalState.SetIntent(Intent::Ignore);
               #else
                  GlobalState.SetIntent(Intent::Flow);
                  GlobalState.NewLine();
                  (GlobalState.Write(FWD(arguments)), ...);
               #endif
            }
         }
      #endif
   }

   /// Write a new-line flow and tab all next lines                           
   ///   @return a scoped tab, that will untab when destroyed                 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr auto FlowScoped([[maybe_unused]] T&&...arguments) noexcept {
      #if not LANGULUS_FEATURE(LOGGING)
         return UnusedScope {};
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               #ifdef LANGULUS_LOGGER_DISABLE_FLOWS
                  GlobalState.SetIntent(Intent::Ignore);
                  return UnusedScope {};
               #else
                  GlobalState.SetIntent(Intent::Flow);
                  GlobalState.NewLine();
                  (GlobalState.Write(FWD(arguments)), ...);
                  return GlobalState.NewTab();
               #endif
            }
            else {
               return UnusedScope {};
            }
         }
         else return UnusedScope {};
      #endif
   }

   /// Write a new-line on user input                                         
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void Input([[maybe_unused]] T&&...arguments) noexcept {
      #if not LANGULUS_FEATURE(LOGGING)
         LANGULUS(NOOP);
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               #ifdef LANGULUS_LOGGER_DISABLE_INPUTS
                  GlobalState.SetIntent(Intent::Ignore);
               #else
                  GlobalState.SetIntent(Intent::Input);
                  GlobalState.NewLine();
                  (GlobalState.Write(FWD(arguments)), ...);
               #endif
            }
         }
      #endif
   }

   /// Write a new-line input and tab all next lines                          
   ///   @return a scoped tab, that will untab when destroyed                 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr auto InputScoped([[maybe_unused]] T&&...arguments) noexcept {
      #if not LANGULUS_FEATURE(LOGGING)
         return UnusedScope {};
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               #ifdef LANGULUS_LOGGER_DISABLE_INPUTS
                  GlobalState.SetIntent(Intent::Ignore);
                  return UnusedScope {};
               #else
                  GlobalState.SetIntent(Intent::Input);
                  GlobalState.NewLine();
                  (GlobalState.Write(FWD(arguments)), ...);
                  return GlobalState.NewTab();
               #endif
            }
            else {
               return UnusedScope {};
            }
         }
         else return UnusedScope {};
      #endif
   }

   /// Write a new-line with network message                                  
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void Network([[maybe_unused]] T&&...arguments) noexcept {
      #if not LANGULUS_FEATURE(LOGGING)
         LANGULUS(NOOP);
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               #ifdef LANGULUS_LOGGER_DISABLE_NETWORKS
                  GlobalState.SetIntent(Intent::Ignore);
               #else
                  GlobalState.SetIntent(Intent::Network);
                  GlobalState.NewLine();
                  (GlobalState.Write(FWD(arguments)), ...);
               #endif
            }
         }
      #endif
   }

   /// Write a new-line network and tab all next lines                        
   ///   @return a scoped tab, that will untab when destroyed                 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr auto NetworkScoped([[maybe_unused]] T&&...arguments) noexcept {
      #if not LANGULUS_FEATURE(LOGGING)
         return UnusedScope {};
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               #ifdef LANGULUS_LOGGER_DISABLE_NETWORKS
                  GlobalState.SetIntent(Intent::Ignore);
                  return UnusedScope {};
               #else
                  GlobalState.SetIntent(Intent::Network);
                  GlobalState.NewLine();
                  (GlobalState.Write(FWD(arguments)), ...);
                  return GlobalState.NewTab();
               #endif
            }
            else {
               return UnusedScope {};
            }
         }
         else return UnusedScope {};
      #endif
   }

   /// Write a new-line with a message from OS                                
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void OS([[maybe_unused]] T&&...arguments) noexcept {
      #if not LANGULUS_FEATURE(LOGGING)
         LANGULUS(NOOP);
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               #ifdef LANGULUS_LOGGER_DISABLE_OS
                  GlobalState.SetIntent(Intent::Ignore);
               #else
                  GlobalState.SetIntent(Intent::OS);
                  GlobalState.NewLine();
                  (GlobalState.Write(FWD(arguments)), ...);
               #endif
            }
         }
      #endif
   }

   /// Write a new-line OS and tab all next lines                             
   ///   @return a scoped tab, that will untab when destroyed                 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr auto OSScoped([[maybe_unused]] T&&...arguments) noexcept {
      #if not LANGULUS_FEATURE(LOGGING)
         return UnusedScope {};
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               #ifdef LANGULUS_LOGGER_DISABLE_OS
                  GlobalState.SetIntent(Intent::Ignore);
                  return UnusedScope {};
               #else
                  GlobalState.SetIntent(Intent::OS);
                  GlobalState.NewLine();
                  (GlobalState.Write(FWD(arguments)), ...);
                  return GlobalState.NewTab();
               #endif
            }
            else {
               return UnusedScope {};
            }
         }
         else return UnusedScope {};
      #endif
   }

   /// Write a new-line with an input prompt                                  
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr void Prompt([[maybe_unused]] T&&...arguments) noexcept {
      #if not LANGULUS_FEATURE(LOGGING)
         LANGULUS(NOOP);
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               #ifdef LANGULUS_LOGGER_DISABLE_PROMPTS
                  GlobalState.SetIntent(Intent::Ignore);
               #else
                  GlobalState.SetIntent(Intent::Prompt);
                  GlobalState.NewLine();
                  (GlobalState.Write(FWD(arguments)), ...);
               #endif
            }
         }
      #endif
   }
   
   /// Write a new-line prompt and tab all next lines                         
   ///   @return a scoped tab, that will untab when destroyed                 
   template<bool TOGGLE = true, class...T> LANGULUS(INLINED)
   constexpr auto PromptScoped([[maybe_unused]] T&&...arguments) noexcept {
      #if not LANGULUS_FEATURE(LOGGING)
         return UnusedScope {};
      #else
         if constexpr (TOGGLE) {
            if not consteval {
               #ifdef LANGULUS_LOGGER_DISABLE_PROMPTS
                  GlobalState.SetIntent(Intent::Ignore);
                  return UnusedScope {};
               #else
                  GlobalState.SetIntent(Intent::Prompt);
                  GlobalState.NewLine();
                  (GlobalState.Write(FWD(arguments)), ...);
                  return GlobalState.NewTab();
               #endif
            }
            else {
               return UnusedScope {};
            }
         }
         else return UnusedScope {};
      #endif
   }

} // namespace Langulus::Logger
