#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Implements ForEach iteration interface for containers                  
   ///                                                                        
   struct IterationForEach {
      using CTTI_Component = Yes;

   private:
      template<CT::Container C>
      using Count = typename Deref<C>::CountType;
      
   public:
      template<CT::Container C>
      auto ForEachElement(this C&&, auto&&...) -> Count<C>;
      template<CT::Container C>
      auto ForEachElementRev(this C&&, auto&&...) -> Count<C>;

      template<CT::Container C>
      auto ForEach(this C&&, auto&&...) -> Count<C> {
         return 0;
      }

      template<CT::Container C>
      auto ForEachRev(this C&&, auto&&...) -> Count<C>;

      template<bool SKIP = true, CT::Container C>
      auto ForEachDeep(this C&&, auto&&...) -> Count<C>;
      template<bool SKIP = true, CT::Container C>
      auto ForEachDeepRev(this C&&, auto&&...) -> Count<C>;
   };

} // namespace Langulus::Anyness::Component

namespace Langulus
{

   /// Loop controls from inside ForEach lambdas when iterating containers    
   struct LoopControl {
      enum Command : int {
         Break = 0,     // Break the loop                               
         Continue = 1,  // Continue the loop                            
         Repeat = 2,    // Repeat the current element                   
         Discard = 3,   // Remove the current element                   
         NextLoop = 4   // Skip to next function in the ForEach         
      } mControl;

      LoopControl() = delete;

      constexpr LoopControl(bool a) noexcept
         : mControl {static_cast<Command>(a)} {}
      constexpr LoopControl(Command a) noexcept
         : mControl {a} {}

      explicit constexpr operator bool() const noexcept {
         return mControl == Continue or mControl == Repeat;
      }

      constexpr bool operator == (const LoopControl& rhs) const noexcept {
         return mControl == rhs.mControl;
      }
   };

   namespace Loop
   {

      constexpr LoopControl Break      = LoopControl::Break;
      constexpr LoopControl Continue   = LoopControl::Continue;
      constexpr LoopControl Repeat     = LoopControl::Repeat;
      constexpr LoopControl Discard    = LoopControl::Discard;
      constexpr LoopControl NextLoop   = LoopControl::NextLoop;

   } // namespace Langulus::Loop

} // namespace Langulus
